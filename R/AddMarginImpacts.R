#' Prepare M and U matrices with sector margin impacts
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param margin_type A character value: can be "intermediate" or "final consumer".
#' @export
#' @return A list with M_m and U_m
deriveMarginSectorImpacts <- function(model, margin_type = "intermediate") {
  # Determine Margins table
  if (margin_type=="intermediate") {
    Margins <- model$IntermediateMargins
  } else {
    Margins <- model$FinalConsumerMargins
  }
  ## Add in impacts of margin sectors
  # Calculation fractions of producer price for each margin
  Phi <- as.matrix(Margins[, c("Transportation", "Wholesale", "Retail")]/Margins[, c("ProducersValue")])
  rownames(Phi) <- Margins$SectorCode
  
  # Create margin_allocation matrix, MA, to allocate fractions by margin sector
  # In the matrix, rows are three margin types and columns are margin sectors
  all_margin_sectors <- c(model$BEA$TransportationCodes, model$BEA$WholesaleCodes, model$BEA$RetailCodes)
  MA <- matrix(nrow = 3, ncol = length(all_margin_sectors), 0)
  rownames(MA) <- colnames(Phi)
  colnames(MA) <- all_margin_sectors
  # Assign allocation factors to margin sectors based on total Commodity output
  output_ratio <- calculateOutputRatio(model, output_type="Commodity")
  MA["Transportation", model$BEA$TransportationCodes] <- output_ratio[output_ratio$SectorCode%in%model$BEA$TransportationCodes, "toSectorRatio"]
  MA["Wholesale", model$BEA$WholesaleCodes] <- output_ratio[output_ratio$SectorCode%in%model$BEA$WholesaleCodes, "toSectorRatio"]
  MA["Retail", model$BEA$RetailCodes] <- output_ratio[output_ratio$SectorCode%in%model$BEA$RetailCodes, "toSectorRatio"]
  
  # Multiply fractions by allocation matrix to get a fraction per margin sector for each commodity
  Phi_m <- Phi %*% MA
  
  # Put Phi_m into a matrix in the form of A
  A_m <- model$A
  # Make sure sector ordering is the same
  A_m[,] <- 0 
  for (s in all_margin_sectors) {
    A_m[s, ] <- Phi_m[, s]
  }
  # Multiply M and U by Phi_m to derive M_m and U_m
  model$M_m <- model$M %*% A_m
  colnames(model$M_m) <- tolower(paste(colnames(model$M_m), model$specs$PrimaryRegionAcronym, sep = "/"))
  model$U_m <- model$U %*% A_m
  colnames(model$U_m) <- tolower(paste(colnames(model$U_m), model$specs$PrimaryRegionAcronym, sep = "/"))
  logging::loginfo("Model margin impacts derived")
  return(model)
}

