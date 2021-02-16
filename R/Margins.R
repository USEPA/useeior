#' Prepare M and U matrices with sector margin impacts
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param margin_type A character value: can be "intermediate" or "final consumer".
#' @export
#' @return A list with M_margin and U_margin
deriveMarginSectorImpacts <- function(model, margin_type = "intermediate") {
  # Determine Margins table
  if (margin_type=="intermediate") {
    Margins <- model$IntermediateMargins
  } else {
    Margins <- model$FinalConsumerMargins
  }
  ## Add in impacts of margin sectors
  # Calculate fractions (in absolute value) of producer price for each margin
  MarginCoefficients <- abs(as.matrix(Margins[, c("Transportation", "Wholesale", "Retail")]/Margins[, c("ProducersValue")]))
  rownames(MarginCoefficients) <- Margins$SectorCode
  
  # Allocate coefficients to margin sectors using margin allocation matrix
  all_margin_sectors <- c(model$BEA$TransportationCodes, model$BEA$WholesaleCodes, model$BEA$RetailCodes)
  margin_allocation <- buildMarginAllocationMatrix(all_margin_sectors, model)
  margins_by_sector <- MarginCoefficients %*% margin_allocation
  
  # Put margins_by_sector into a matrix in the form of A
  A_margin <- model$A
  # Make sure sector ordering is the same
  A_margin[,] <- 0 
  for (s in all_margin_sectors) {
    A_margin[s, ] <- margins_by_sector[, s]
  }
  # Multiply M and U by margins_by_sector to derive M_margin and U_margin
  model$M_margin <- model$M %*% A_margin
  colnames(model$M_margin) <- tolower(paste(colnames(model$M_margin), model$specs$PrimaryRegionAcronym, sep = "/"))
  model$U_margin <- model$U %*% A_margin
  colnames(model$U_margin) <- tolower(paste(colnames(model$U_margin), model$specs$PrimaryRegionAcronym, sep = "/"))
  logging::loginfo("Model margin impacts derived")
  return(model)
}

#'Create margin_allocation matrix to allocate fractions by margin sector
#'Currently uses sector output to provide that allocation
#'@param all_margin_sectors, vector of sector codes
#'@param model, a fully built model
#'@return matrix, margin types x margin sectors with values being fractions of type to each sector
buildMarginAllocationMatrix <- function(all_margin_sectors, model) {
  
  margin_allocation <- matrix(nrow = 3, ncol = length(all_margin_sectors), 0)
  rownames(margin_allocation) <- colnames(MarginCoefficients)
  colnames(margin_allocation) <- all_margin_sectors
  # Assign allocation factors to margin sectors based on total Commodity output
  output_ratio <- calculateOutputRatio(model, output_type="Commodity")
  margin_allocation["Transportation", model$BEA$TransportationCodes] <- output_ratio[output_ratio$SectorCode%in%model$BEA$TransportationCodes, "toSectorRatio"]
  margin_allocation["Wholesale", model$BEA$WholesaleCodes] <- output_ratio[output_ratio$SectorCode%in%model$BEA$WholesaleCodes, "toSectorRatio"]
  margin_allocation["Retail", model$BEA$RetailCodes] <- output_ratio[output_ratio$SectorCode%in%model$BEA$RetailCodes, "toSectorRatio"]
  return(margin_allocation)
  
}


 
