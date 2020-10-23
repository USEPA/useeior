#' Build an EEIO form USEEIO model. Requires model object with 
#' loaded IO tables (see loadIOtables), built satellite tables, and built
#' @param model Model file loaded with IO tables and satellite tables built
#' @export
#' @return A list with USEEIO model components and attributes.
buildEEIOModel <- function(model) {
  # Generate matrices
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
  model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse
  model$W <- as.matrix(model$BEA$UseValueAdded)
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    logging::loginfo("Building commodityxcommodity direct requirement matrix ...")
    model$A <- model$U_n %*% model$V_n
    model$A_d <- model$U_d_n %*% model$V_n
  } else if(model$specs$CommoditybyIndustryType == "Industry") {
    logging::loginfo("Building industryxindustry requirement matrix ...")
    model$A <- model$V_n %*% model$U_n
    model$A_d <- model$V_n %*% model$U_d_n
  }
  # Create an imports requirements matrix 
  model$A_m <- model$A - model$A_d

  # Generate B matrix
  model$B <- as.matrix(model$sattables_cast)

  # Transform B into a flow x commodity matrix using market shares matrix for commodity models
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    model$B <- model$B %*% model$V_n
    colnames(model$B) <- tolower(apply(cbind(colnames(model$B), model$specs$PrimaryRegionAcronym),
                                       1, FUN = joinStringswithSlashes))
  }

  # Generate C matrix: LCIA indicators in indicator x flow format
  model$C <- reshape2::dcast(model$indicators, Code ~ Flow, value.var = "Amount")
  rownames(model$C) <- model$C$Code
  # Get flows in B not in C and add to C
  flows_inBnotC <- setdiff(rownames(model$B), colnames(model$C))
  model$C[, flows_inBnotC] <- 0
  model$C[is.na(model$C)] <- 0
  # Filter and resort model C flows and make it into a matrix
  model$C <- as.matrix(model$C[, rownames(model$B)])

  # Add direct impact matrix
  model$D <- model$C %*% model$B 
  
  # Calculate total requirements matrix as Leontief inverse of A (L)
  logging::loginfo("Calculating total requirements matrix...")
  I <- diag(nrow(model$A))
  I_d <- diag(nrow(model$A_d))
  model$L <- solve(I - model$A)
  model$L_d <- solve(I_d - model$A_d)
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating total emissions per dollar matrix...")
  model$M <- model$B %*% model$L
  colnames(model$M) <- tolower(paste(colnames(model$M), model$specs$PrimaryRegionAcronym, sep = "/"))
  # Calculate M_d, the domestic emissions per dollar using domestic Leontief
  model$M_d <- model$B %*% model$L_d
  colnames(model$M_d) <- tolower(paste(colnames(model$M_d), model$specs$PrimaryRegionAcronym, sep = "/"))
  # Calculate total requirements for imports - note different method
  model$L_m <- model$A_m %*% model$L_d
  # Calculate M_e, the external emissions per dollar using the domestic technology assumption
  model$M_e <- model$B %*% model$L_m
  colnames(model$M_e) <- tolower(paste(colnames(model$M_e), model$specs$PrimaryRegionAcronym, sep = "/"))
  # Calculate total impacts per dollar (U), impact category x sector
  model$U <- model$C %*% model$M
  # Calculate U_d, the domestic impacts per dollar
  model$U_d <- model$C %*% model$M_d
  #Calculate U_e, the external impacts per dollar using the domestic technology assumption
  model$U_e <- model$C %*% model$M_e
  
  logging::loginfo("Model build complete.")
  return(model)
}

