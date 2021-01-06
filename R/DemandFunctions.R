#' Register functions for preset demand vector
dem_vec_fxn_registry <- list()
dem_vec_fxn_registry["Consumption"] <- "prepareConsumptionDemand"
dem_vec_fxn_registry["Production"] <- "prepareProductionDemand"
dem_vec_fxn_registry["Household"] <- "prepareHouseholdDemand"


#Core production and consumption demand formulas
#y_c <-  Y_h + Y_v + Y_g 
#y_dc <- Y_dh + Y_dv + Y_dg
#y_p <- y_dc + y_e + y_delta


#'Sums across sectors for a given set of BEA codes/cols in a given final demand df
#'
sumDemandCols <- function(Y,codes) {
  if (length(codes)>1) {
    y <- rowSums(Y[,codes])
  } else {
    y <- Y[,codes]
    names(y) <- rownames(Y)
  }
  return(y)
}

sumforConsumption <- function(Y) {
  Y_h <- sumDemandCols(Y,model$BEA$HouseholdDemandCodes)
  Y_v <- sumDemandCols(Y,model$BEA$InvestmentDemandCodes) 
  Y_g <- sumDemandCols(Y,model$BEA$GovernmentDemandCodes) 
  y_c <-  Y_h + Y_v + Y_g 
  return (y_c) 
}

prepareProductionDemand <- function(model) {
  y_dc <- sumforConsumption(model$DomesticFinalDemand)
  Y <- model$FinalDemand
  y_e <- sumDemandCols(Y,model$BEA$ExportCodes)
  y_delta <- sumDemandCols(Y,model$BEA$ChangeInventoriesCodes)
  y_p <- y_dc + y_e + y_delta
  return(y_p)
}

prepareConsumptionDemand <- function(model) {
  y_c <- sumforConsumption(model$FinalDemand)
  return(y_c)
}

prepareHouseholdDemand <- function(model) {
  y_h <- sumDemandCols(Y,model$BEA$HouseholdDemandCodes)
  return(y_h)
}
