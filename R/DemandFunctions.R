# Register functions for preset demand vector
dem_vec_fxn_registry <- list()
dem_vec_fxn_registry$Consumption$Complete <- "prepareConsumptionDemand"
dem_vec_fxn_registry$Production$Complete <- "prepareProductionDemand"
dem_vec_fxn_registry$Consumption$Household <- "prepareHouseholdDemand"


#Core production and consumption demand formulas
#y_c <-  Y_h + Y_v + Y_g 
#y_dc <- Y_dh + Y_dv + Y_dg
#y_p <- y_dc + y_e + y_d_delta


#'Sums across sectors for a given set of BEA codes/cols in a given final demand df
#'@param Y, a model Demand df 
#'@param codes, sector code(s) for a subset of Final Demand cols
#'@return a named vector with model sectors and demand amounts
sumDemandCols <- function(Y,codes) {
  if (length(codes)>1) {
    y <- rowSums(Y[,codes])
  } else {
    y <- Y[,codes]
    names(y) <- rownames(Y)
  }
  return(y)
}

#'Sums the demand cols representing final consumption
#'@param Y, a model Demand df 
#'@return a named vector with model sectors and demand amounts
sumforConsumption <- function(Y) {
  household_code <- toupper(apply(cbind(model$BEA$HouseholdDemandCodes, model$specs$PrimaryRegionAcronym),
                                  1, FUN = joinStringswithSlashes))
  investment_code <- toupper(apply(cbind(model$BEA$InvestmentDemandCodes, model$specs$PrimaryRegionAcronym),
                                   1, FUN = joinStringswithSlashes))
  government_code <- toupper(apply(cbind(model$BEA$GovernmentDemandCodes, model$specs$PrimaryRegionAcronym),
                                   1, FUN = joinStringswithSlashes))
  Y_h <- sumDemandCols(Y, household_code)
  Y_v <- sumDemandCols(Y, investment_code) 
  Y_g <- sumDemandCols(Y, government_code) 
  y_c <-  Y_h + Y_v + Y_g 
  return (y_c) 
}


#'Prepares a demand vector representing production
#'@param model, a model
#'@return a named vector with demand
prepareProductionDemand <- function(model) {
  y_dc <- sumforConsumption(model$DomesticFinalDemand)
  export_code <- toupper(apply(cbind(model$BEA$ExportCodes, model$specs$PrimaryRegionAcronym),
                               1, FUN = joinStringswithSlashes))
  y_e <- sumDemandCols(model$FinalDemand, export_code)
  changeinventories_code <- toupper(apply(cbind(model$BEA$ChangeInventoriesCodes, model$specs$PrimaryRegionAcronym),
                                          1, FUN = joinStringswithSlashes))
  y_d_delta <- sumDemandCols(model$DomesticFinalDemand, changeinventories_code)
  y_p <- y_dc + y_e + y_d_delta
  return(y_p)
}

#'Prepares a demand vector representing household consumption
#'@param model, a model
#'@return a named vector with demand
prepareConsumptionDemand <- function(model) {
  y_c <- sumforConsumption(model$FinalDemand)
  return(y_c)
}

#'Prepares a demand vector representing consumption
#'@param model, a model
#'@return a named vector with demand
prepareHouseholdDemand <- function(model) {
  Y <- model$FinalDemand
  household_code <- toupper(apply(cbind(model$BEA$HouseholdDemandCodes, model$specs$PrimaryRegionAcronym),
                                  1, FUN = joinStringswithSlashes))
  y_h <- sumDemandCols(Y, household_code)
  return(y_h)
}

#'A function to validate a user provided demand vector
#' @param dv a user provided demand vector
#' @param L, the L matrix for the given model, used as a reference
#' 
isDemandVectorValid <- function(dv,L){
  #should be a format like this
  # >dv <- c("1111A0"=1,"1111B0"=2,"327100"=30)
  # > dv
  #1111A0 1111B0 327100 
  #1      2     30 
  
  
  #!temp just return true
  return(TRUE)
}

#' Format a named demand vector with partial sectors to have all the rows and ordering needed
#' @param dv, a user provided demand vector. See calculateEEIOModel()
#' @param L, the L matrix for the given model, used as a reference
#' @return a named vector with values for all names in L and ordered like L
formatDemandVector <- function(dv,L) {
  #create a named vector using the first col of L
  d <- c(L[,1])
  #Set all values to 0
  d[0:nrow(L)] <- 0
  #merge in names from dv
  #replace the values from d
  d[match(names(dv),names(d))] <- dv
  return(d)
}

