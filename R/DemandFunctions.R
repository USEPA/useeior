# Register functions for preset demand vector
dem_vec_fxn_registry <- list()
dem_vec_fxn_registry$Consumption$Complete <- "prepareConsumptionDemand"
dem_vec_fxn_registry$Production$Complete <- "prepareProductionDemand"
dem_vec_fxn_registry$Consumption$Household <- "prepareHouseholdDemand"
dem_vec_fxn_registry$Consumption$Domestic <- "prepareDomesticConsumptionDemand"

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

#'Sums the demand cols representing final consumption, i.e. household, investment, and government
#'@param model, a model
#'@param Y, a model Demand df.
#'@return a named vector with model sectors and demand amounts
sumforConsumption <- function(model, Y) {
  codes <- model$FinalDemandSectors[model$FinalDemandSectors$Name%in%c("Household", "Investment", "Government"),
                                    "Code_Loc"]
  y_c <- sumDemandCols(Y, codes)
  return (y_c) 
}

#'Prepares a demand vector representing production
#'@param model, a model
#'@return a named vector with demand
prepareProductionDemand <- function(model) {
  y_dc <- sumforConsumption(model, model$DomesticFinalDemand)
  export_code <- model$FinalDemandSectors[model$FinalDemandSectors$Name=="Export", "Code_Loc"]
  y_e <- sumDemandCols(model$FinalDemand, export_code)
  changeinventories_code <- model$FinalDemandSectors[model$FinalDemandSectors$Name=="ChangeInventories", "Code_Loc"]
  y_d_delta <- sumDemandCols(model$DomesticFinalDemand, changeinventories_code)
  y_p <- y_dc + y_e + y_d_delta
  return(y_p)
}

#'Prepares a demand vector representing consumption
#'@param model, a model
#'@return a named vector with demand
prepareConsumptionDemand <- function(model) {
  y_c <- sumforConsumption(model, model$FinalDemand)
  return(y_c)
}

#'Prepares a demand vector representing domestic consumption
#'@param model, a model
#'@return a named vector with demand
prepareDomesticConsumptionDemand <- function(model) {
  y_c_d <- sumforConsumption(model, model$DomesticFinalDemand)
  return(y_c_d)
}

#'Prepares a demand vector representing household consumption
#'@param model, a model
#'@return a named vector with demand
prepareHouseholdDemand <- function(model) {
  Y <- model$FinalDemand
  household_code <- model$FinalDemandSectors[model$FinalDemandSectors$Name=="Household", "Code_Loc"]
  y_h <- sumDemandCols(Y, household_code)
  return(y_h)
}

#'A function to validate a user provided demand vector
#' @param dv a user provided demand vector
#' @param L, the L matrix for the given model, used as a reference
#' @return A boolean value indicating demand vector is valid or not.
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