# Functions for assembling final demand vectors

#'Registry of functions that construct various demand vector in the form of as a named list with nested names
#'as keys and function name as values
DemandVectorFunctionRegistry <- list()
DemandVectorFunctionRegistry$Consumption$Complete <- "prepareConsumptionDemand"
DemandVectorFunctionRegistry$Production$Complete <- "prepareProductionDemand"
DemandVectorFunctionRegistry$Consumption$Household <- "prepareHouseholdDemand"
DemandVectorFunctionRegistry$Consumption$Domestic <- "prepareDomesticConsumptionDemand"

#'Sums across sectors for a given set of codes/cols in a given final demand df
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
#'Complete national consumption formula: y_c <-  Y_h + Y_v + Y_g 
#'Domestic portion of national consumption: y_dc <- Y_dh + Y_dv + Y_dg
#'@param model, a model
#'@param Y, a model Demand df.
#'@return a named vector with model sectors and demand amounts
sumforConsumption <- function(model, Y) {
  codes <- model$FinalDemandMeta[model$FinalDemandMeta$Group%in%c("Household", "Investment", "Government"),
                                    "Code_Loc"]
  y_c <- sumDemandCols(Y, codes)
  return (y_c) 
}

#'Prepares a demand vector representing production
#'Formula for production vector: y_p <- y_dc + y_e + y_d_delta
#'@param model, a model
#'@return a named vector with demand
prepareProductionDemand <- function(model) {
  y_dc <- sumforConsumption(model, model$DomesticFinalDemand)
  export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export", "Code_Loc"]
  y_e <- sumDemandCols(model$FinalDemand, export_code)
  changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories", "Code_Loc"]
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
  household_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Household", "Code_Loc"]
  y_h <- sumDemandCols(Y, household_code)
  return(y_h)
}

#'A function to validate a user provided demand vector
#' @param dv a user provided demand vector
#' @param L, the L matrix for the given model, used as a reference
#' @return A logical value indicating demand vector is valid or not.
isDemandVectorValid <- function(dv, L){
  # dv should be a named numeric vector
  # names of dv should be part of sectors in L
  is_valid <- all(is.numeric(dv), all(names(dv)%in%rownames(L)))
  return(is_valid)
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
