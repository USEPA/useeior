# Functions for assembling final demand vectors

#'Registry of functions that construct various demand vector in the form of as a named list with nested names
#'as keys and function name as values
DemandVectorFunctionRegistry <- list()
# Production
DemandVectorFunctionRegistry$Production$Complete <- "prepareProductionDemand"
DemandVectorFunctionRegistry$Production$Domestic <- "prepareDomesticProductionDemand"
# Consumption
DemandVectorFunctionRegistry$Consumption$Complete <- "prepareConsumptionDemand"
DemandVectorFunctionRegistry$Consumption$Domestic <- "prepareDomesticConsumptionDemand"
DemandVectorFunctionRegistry$Consumption$Household <- "prepareHouseholdDemand"

#' Sums across sectors for a given set of codes/cols in a given final demand df
#' @param Y, a model Demand df 
#' @param codes, sector code(s) for a subset of Final Demand cols
#' @return A named vector with model sectors and demand amounts
sumDemandCols <- function(Y,codes) {
  if (length(codes)>1) {
    y <- rowSums(Y[,codes])
  } else {
    y <- Y[,codes]
    names(y) <- rownames(Y)
  }
  return(y)
}

#' Sums the demand cols representing final consumption, i.e. household, investment, and government
#' Complete national consumption formula: y_c <-  Y_h + Y_v + Y_g 
#' Domestic portion of national consumption: y_dc <- Y_dh + Y_dv + Y_dg
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param Y, a model Demand df.
#' @param location, str of location code for demand vector
#' @return A named vector with model sectors and demand amounts
sumforConsumption <- function(model, Y, location) {
  codes <- model$FinalDemandMeta[model$FinalDemandMeta$Group%in%c("Household", "Investment", "Government") &
                                 grepl(location, model$FinalDemandMeta$Code_Loc),
                                 "Code_Loc"]
  y_c <- sumDemandCols(Y, codes)
  return (y_c) 
}

#' Prepares a demand vector representing production
#' Formula for production vector: y_p <- y_c + y_e + y_m + y_delta
#' where y_c = consumption, y_e = exports, y_m = imports, y_delta = change in inventories
#' y_m values are generally negative in the BEA data and thus are added (whereas when positive they are subtracted)
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareProductionDemand <- function(model, location) {

  if(model$specs$IODataSource=="BEA"){
    loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
    export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export" & loc, "Code_Loc"]
    changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories" & loc, "Code_Loc"]
    import_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Import" & loc, "Code_Loc"]
    y_c <- sumforConsumption(model, model$FinalDemand, location)
    y_e <- sumDemandCols(model$FinalDemand, export_code)
    y_m <- sumDemandCols(model$FinalDemand, import_code)
    y_delta <- sumDemandCols(model$FinalDemand, changeinventories_code)
    y_p <- y_c + y_e + y_m + y_delta
    
  }else if(model$specs$IODataSource == "stateior"){
    y_p <- prepare2RProductionDemand(model, location)
  }

  return(y_p)
  
}

#' #' Prepares a production demand vector representing production for two region models
#' #' Demand for SoI = SoI2SoI + RoUS2SoI
#' #' Demand for RoUS = SoI2RoUS + RoUS2RoUS
#' #' @param model An EEIO model object with model specs and IO tables loaded
#' #' @param location, str of location code for demand vector
#' #' @return A named vector with demand
#' prepare2RProductionDemand <- function(model, location) {
#' temp <-1
#' 
#'   # Get state abbreviations, e.g., "US-ME" and "RoUS"
#'   state_abb <- sub(".*/","",model$FinalDemandMeta$Code_Loc) ## Extract characters after /
#'   state_abb <- unique(state_abb)
#'   
#'   loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
#'   iolevel <- model$specs$BaseIOLevel
#'   FD_columns  <- getFinalDemandCodes("Summary")
#'   ita_column <- ifelse(iolevel == "Detail", "F05100", "F051")
#'   
#'   if(location == state_abb[1]){# calculate production final demand for SoI
#'     SoI2SoI_y   <- rowSums(model$DomesticUseTransactionswithTrade[["SoI2SoI"]][, c(FD_columns, ita_column, "ExportResidual")])
#'     RoUS2SoI_y  <- rowSums(model$DomesticUseTransactionswithTrade[["RoUS2SoI"]][, c(FD_columns, ita_column)])
#'     y_p <- c(SoI2SoI_y,RoUS2SoI_y)
#' 
#'   }else if(location == state_abb[2]){# calculate production final demand for RoUS
#'     SoI2RoUS_y  <- rowSums(model$DomesticUseTransactionswithTrade[["SoI2RoUS"]][, c(FD_columns, ita_column)])
#'     RoUS2RoUS_y <- rowSums(model$DomesticUseTransactionswithTrade[["RoUS2RoUS"]][, c(FD_columns, ita_column, "ExportResidual")])
#'     y_p <- c(SoI2RoUS_y, RoUS2RoUS_y)
#'   }
#'   
#'   names(y_p) <- model$Commodities$Code_Loc
#'   # loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
#'   # export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export" & loc, "Code_Loc"]
#'   # changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories" & loc, "Code_Loc"]
#'   # import_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Import" & loc, "Code_Loc"]
#'   # y_c <- sumforConsumption(model, model$FinalDemand, location)
#'   # y_e <- sumDemandCols(model$FinalDemand, export_code)
#'   # y_m <- sumDemandCols(model$FinalDemand, import_code)
#'   # y_delta <- sumDemandCols(model$FinalDemand, changeinventories_code)
#'   # y_p <- y_c + y_e + y_m + y_delta
#'   
#' 
#'   return(y_p)
#' }

#TODO: 
#1) Modify prepareConsumptionDemand() and prepareDomesticConsumptionDemand() functions to handle 2R models.
#2) MODIFY prepareDomesticProductionDemand() function to handle 2R models similar to how prepareProductionDemand function was modified.


#' Prepares a demand vector representing domestic production
#' Formula for production vector: y_p <- y_dc + y_e + y_d_delta + mu
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareDomesticProductionDemand <- function(model, location) {
  loc <- grepl(location, model$FinalDemandMeta$Code_Loc)
  export_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Export" & loc, "Code_Loc"]
  changeinventories_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="ChangeInventories" & loc, "Code_Loc"]
  y_d_c <- sumforConsumption(model, model$DomesticFinalDemand, location)
  y_d_e <- sumDemandCols(model$DomesticFinalDemand, export_code)
  y_d_delta <- sumDemandCols(model$DomesticFinalDemand, changeinventories_code)
  mu <- model$InternationalTradeAdjustment
  y_d_p <- y_d_c + y_d_e + y_d_delta + mu
  return(y_d_p)
}

#' Prepares a demand vector representing consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return a named vector with demand
prepareConsumptionDemand <- function(model, location) {
  y_c <- sumforConsumption(model, model$FinalDemand, location)
  return(y_c)
}

#' Prepares a demand vector representing domestic consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareDomesticConsumptionDemand <- function(model, location) {
  y_c_d <- sumforConsumption(model, model$DomesticFinalDemand, location)
  return(y_c_d)
}

#' Prepares a demand vector representing household consumption
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param location, str of location code for demand vector
#' @return A named vector with demand
prepareHouseholdDemand <- function(model, location) {
  Y <- model$FinalDemand
  household_code <- model$FinalDemandMeta[model$FinalDemandMeta$Group=="Household" &
                                            grepl(location, model$FinalDemandMeta$Code_Loc), "Code_Loc"]
  y_h <- sumDemandCols(Y, household_code)
  return(y_h)
}

#' A function to validate a user provided demand vector
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
#' @return A named vector with values for all names in L and ordered like L
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

#' Read demand vector from a csv file and format for use in model calculations
#' @param file_path str, path to csv file containing demand data
#' @param demand_name str, name of demand data as field header
#' @param model, a model
#' @return a demand vector formatted for use in calculating model results
#' @export
extractAndFormatDemandVector <- function(file_path, demand_name, model){
  demand_df <- read.csv(file_path, stringsAsFactors = FALSE)
  row.names(demand_df) <- demand_df$Code
  demand_df$Code <- NULL
  y <- na.omit(demand_df[,demand_name])
  names(y) <- row.names(demand_df)
  if(isDemandVectorValid(y, model$L)) {
    y <- formatDemandVector(y, model$L)
  } else {
    stop("Format of the demand vector is invalid.")
  }
  return(y)
}
