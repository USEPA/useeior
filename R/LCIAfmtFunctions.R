
#' Calls the Python LCIAformatter package's get_mapped_method function
#' @param parameters List of parameters, must include 'indicators' which is a list of one or 
#' more indicators to include from inventory method
#' @return An LCIAmethod with the specified indicators
getInventoryMethod <- function(parameters) {
  # Convert the passed indicators to a list, if none provided all indicators are returned
  if(length(parameters$indicators)==1){
    indicators <- list(parameters$indicators)
  }
  else{
    indicators <- parameters$indicators
  }
  lciafmt <- reticulate::import("lciafmt")
  inv_method <- lciafmt$get_mapped_method(method_id="FEDEFL_INV", indicators=indicators)
  return(inv_method)
}

#' Calls the Python LCIAformatter package's get_mapped_method function
#' @param parameters List of parameters, must include method_id
#' @return An LCIAmethod with the specified indicators
getImpactMethod <- function(parameters) {
  method_id <- parameters$method_id #Name of method from LCIAformatter
  
  # Convert the passed indicators to a list, if none provided all indicators are returned
  if(!is.null(parameters$indicators)){ #
    if(length(parameters$indicators)==1){
      indicators <- list(parameters$indicators)
    }
    else{
      indicators <- parameters$indicators
    }}
  else{indicators <- NULL}

  # Convert the passed methods to a list (e.g. "ReCiPe Midpoint/H"), if none provided all methods are returned
  if(!is.null(parameters$methods)){
    if(length(parameters$methods)==1){
      methods <- list(parameters$methods)
    }
    else{
      methods <- parameters$methods
    }}
  else{methods <- NULL}

  lciafmt <- reticulate::import("lciafmt")
  imp_method <- lciafmt$get_mapped_method(method_id=method_id, indicators=indicators, methods=methods)
  return(imp_method)
}

#' Prepares and reformats LCIAmethod data from LCIAformatter for use
#' modeled after prepareFlowBySectorCollapsedforSatellite
#' @param df A full LCIAmethod from LCIAformatter via getInventoryMethod
prepareLCIAmethodforIndicators <- function(lciamethod) {
  
  #replace Python type None with NA
  lciamethod <- replaceNonewithNA(lciamethod)
  
  #remove unused fields
  cols_not_used <- c("Method","Method UUID","Indicator UUID", "Indicator unit","CAS No","Location","Location UUID")
  lciamethodt <- lciamethod[,-which(names(lciamethod) %in% cols_not_used)]  
  
  #map cols to match regional and sector totals format
  factors <- mapLCIAmethodtoIndicators(lciamethodt)
  factors$Subcategory <- NA
  return(factors)
  
}


#' Temp function to map the trimmed LCIAmethod format from the LCIAformatter to 
#' the format for use in useeior. Very closely follows the mapFlowBySectorCollapsedtoRegionalTotals
#' in flowsa functions
#' @param df with the trimmed LCIAmethod output of prepareLCIAmethodforIndicators
mapLCIAmethodtoIndicators <- function(lciamethodt) {
  mapping <- c('Flowable' = 'Name',
               'Characterization Factor' = 'Amount',
               'Context' = 'Category',
               'Unit' = 'Unit',
               'Flow UUID' = 'UUID')
  
  # 2. Replace all pattern, according to the dictionary-values (only a single vector of string, or a single string
  colnames(lciamethodt) <- stringr::str_replace_all(string = colnames(lciamethodt),pattern=mapping)
  return(lciamethodt)
}