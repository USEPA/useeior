
#' Calls the Python LCIAformatter package's get_mapped_method function
#' @param indicators List of one or more indicators to include from inventory method
#' @return An LCIAmethod with the specified indicators
getInventoryMethod <- function(indicators) {
  lciafmt <- reticulate::import("lciafmt")
  inv_method <- lciafmt$get_mapped_method(method_id="FEDEFL_INV", indicators=indicators)
  return(inv_method)
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