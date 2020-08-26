
#' Calls the Python flowsa package's getFlowBySector method
#' @param method_name The name
#' @return A dataframe for flowsa data in sector by region totals format
getInventoryMethod <- function(subset) {
  lciafmt <- reticulate::import("lciafmt")
  inv_method <- lciafmt$get_method(method_id="FEDEFL Inventory")
  inv_method <- inv_method[inv_method['Indicator']==subset,]
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