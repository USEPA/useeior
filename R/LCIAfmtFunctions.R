
#' Calls the Python flowsa package's getFlowBySector method
#' @param method_name The name
#' @return A dataframe for flowsa data in sector by region totals format
getInventoryMethod <- function(subset) {
  lciafmt <- reticulate::import("lciafmt")
  inv_method <- lciafmt$get_method(method_id="FEDEFL Inventory")
  inv_method <- inv_method[inv_method['Indicator']==subset,]
  return(inv_method)
}
  