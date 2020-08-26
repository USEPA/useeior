
#' Calls the Python flowsa package's getFlowBySector method
#' @param method_name The name
#' @return A dataframe for flowsa data in sector by region totals format
getInventoryMethod <- function(subset=subset) {
  lciafmt <- reticulate::import("lciafmt")
  inv_method <- lciafmt$get_method(method_id="FEDEFL Inventory",subset)
  return(inv_method)
}
  