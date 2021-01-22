#' Get inventory method using the Python LCIAformatter package's get_mapped_method function.
#' @param parameters List of parameters, must include 'indicators' which is a list of one or 
#' more indicators to include from inventory method.
#' @return An LCIAmethod data frame with the specified indicators.
getInventoryMethod <- function(parameters) {
  # Convert the passed indicators to a list, if none provided all indicators are returned
  if(length(parameters$indicators)==1){
    indicators <- list(parameters$indicators)
  } else {
    indicators <- parameters$indicators
  }
  # Generate inventory method table
  lciafmt <- reticulate::import("lciafmt")
  inv_method <- lciafmt$get_mapped_method(method_id = "FEDEFL_INV", indicators = indicators)
  return(inv_method)
}

#' Get impact method using the Python LCIAformatter package's get_mapped_method function.
#' @param parameters List of parameters, must include 'method_id' and 'indicators'
#' which is a list of one or more indicators to include from inventory method.
#' @return An LCIAmethod with the specified indicators
getImpactMethod <- function(parameters) {
  # Convert the passed indicators to a list, if none provided all indicators are returned
  if(!is.null(parameters$indicators)){ #
    if(length(parameters$indicators)==1){
      indicators <- list(parameters$indicators)
    } else {
      indicators <- parameters$indicators
    }
  } else {
    indicators <- NULL
  }
  # Convert the passed methods to a list (e.g. "ReCiPe Midpoint/H"), if none provided all methods are returned
  if(!is.null(parameters$methods)){
    if(length(parameters$methods)==1){
      methods <- list(parameters$methods)
    } else {
      methods <- parameters$methods
    }
  } else {
    methods <- NULL
  }
  # Generate impact method table
  lciafmt <- reticulate::import("lciafmt")
  imp_method <- lciafmt$get_mapped_method(method_id = parameters$method_id,
                                          indicators = indicators, methods = methods)
  return(imp_method)
}

#' Get and combine impact methods using the Python LCIAformatter package's get_mapped_method function.
#' @param parameters List of parameters, must include 'method_id' and list of two or more 'indicators'
#' to include from inventory method. Characterization factors for these two indicators are summed by flow.
#' @return An LCIAmethod with the specified indicators
getCombinedImpactMethods <- function(parameters) {
  
  imp_method = getImpactMethod(parameters)
  
  combined_imp_method <- dplyr::group_by(imp_method, Method,Flowable,`Flow UUID`,Context,Unit)
  combined_imp_method <- dplyr::summarize(
    combined_imp_method,
    CF_agg = sum(`Characterization Factor`),
    .groups = 'drop')
  colnames(combined_imp_method)[colnames(combined_imp_method)=="CF_agg"] <- "Characterization Factor"
  # Indicator name will be assigned from satellite spec
  combined_imp_method[,"Indicator"] <- NA
  combined_imp_method <- as.data.frame(combined_imp_method)
  
  return(combined_imp_method)
}

#' Prepares and reformats LCIAmethod data from LCIAformatter for use
#' modeled after prepareFlowBySectorCollapsedforSatellite
#' @param lciamethod A full LCIAmethod data frame from LCIAformatter via getInventoryMethod or getImpactMethod.
#' @return A LCIAmethod data frame formatted for indicators
prepareLCIAmethodforIndicators <- function(lciamethod) {
  # Replace Python type None with NA
  lciamethod <- replaceNonewithNA(lciamethod)
  # Create UUID and Amount columns
  lciamethod[, "UUID"] <- lciamethod[, "Flow UUID"]
  lciamethod[, "Amount"] <- lciamethod[, "Characterization Factor"]
  # Keep useful fields
  lciamethod <- lciamethod[, c("Indicator", "Flowable", "UUID", "Context", "Unit", "Amount")]
  return(lciamethod)

}
