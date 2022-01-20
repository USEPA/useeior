# Functions handling data from the LCIAformatter, https://github.com/USEPA/LCIAformatter

#' Get impact method in the format of the Python LCIAformatter package's get_mapped_method function.
#' @param ind_spec Specification of an indicator
#' @return An LCIAmethod with the specified indicators
getImpactMethod <- function(ind_spec) {
  parameters <- ind_spec[["ScriptFunctionParameters"]]

  # Convert the passed indicators to a list, if none provided all indicators are returned
  if(!is.null(parameters$indicators)){
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
  
  if(!ind_spec$StaticSource){
    # Generate impact method in lciafmt, requires method_id to be included in ScriptFunctionParameters
    if(is.null(parameters$method_id)){
      logging::logwarn("method_id must be passed in ScriptFunctionParameters to access LCIAfmt")
    }
    lciafmt <- reticulate::import("lciafmt")
    imp_method <- lciafmt$get_mapped_method(method_id = parameters$method_id,
                                            indicators = indicators, methods = methods)
  } else {
    f <- loadDataCommonsfile(ind_spec$StaticFile)
    imp_method <- as.data.frame(arrow::read_parquet(f))
  }
  
  # Subset the method by method
  if(!is.null(methods)){  
    imp_method <- imp_method[imp_method$Method %in% methods, ]
  }

  # Subset the method by indicator
  if(!is.null(indicators)){
    imp_method <- imp_method[imp_method$Indicator %in% indicators, ]
  }

  return(imp_method)
}

#' Get and combine impact methods using the Python LCIAformatter package's get_mapped_method function.
#' @param ind_spec Specification of an indicator
#' @return An LCIAmethod with the specified indicators
getCombinedImpactMethods <- function(ind_spec) {
  
  imp_method = getImpactMethod(ind_spec)
  
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
