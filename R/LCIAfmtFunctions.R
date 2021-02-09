#' Get impact method in the format of the Python LCIAformatter package's get_mapped_method function.
#' @param parameters List of parameters, must include 'filename', 'methods' and 'indicators' are optional
#' which are a list of one or more indicators to include from the specified method.
#' @return An LCIAmethod with the specified indicators
getImpactMethod <- function(parameters) {
  
  directory <- paste0(rappdirs::user_data_dir(), "\\lciafmt")
  debug_url <- "https://edap-ord-data-commons.s3.amazonaws.com/index.html?prefix=lciafmt/"

  # file must be saved in the local directory
  f <- paste0(directory,'\\', parameters$filename)

  if(!file.exists(f)){
    logging::loginfo(paste0("parquet not found, downloading from ", debug_url))
    downloadfiles(parameters$filename, 'lciafmt')
    }

  imp_method <- as.data.frame(arrow::read_parquet(f))
  
  # Subset the method by method
  # Convert the passed methods to a list (e.g. "ReCiPe Midpoint/H"), if none provided all methods are returned
  if(!is.null(parameters$methods)){
    if(length(parameters$methods)==1){
      methods <- list(parameters$methods)
    } else {
      methods <- parameters$methods
    }
    imp_method <- imp_method[imp_method$Method %in% methods, ]
  }

  # Subset the method by indicator
  # Convert the passed indicators to a list, if none provided all indicators are returned
  if(!is.null(parameters$indicators)){ #
    if(length(parameters$indicators)==1){
      indicators <- list(parameters$indicators)
    } else {
      indicators <- parameters$indicators
    }
    imp_method <- imp_method[imp_method$Indicator %in% indicators, ]
    
  }

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
