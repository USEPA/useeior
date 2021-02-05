# Functions for handling data from flowsa

#' Calls the Python flowsa package's getFlowBySector method
#' @param method_name The name
#' @return A data frame for flowsa data in sector by region totals format
getFlowbySectorCollapsed <- function(method_name) {
  flowsa <- reticulate::import("flowsa")
  fbsc <- flowsa$getFlowBySector_collapsed(method_name)
  # checks columns that are all None values and converts to NA
  for(i in colnames(fbsc)){
    if(is.list(fbsc[[i]])){
      fbsc[ , i] <- NA
    }
  }
  flows_by_sector_and_region <- prepareFlowBySectorCollapsedforSatellite(fbsc)
  return(flows_by_sector_and_region)
}


#' Adjusts flowbysector data from flowsa
#' Currently only works for national totals (location="00000") and
#' assumes that sector schema is NAICS_2012_Code
#' @param fbsc A FlowBySector collapsed df from flowsa
#' @return A data frame of sector by region totals
prepareFlowBySectorCollapsedforSatellite <- function(fbsc) {
  # Replace Python type None with NA
  fbsc <- replaceNonewithNA(fbsc)
  # If context is NA replace with blank
  fbsc[,"Context"][is.na(fbsc[,"Context"])] <- ""
  # Filter technosphere flows
  acceptable_types <- c("ELEMENTARY_FLOW", "WASTE_FLOW")
  fbsc <- fbsc[fbsc$FlowType %in% acceptable_types, ]
  # Map codes to locations
  fbsc$Location <- mapFIPS5toLocationCodes(fbsc$Location)
  # Remove unused data
  fbsc[, c("Class", "FlowType", "LocationSystem", "MeasureofSpread", "Spread")] <- NULL
  return(fbsc)
}
