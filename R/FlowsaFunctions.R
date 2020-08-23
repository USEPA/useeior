# Functions for handling data from flowsa

#' Calls the Python flowsa package's getFlowBySector method
#' @param method_name The name
#' @return A dataframe for flowsa data in sector by region totals format
getFlowbySectorCollapsed <- function(method_name) {
  flowsa <- reticulate::import("flowsa")
  fbsc <- flowsa$getFlowBySector_collapsed(method_name)
  flows_by_sector_and_region <- prepareFlowBySectorCollapsedforSatellite(fbsc)
  return(flows_by_sector_and_region)
}


#' Adjusts flowbysector data from flowsa
#' Currently only works for national totals (location='00000') and
#' assumes that sector schema is NAICS_2012_Code
#' @param fbsc A FlowBySector collapsed df from flowsa
#' @return A df of sector by region totals
prepareFlowBySectorCollapsedforSatellite <- function(fbsc) {
  
  #replace Python type None with NA
  fbsc <- replaceNonewithNA(fbsc)
  
  #filter technosphere flows
  acceptable_types <- c('ELEMENTARY_FLOW','WASTE_FLOW')
  fbsc <- fbsc[fbsc$FlowType %in% acceptable_types, ]
  
  #map codes to locations
  fbsc$Location <- mapFIPS5toLocationCodes(fbsc$Location)
  
  #remove unused data
  fbsc_cols_not_used <- c('Class','FlowType','LocationSystem','MeasureofSpread','Spread')
  fbsc_trimmed <- fbsc[,-which(names(fbsc) %in% fbsc_cols_not_used)]  
  
  #map cols to match regional and sector totals format
  totals_by_sector_and_loc <- mapFlowBySectorCollapsedtoRegionalTotals(fbsc_trimmed)
  return(totals_by_sector_and_loc)
}

#' Maps flows by sector collapsed format to totals by sector and region format
#' @param fbsct A df in the flowbysector collapsed format with additional columns removed
#' @return A df of sector by region totals
mapFlowBySectorCollapsedtoRegionalTotals <- function(fbsct) {
  
  mapping <- c('Sector' = 'SectorCode',
               'Flowable' = 'FlowName',
               'Year' = 'Year',
               'FlowAmount' = 'FlowAmount',
               'Context' = 'Compartment',
               'Unit' = 'Unit',
               'Location' = 'Location',
               'DataReliability' = 'ReliabilityScore',
               'TemporalCorrelation' = 'TemporalCorrelation',
               'GeographicCorrelation' = 'GeographicCorrelation',
               'TechnologicalCorrelation' = 'TechnologicalCorrelation',
               'DataCollection' = 'DataCollection')
  
  # 2. Replace all pattern, according to the dictionary-values (only a single vector of string, or a single string
  colnames(fbsct) <- stringr::str_replace_all(string = colnames(fbsct),pattern= mapping)  # we only use the 'pattern'
  return(fbsct)
  
}
