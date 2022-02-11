#' Load totals by sector/region and prepares them based on model specs.
#' @param model A model list object with model specs and IO tables listed
#' @return Lists of totals by sector by region and unique flows
#' @format A list with lists of totals by sector
#' \describe{
#'  \itemize{
#'    \item totals_by_sector
#'      \itemize{
#'        \item Flowable {Name of the flow}
#'        \item Context {Full context of the flow, compartment and subcompartment combined}
#'        \item Unit {SI unit acronym. 'kg' for mass flows; 'MJ' for energy flows.}
#'        \item FlowUUID {unique hex code for flow}
#'        \item SectorName {Name of the sector}
#'        \item Sector {Code of the sector in the model IO schema}
#'        \item FlowAmount {Amount of the flow}
#'        \item Location {Activity location, at a national, state, or county level}
#'        \item Year {Year of the data}
#'        \item DistributionType {Form of the frequency distribution, if given. Acceptable values are 'NORMAL', 'LOGNORMAL', 'TRIANGULAR', 'UNIFORM'.}
#'        \item Min {The minimum FlowAmount, if provided for the data range.}
#'        \item Max {The maximum FlowAmount, if provided for the data range.}
#'        \item DataReliability {A 1-5 score of data reliability based on reporting values associated with the amount.}
#'        \item TemporalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item GeographicalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item TechnologicalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item DataCollection {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item MetaSources {Tag for the data source.}
#'    }
#'  }
#' }
#' @description Supports BEA and NAICS based totals. These totals can be provided as static files
#' or dynamic function calls are supported. NAICS-based totals are aggregated/allocated to BEA sectors
#' as part of the preparation.
loadSatTables <- function(model) {
  sattables <- list()
  sattables$totals_by_sector <- list()
  flows <- list()
  logging::loginfo("Initializing model satellite tables...")

  #Loop through each sat specification
  for (sat_spec in model$specs$SatelliteTable) {
    if(sat_spec$FileLocation == 'None'){
      logging::loginfo(paste0("Generating ", sat_spec$FullName, " flows..."))      
    } else {
      logging::loginfo(paste0("Loading ", sat_spec$FullName, " flows from ", sat_spec$FileLocation, "..."))
    }

    ### Generate totals_by_sector, tbs
    tbs0 <- generateTbSfromSatSpec(sat_spec, model)
    
    # Convert totals_by_sector to standard satellite table format
    tbs <- conformTbStoStandardSatTable(tbs0)
    
    ### Make tbs conform to the model schema
    tbs <- conformTbStoIOSchema(tbs, sat_spec, model)
    
    ##Check for any loss of flow data
    checkSatelliteFlowLoss(tbs0,tbs)
    tbs <- removeMissingSectors(tbs)
    
    # Add in DQ columns and additional contextual scores not provided
    # Only setting TemporalCorrelation for now
    tbs <- scoreContextualDQ(tbs)
    
    # Convert totals_by_sector to standard satellite table format
    tbs <- conformTbStoStandardSatTable(tbs)
    
    #Map names for files not already using FEDEFL
    if (!substring(sat_spec$OriginalFlowSource,1,6) == 'FEDEFL') {
      tbs <- mapListbyName(tbs, sat_spec)
    }
    flow_fields <- c("Flowable","Context","Unit","FlowUUID")
    flows_tbs <- unique(tbs[,flow_fields])
    flows <- rbind(flows,flows_tbs)
    # Add totals_by_sector to the sattables list
    sattables$totals_by_sector[[sat_spec$Abbreviation]] <- tbs
  }
  # Check for duplicate flows across satellite tables
  checkDuplicateFlowsBySector(sattables$totals_by_sector)
  
  flows <- flows[!duplicated(flows[,flow_fields[flow_fields != "FlowUUID"]]),]
  #Re-index the flows
  rownames(flows) <- NULL
  sattables$flows <- flows
  return(sattables)
}

#' Loads data for all satellite tables as lists in model specs
#' @param model A model list object with model specs and IO tables listed
#' @return A model object with Satellite tables added
loadandbuildSatelliteTables <- function(model) {
  # Generate satellite tables
  model$SatelliteTables <- loadSatTables(model)
  return(model)
}

#'Reads a satellite table specification and generates a totals-by-sector table
#'@param sat_spec, a standard specification for a single satellite table
#' @param model A model list object with model specs and IO tables listed
#'@return a totals-by-sector dataframe
generateTbSfromSatSpec <- function(sat_spec, model) {
  # Check if the satellite table uses a file from within useeior. If so, proceed.
  # If not, use specified functions in model metadata to load data from dynamic source
  if(sat_spec$FileLocation == "useeior") {
    totals_by_sector <- utils::read.table(system.file("extdata", sat_spec$StaticFile, package = "useeior"),
                                          sep = ",", header = TRUE, stringsAsFactors = FALSE,
                                          fileEncoding = 'UTF-8-BOM')
  } else if (!is.null(sat_spec$ScriptFunctionCall)) {
    func_to_eval <- sat_spec$ScriptFunctionCall
    totalsgenfunction <- as.name(func_to_eval)
    params <- sat_spec
    if (!is.null(sat_spec$ScriptFunctionParameters)) {
      if (sat_spec$ScriptFunctionParameters == "model") {
        params <- model
      }
    }
    totals_by_sector <- do.call(eval(totalsgenfunction), list(params))
  } else {
    f <- loadDataCommonsfile(sat_spec$StaticFile)
    totals_by_sector <- utils::read.table(f, sep = ",", header = TRUE, stringsAsFactors = FALSE,
                                          fileEncoding = 'UTF-8-BOM')
  }
  return(totals_by_sector)
}

#'Take a totals-by-sector df and maps flows to the model schema
#'@param tbs, totals-by-sector df
#'@param sat_spec, a standard specification for a single satellite table
#'@param model an EEIO model with IO tables loaded
#'@return a totals-by-sector df with the sectors and flow amounts corresponding to the model schema
conformTbStoIOSchema <- function(tbs, sat_spec, model) {
  # Check if aggregation or disaggregation are needed based on model metadata
  if(!is.null(sat_spec$StaticFile)) {
    for(aggSpecs in model$AggregationSpecs) {
      tbs <- aggregateSectorsinTBS(model, aggSpecs, tbs, sat_spec)  
    }
    for (disagg in model$DisaggregationSpecs) {
      tbs <- disaggregateSatelliteTable(disagg, tbs, sat_spec)
    }
  }
  # Change Location if model is a state model
  if (all(model$specs$ModelRegionAcronyms!="US", model$specs$IODataSource=="stateior")) {
    # Format location in tbs
    tbs$Location <- formatLocationforStateModels(tbs$Location)
  }
  tbs$Location <- ifelse(tbs$Location%in%model$specs$ModelRegionAcronyms,
                         tbs$Location,
                         setdiff(model$specs$ModelRegionAcronyms, tbs$Location))
  
  # Check if the original data is BEA-based. If so, apply necessary allocation or aggregation.
  # If not, map data from original sector to BEA.
  if (sat_spec$SectorListSource == "BEA") {
    # If BEA years is not the same as model year, must perform allocation
    if (all(sat_spec$SectorListLevel == "Detail", sat_spec$SectorListYear == 2007, model$specs$BaseIOSchema == 2012)) {
      tbs <- mapFlowTotalsbySectorfromBEASchema2007to2012(tbs)
    }
    # If the original data is at Detail level but model is not, apply aggregation
    if (sat_spec$SectorListLevel == "Detail" && model$specs$BaseIOLevel != "Detail") {
      tbs <- aggregateSatelliteTable(tbs,from_level = sat_spec$SectorListLevel,model)
    }
  } else if ("NAICS" %in% sat_spec$SectorListSource) {
    tbs <- mapFlowTotalsbySectorandLocationfromNAICStoBEA(tbs, sat_spec$DataYears[1], model)
  }  
  return(tbs)
}
