#' Load totals by sector/region and builds satellite tables based on model specs.
#' Supports BEA and NAICS based totals. These totals can be provided as static files
#' or dynamic function calls are supported. NAICS-based totals are aggregated/allocated to BEA sectors
#' as part of the preparation.
#' @param model A model list object with the specs object listed
#' @return Lists of national totals by sector and formatted satellite tables
#' @format A list with lists of totals by sector and formatted satellite tables
#' \describe{
#'  \itemize{
#'    \item totals_by_sector
#'      \itemize{
#'        \item Flowable {Name of the flow}
#'        \item Sector {Code of the sector in the model IO schema}
#'        \item SectorSourceName {Source of the sector categorization, default is NAICS_2012_Code}
#'        \item Context {Full context of the flow, compartment and subcompartment combined}
#'        \item Location {Activity location, at a national, state, or county level}
#'        \item Unit {SI unit acronym. 'kg' for mass flows; 'MJ' for energy flows.}
#'        \item Year {Year of the data}
#'        \item DistributionType {Form of the frequency distribution, if given. Acceptable values are 'NORMAL', 'LOGNORMAL', 'TRIANGULAR', 'UNIFORM'.}
#'        \item FlowAmount {Amount of the flow}
#'        \item Min {The minimum FlowAmount, if provided for the data range.}
#'        \item Max {The maximum FlowAmount, if provided for the data range.}
#'        \item DataReliability {A 1-5 score of data reliability based on reporting values associated with the amount.}
#'        \item TemporalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item GeographicalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item TechnologicalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item DataCollection {A 1-5 score of data collection based on reporting values associated with the amount.}
#'      }
#'    \item coeffs_by_sector
#'      \itemize{
#'        \item Flowable {Name of the flow}
#'        \item CAS
#'        \item Context {Full context of the flow, compartment and subcompartment combined}
#'        \item FlowUUID
#'        \item SectorName {Name of the sector in the model IO schema}
#'        \item Sector {Code of the sector in the model IO schema}
#'        \item Location {Activity location, at a national, state, or county level}
#'        \item FlowAmount {Amount of the flow}
#'        \item Unit {SI unit acronym. 'kg' for mass flows; 'MJ' for energy flows.}
#'        \item DistributionType {Form of the frequency distribution, if given. Acceptable values are 'NORMAL', 'LOGNORMAL', 'TRIANGULAR', 'UNIFORM'.}
#'        \item ExpectedValue
#'        \item Dispersion
#'        \item Min {The minimum FlowAmount, if provided for the data range.}
#'        \item Max {The maximum FlowAmount, if provided for the data range.}
#'        \item DataReliability {A 1-5 score of data reliability based on reporting values associated with the amount.}
#'        \item TemporalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item GeographicalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item TechnologicalCorrelation {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item DataCollection {A 1-5 score of data collection based on reporting values associated with the amount.}
#'        \item Year {Year of the data}
#'        \item MetaTags
#'        \item MetaSources       
#'        \item MetaOther
#'      }
#'  }
#' }
#' @description Only works for static national totals by BEA sector in a set format
loadSatTables <- function(model) {
  sattables <- list()
  sattables$totals_by_sector <- list()
  sattables$coeffs_by_sector <- list()
  
  logging::loginfo("Initializing model satellite tables...")

  for (sat in model$specs$SatelliteTable) {
    logging::loginfo(paste("Adding", tolower(sat$FullName), "satellite tables..."))
    ### Generate totals_by_sector
    # Check if the satellite table uses a static file. If so, proceed.
    # If not, use specified functions in model metadata to load data from dynamic source
    if(!is.null(sat$StaticFile)) {
      # If the file is a URL tested by the first 4 characters of the string = "http", don't wrap in system.file()
      if (substring(sat$StaticFile, 0, 4)=="http") {
        totals_by_sector <- utils::read.table(sat$StaticFile, sep = ",", header = TRUE, stringsAsFactors = FALSE,
                                              fileEncoding = 'UTF-8-BOM')  
      } else {
        totals_by_sector <- utils::read.table(system.file("extdata", sat$StaticFile, package = "useeior"),
                                              sep = ",", header = TRUE, stringsAsFactors = FALSE,
                                              fileEncoding = 'UTF-8-BOM')
      }
    } else {
      func_to_eval <- sat$ScriptFunctionCall
      totalsgenfunction <- as.name(func_to_eval)
      if (sat$ScriptFunctionParameters == "model") {
        params <- model
      } else {
        params <- sat$ScriptFunctionParameters
      }
      totals_by_sector <- do.call(eval(totalsgenfunction), list(params))
    }
    # Check if the orginal data is BEA-based. If so, apply necessary allocation or aggregation.
    # If not, map data from original sector to BEA.
    if (sat$SectorListSource == "BEA") {
      # If BEA years is not the same as model year, must perform allocation
      if (sat$SectorListLevel == "Detail" && sat$SectorListYear == 2007 && model$specs$BaseIOSchema == 2012) {
        totals_by_sector <- mapFlowTotalsbySectorfromBEASchema2007to2012(totals_by_sector)
      }
      # If the orginal data is at Detail level but model is not, apply aggregation
      if (sat$SectorListLevel == "Detail" && model$specs$BaseIOLevel != "Detail") {
        totals_by_sector <- aggregateSatelliteTable(totals_by_sector,
                                                    from_level = sat$SectorListLevel,
                                                    to_level = model$specs$BaseIOLevel,
                                                    model)
      }
    } else if ("NAICS" %in% sat$SectorListSource) {
      totals_by_sector <- mapFlowTotalsbySectorandLocationfromNAICStoBEA(totals_by_sector, sat$DataYears[1], model)
    }

    # Check if disaggregation is needed based on model metadata
    if(!is.null(model$specs$DisaggregationSpecs) & !is.null(sat$StaticFile)){
      totals_by_sector <- disaggregateSatelliteTable(model, totals_by_sector, sat)
    }
    
    # Add in DQ columns and additional contextual scores not provided
    # Only setting TemporalCorrelation for now
    totals_by_sector <- scoreContextualDQ(totals_by_sector) 
    # Check if all DQ columns are present. If not, print error message.
    len_dq_fields <- length(getDQfields(totals_by_sector))
    if(len_dq_fields!=5){
      logging::logerror(paste("Missing 1 or more data quality fields in satellite data.", len_dq_fields, "present"))
    }
    # Convert totals_by_sector to standard satellite table format
    totals_by_sector <- generateStandardSatelliteTable(totals_by_sector)
    
    ### Generate coeffs_by_sector
    coeffs_by_sector <- data.frame()
    for (r in model$specs$ModelRegionAcronyms) {
      sattable_r <- totals_by_sector[totals_by_sector$Location==r, ]
      if (r=="RoUS") {
        IsRoUS <- TRUE
      } else {
        IsRoUS <- FALSE
        # Change Location if model is a state model
        if (model$specs$ModelType=="state") {
          sattable_r[, "Location"] <- paste0("US-", r)
        }
      }
      for (year in sat$DataYears){
      coeffs_by_sector_r <- generateFlowtoDollarCoefficient(totals_by_sector[totals_by_sector$Year==year, ], year,
                                                            model$specs$IOYear, r, IsRoUS = IsRoUS, model)
      coeffs_by_sector <- rbind(coeffs_by_sector, coeffs_by_sector_r)
      }
    }
    # If the satellite table uses a static file, it will use the embedded mapping files to map flows to internal flow names
    if (!is.null(sat$StaticFile)) {
      if (!substring(sat$OriginalFlowSource,1,6) == 'FEDEFL') {
      coeffs_by_sector <- mapListbyName(coeffs_by_sector, sat)
      }
    }
    
    # Add totals_by_sector and coeffs_by_sector to the sattables list
    sattables$totals_by_sector[[sat$Abbreviation]] <- totals_by_sector
    sattables$coeffs_by_sector[[sat$Abbreviation]] <- coeffs_by_sector
  }
  return(sattables)
}

#' Loads data for all satellite tables as lists in model specs
#' @param model A model object with IO data loaded
#' @return A model object with Satellite tables added 
#' @export
loadandbuildSatelliteTables <- function(model) {
  # Generate satellite tables
  model$SatelliteTables <- loadSatTables(model)
  # Check for duplicate flows across satellite tables
  checkDuplicateFlows(model$SatelliteTables$coeffs_by_sector)
  # Combine satellite tables (coeffs_by_sector) into a single df
  StandardizedSatelliteTable <- do.call(rbind, model$SatelliteTables$coeffs_by_sector)
  # Transform satellite tables into a flow x sector matrix
  StandardizedSatelliteTable[, "Flow"] <- apply(StandardizedSatelliteTable[, c("Flowable", "Context", "Unit")],
                                                1, FUN = joinStringswithSlashes)
  StandardizedSatelliteTable[, "Sector"] <- apply(StandardizedSatelliteTable[, c("Sector", "Location")],
                                                  1, FUN = joinStringswithSlashes)
  # Cast StandardizedSatelliteTable into a flow x sector matrix
  sattables_cast <- reshape2::dcast(StandardizedSatelliteTable, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  # Move Flow to rowname so matrix is all numbers
  rownames(sattables_cast) <- sattables_cast$Flow
  sattables_cast$Flow <- NULL
  # Complete sector list according to model$Industries
  standard_columns <- tolower(apply(cbind(model$Industries, model$specs$PrimaryRegionAcronym),
                                    1, FUN = joinStringswithSlashes))
  sattables_cast[, setdiff(standard_columns, colnames(sattables_cast))] <- 0
  # Adjust column order to be the same with V_n rownames
  model$sattables_cast <- sattables_cast[, standard_columns]
  return(model)
}
