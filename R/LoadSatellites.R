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

  #Loop through each sat specification
  for (sat_spec in model$specs$SatelliteTable) {
    logging::loginfo(paste("Loading", tolower(sat_spec$FullName), "flows..."))

    ### Generate totals_by_sector, tbs
    tbs0 <- generateTbSfromSatSpec(sat_spec)
    
    ### Make tbs conform to the model schema
    tbs <- conformTbStoIOSchema(tbs0, sat_spec, model)
    
    ##Check for any loss of flow data
    
    # Add in DQ columns and additional contextual scores not provided
    # Only setting TemporalCorrelation for now
    tbs <- scoreContextualDQ(tbs) 
    
    # Check if all DQ columns are present. If not, print error message.

    #    len_dq_fields <- length(getDQfields(totals_by_sector))
#    if(len_dq_fields!=5){
#      logging::logerror(paste("Missing 1 or more data quality fields in satellite data.", len_dq_fields, "present"))
#    }
    
    # Convert totals_by_sector to standard satellite table format
    tbs <- conformTbStoStandardSatTable(tbs)
    
    #Map names for static files not already using FEDEFL
    if (!is.null(sat_spec$StaticFile)) {
      if (!substring(sat_spec$OriginalFlowSource,1,6) == 'FEDEFL') {
        tbs <- mapListbyName(tbs, sat_spec)
      }
    }
    
    for (r in model$specs$ModelRegionAcronyms) {
      sattable_r <- tbs[tbs$Location==r, ]
      if (r=="RoUS") {
        IsRoUS <- TRUE
      } else {
        IsRoUS <- FALSE
        # Change Location if model is a state model
        if (model$specs$ModelType=="state") {
          sattable_r[, "Location"] <- paste0("US-", r)
        }
      }
      for (year in sat_spec$DataYears){
      coeffs_by_sector_r <- generateFlowtoDollarCoefficient(tbs[tbs$Year==year, ], year,
                                                            model$specs$IOYear, r, IsRoUS = IsRoUS, model)
      coeffs_by_sector <- rbind(coeffs_by_sector, coeffs_by_sector_r)
      }
    }

    # Add totals_by_sector and coeffs_by_sector to the sattables list
    sattables$totals_by_sector[[sat_spec$Abbreviation]] <- tbs
    sattables$coeffs_by_sector[[sat_spec$Abbreviation]] <- coeffs_by_sector
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
  # Combine satellite tables (coeffs_by_sector) into a single df
  StandardizedSatelliteTable <- do.call(rbind, model$SatelliteTables$coeffs_by_sector)
  model$sattables_cast <- standardizeandcastSatelliteTable(StandardizedSatelliteTable,model)
  return(model)
}

#'Converts flows table into flows x sector matrix-like format
#'@param df a dataframe of flowables, contexts, units, sectors and locations
#'@param model an EEIO model with IO tables loaded
#'@return a flows x sector matrix-like dataframe 
standardizeandcastSatelliteTable <- function(df,model) {
  # Add fields for flows and sectors as combinations of existing fields
  df[, "Flow"] <- apply(df[, c("Flowable", "Context", "Unit")],
                              1, FUN = joinStringswithSlashes)
  df[, "Sector"] <- apply(df[, c("Sector", "Location")],
                                1, FUN = joinStringswithSlashes)
  # Cast df into a flow x sector matrix
  df_cast <- reshape2::dcast(df, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  # Move Flow to rowname so matrix is all numbers
  rownames(df_cast) <- df_cast$Flow
  df_cast$Flow <- NULL
  # Complete sector list according to model$Industries
  standard_columns <- tolower(apply(cbind(model$Industries, model$specs$PrimaryRegionAcronym),
                                    1, FUN = joinStringswithSlashes))
  df_cast[, setdiff(standard_columns, colnames(df_cast))] <- 0
  # Adjust column order to be the same with V_n rownames
  df_cast <- df_cast[, standard_columns]
  return(df_cast)
}

#'Reads a satellite table specification and generates a totals-by-sector table
#'@param sat_spec, a standard specification for a single satellite table
#'@return a totals-by-sector dataframe
generateTbSfromSatSpec <- function(sat_spec) {
  # Check if the satellite table uses a static file. If so, proceed.
  # If not, use specified functions in model metadata to load data from dynamic source
  if(!is.null(sat_spec$StaticFile)) {
    # If the file is a URL tested by the first 4 characters of the string = "http", don't wrap in system.file()
    if (substring(sat_spec$StaticFile, 0, 4)=="http") {
      totals_by_sector <- utils::read.table(sat_spec$StaticFile, sep = ",", header = TRUE, stringsAsFactors = FALSE,
                                            fileEncoding = 'UTF-8-BOM')  
    } else {
      totals_by_sector <- utils::read.table(system.file("extdata", sat_spec$StaticFile, package = "useeior"),
                                            sep = ",", header = TRUE, stringsAsFactors = FALSE,
                                            fileEncoding = 'UTF-8-BOM')
    }
  } else {
    func_to_eval <- sat_spec$ScriptFunctionCall
    totalsgenfunction <- as.name(func_to_eval)
    if (sat_spec$ScriptFunctionParameters == "model") {
      params <- model
    } else {
      params <- sat_spec$ScriptFunctionParameters
    }
    totals_by_sector <- do.call(eval(totalsgenfunction), list(params))
  }
  return(totals_by_sector)
}

#'Take a totals-by-sector df and maps flows to the model schema
#'@param tbs, totals-by-sector df
#'@param sat_spec, a standard specification for a single satellite table
#'@param model an EEIO model with IO tables loaded
#'@return a totals-by-sector df with the sectors and flow amounts corresponding to the model schema
conformTbStoIOSchema <- function(tbs, sat_spec, model) {
  # Check if the original data is BEA-based. If so, apply necessary allocation or aggregation.
  # If not, map data from original sector to BEA.
  if (sat_spec$SectorListSource == "BEA") {
    # If BEA years is not the same as model year, must perform allocation
    if (sat_spec$SectorListLevel == "Detail" && sat_spec$SectorListYear == 2007 && model$specs$BaseIOSchema == 2012) {
      tbs <- mapFlowTotalsbySectorfromBEASchema2007to2012(tbs)
    }
    # If the orginal data is at Detail level but model is not, apply aggregation
    if (sat_spec$SectorListLevel == "Detail" && model$specs$BaseIOLevel != "Detail") {
      tbs <- aggregateSatelliteTable(tbs,from_level = sat_spec$SectorListLevel,to_level = model$specs$BaseIOLevel,model)
    }
  } else if ("NAICS" %in% sat_spec$SectorListSource) {
    tbs <- mapFlowTotalsbySectorandLocationfromNAICStoBEA(tbs, sat_spec$DataYears[1], model)
  }
  # Check if disaggregation is needed based on model metadata
  if(!is.null(model$specs$DisaggregationSpecs) & !is.null(sat_spec$StaticFile)){
    tbs <- disaggregateSatelliteTable(model, tbs, sat_spec)
  }
  return(tbs)
}
