# Functions to format, aggregate and otherwise wrangle satellite tables

#' Load the template of standard satellite table.
#' @return A dataframe with the columns of the standard sat table format from the IO model builder.
getStandardSatelliteTableFormat <- function () {
  sat <- configr::read.config(system.file("extdata/IOMB_Fields.yml", package="useeior"))[["SatelliteTable"]]
  return(sat)
}

#' Map a satellite table from NAICS-coded format to BEA-coded format.
#' @param totals_by_sector A standardized satellite table with resource and emission names from original sources.
#' @param totals_by_sector_year Year of the satellite table.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A satellite table aggregated by the USEEIO model sector codes.
mapFlowTotalsbySectorandLocationfromNAICStoBEA <- function (totals_by_sector, totals_by_sector_year, model) {
  # Consolidate master crosswalk on model level and rename
  NAICStoBEA <- unique(model$crosswalk[, c("NAICS","USEEIO")])
  colnames(NAICStoBEA) <- c("NAICS","BEA")
  # Modify TechnologicalCorrelation score based on the the correspondence between NAICS and BEA code
  # If there is allocation (1 NAICS to 2 or more BEA), add one to score = 2
  # Assign TechnologicalCorrelationAdjustment to NAICS
  NAICS_duplicates <- unique(NAICStoBEA[duplicated(NAICStoBEA$NAICS), "NAICS"])
  NAICStoBEA[NAICStoBEA$NAICS%in%NAICS_duplicates, "TechnologicalCorrelationAdjustment"] <- 1
  NAICStoBEA[!NAICStoBEA$NAICS%in%NAICS_duplicates, "TechnologicalCorrelationAdjustment"] <- 0
  
  # Rename the existing Sector field to NAICS
  colnames(totals_by_sector)[colnames(totals_by_sector)=="Sector"] <- "NAICS"
  # Merge totals_by_sector table with NAICStoBEA mapping
  totals_by_sector_BEA <- merge(totals_by_sector, NAICStoBEA, by = "NAICS", all.x = TRUE)
  
  # Because this occurs after disaggregation, some sectors may not map, update those sectors
  disaggNAICS <- unique(totals_by_sector_BEA[is.na(totals_by_sector_BEA$BEA),"NAICS"])
  totals_by_sector_BEA$BEA <- ifelse(totals_by_sector_BEA$NAICS %in% disaggNAICS, totals_by_sector_BEA$NAICS, totals_by_sector_BEA$BEA)
  totals_by_sector_BEA$TechnologicalCorrelationAdjustment[is.na(totals_by_sector_BEA$TechnologicalCorrelationAdjustment)] <- 0
  
  # Generate allocation_factor data frame containing allocation factors between NAICS and BEA sectors
  allocation_factor <- getNAICStoBEAAllocation(totals_by_sector_year, model)
  colnames(allocation_factor) <- c("NAICS", "BEA", "Location", "allocation_factor")
  # Merge the BEA-coded satellite table with allocation_factor dataframe
  totals_by_sector_BEA <- merge(totals_by_sector_BEA, allocation_factor,
                                by = c("NAICS", "BEA", "Location"), all.x = TRUE)
  # Replace NA in allocation_factor with 1
  totals_by_sector_BEA[is.na(totals_by_sector_BEA$allocation_factor), "allocation_factor"] <- 1
  # Calculate FlowAmount for BEA-coded sectors using allocation factors
  totals_by_sector_BEA$FlowAmount <- totals_by_sector_BEA$FlowAmount*totals_by_sector_BEA$allocation_factor
  
  # Apply tech correlation adjustment
  totals_by_sector_BEA$TechnologicalCorrelation <- totals_by_sector_BEA$TechnologicalCorrelation + totals_by_sector_BEA$TechnologicalCorrelationAdjustment
  # Drop unneeded cols
  totals_by_sector_BEA[, c("NAICS", "TechnologicalCorrelationAdjustment", "allocation_factor")] <- NULL
  # Rename BEA to Sector
  colnames(totals_by_sector_BEA)[colnames(totals_by_sector_BEA)=="BEA"] <- "Sector"
  
  totals_by_sector_BEA_agg <- collapseTBS(totals_by_sector_BEA, model)

  return(totals_by_sector_BEA_agg)
}

#' Calculates intensity coefficient (kg/$) for a standard satellite table.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param output_type Type of the output, e.g. "Commodity" or "Industry"
#' @return A dataframe contains intensity coefficient (kg/$).
generateFlowtoDollarCoefficient <- function (sattable, outputyear, referenceyear, location_acronym, IsRoUS = FALSE, model, output_type = "Industry") {
  # Generate adjusted industry output
  Output_adj <- adjustOutputbyCPI(outputyear, referenceyear, location_acronym, IsRoUS, model, output_type)
  rownames(Output_adj) <- gsub(paste0("/", location_acronym), "", rownames(Output_adj))
  # Merge the satellite table with the adjusted industry output
  Sattable_USEEIO_wOutput <- merge(sattable, Output_adj, by.x = "Sector", by.y = 0, all.x = TRUE)
  # Drop rows where output is zero
  outputcolname <- paste0(outputyear, output_type, "Output")
  Sattable_USEEIO_wOutput <- Sattable_USEEIO_wOutput[Sattable_USEEIO_wOutput[, outputcolname] != 0, ]
  # Drop rows where output is NA
  Sattable_USEEIO_wOutput <- Sattable_USEEIO_wOutput[!is.na(Sattable_USEEIO_wOutput[, outputcolname]), ]
  # Calculate FlowAmount by dividing the original FlowAmount by the adjusted industry output
  Sattable_USEEIO_wOutput$FlowAmount <- Sattable_USEEIO_wOutput$FlowAmount/Sattable_USEEIO_wOutput[, outputcolname]
  Sattable_USEEIO_wOutput[, outputcolname] <- NULL
  return(Sattable_USEEIO_wOutput)
}

#' Generate a standard satellite table with coefficients (kg/$) and only columns completed in the original satellite table.
#' @param sattable A satellite table contains FlowAmount already aggregated and transformed to coefficients.
#' @return A standard satellite table with coefficients (kg/$) and only columns completed in the original satellite table.
conformTbStoStandardSatTable <- function (sattable) {
  # Get standard sat table fields
  fields <- getStandardSatelliteTableFormat()
  # Add missing fields as new columns to sattable
  sattable[, setdiff(fields, colnames(sattable))] <- ""
  # Sort by satellite table sector code
  Sattable_standardformat <- as.data.frame(sattable[order(sattable$Sector), fields])
  return(Sattable_standardformat)
}


#' Stacks two tables up
#' @param sattable1 A standardized satellite table.
#' @param sattable2 Another standardized satellite table.
#' @return A complete standardized satellite table.
stackSatelliteTables <- function (sattable1, sattable2) {
  return(rbind(sattable1, sattable2))
}

#' Aggregate (FlowAmount in) satellite tables from BEA level to model configuration
#' @param sattable A satellite table to be aggregated based on the level (Detail, Summary, or Sector) of BEA code.
#' @param from_level The level of BEA code in the satellite table.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A more aggregated satellite table.
aggregateSatelliteTable <- function(sattable, from_level, model) {
  # Determine the columns within MasterCrosswalk that will be used in aggregation
  from_code <- paste0("BEA_", from_level)
  # Merge the satellite table with model$crosswalk
  sattable <- merge(sattable, unique(model$crosswalk[, c(from_code, "USEEIO")]), by.x = "Sector", by.y = from_code)
  # Update Sector field
  sattable$Sector <- sattable$USEEIO
  sattable_agg <- collapseTBS(sattable, model)
  return(sattable_agg)
}

#' Collapse a totals by sector table so that each flow sector combination exists only once
#' @param tbs totals by sector sourced from satellite table
#' @param model An EEIO model object with model specs and IO table loaded
#' @return aggregated totals by sector
collapseTBS <- function(tbs, model) {
  # Add in BEA industry names
  sectornames <- model$Industries[, c("Code", "Name")]
  colnames(sectornames) <- c("Sector", "SectorName")
  # Add F01000 or F010 to sectornames
  if (model$specs$BaseIOLevel=="Detail") {
    sectornames <- rbind.data.frame(sectornames, c("F01000", "Household"))
  } else {
    sectornames <- rbind.data.frame(sectornames, c("F010", "Household"))
  }
  # Assign sector names to TBS
  if("SectorName" %in% colnames(tbs)){
    tbs$SectorName <- NULL
  }
  tbs <- merge(tbs, sectornames, by = "Sector", all.x = TRUE)
  
  # Replace NA in DQ cols with 5
  dq_fields <- getDQfields(tbs)
  for (f in dq_fields) {
    tbs[is.na(tbs[, f]), f] <- 5
  }
  # Aggregate to BEA sectors using unique aggregation functions depending on the quantitative variable
  tbs_agg <- dplyr::group_by(tbs, Flowable, Context, FlowUUID, Sector, SectorName,
                             Location, Unit, Year, DistributionType) 
  tbs_agg <- dplyr::summarize(
    tbs_agg,
    FlowAmountAgg = sum(FlowAmount),
    Min = min(Min),
    Max = max(Max),
    DataReliability = stats::weighted.mean(DataReliability, FlowAmount),
    TemporalCorrelation = stats::weighted.mean(TemporalCorrelation, FlowAmount),
    GeographicalCorrelation = stats::weighted.mean(GeographicalCorrelation, FlowAmount),
    TechnologicalCorrelation = stats::weighted.mean(TechnologicalCorrelation, FlowAmount),
    DataCollection = stats::weighted.mean(DataCollection, FlowAmount),
    MetaSources = dplyr::nth(MetaSources, which.max(nchar(MetaSources))),
    .groups = 'drop'
  )
  colnames(tbs_agg)[colnames(tbs_agg)=="FlowAmountAgg"] <- "FlowAmount"
  return(tbs_agg)
    
}

#' Adds an indicator score to a totals by sector table. A short cut alternative to getting totals before model result
#' @param model A EEIO model with model specs, IO tables, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_name The name of the indicator of interest from the model$Indicators$factors
#' @return a totals_by_sector table with fields from the Indicator table "Code" and "Amount", and calculated "IndicatorScore" added
calculateIndicatorScoresforTotalsBySector <- function(model, totals_by_sector_name, indicator_name) {
  # Define indicator variables
  indicator_vars <- c("Flowable", "Context", "Unit", "Amount")
  # Extract flows_in_indicator and totals_by_sector from model
  flows_in_indicator <- model$Indicators$factors[model$Indicators$factors$Indicator==indicator_name, indicator_vars]
  totals_by_sector <-  model$SatelliteTables$totals_by_sector[[totals_by_sector_name]]
  # Mergeflows_in_indicator and totals_by_sector and calculate IndicatorScore
  df <- merge(totals_by_sector, flows_in_indicator, by = c("Flowable", "Context", "Unit")) 
  df$IndicatorScore <- df$FlowAmount*df$Amount
  df$Unit <- model$Indicators$meta[model$Indicators$meta$Name==indicator_name, 'Unit']
  return(df)
}

#' Get value added from BEA input-output use table, convert to standard totals_by_sector format.
#' @param model A EEIO model with model specs and IO tables loaded
#' @return A value-added totals_by_sector table with fields of standard totals_by_sector
getValueAddedTotalsbySector <- function(model) {
  # Extract ValueAdded from Use table, add names
  df <- merge(model$UseValueAdded, model$ValueAddedMeta[, c("Code_Loc", "Name")],
              by.x = 0, by.y = "Code_Loc")
  df[, c("Row.names", "Code_Loc")] <- NULL
  # Convert to standard totals_by_sector format
  df <- reshape2::melt(df, id.vars = "Name")
  colnames(df) <- c("Flowable", "Sector", "FlowAmount")
  # Add columns to convert to standard totals_by_sector format
  df[, "Sector"] <- gsub("/.*", "", df$Sector)
  df <- merge(df, model$Industries[, c("Code", "Name")],
              by.x = "Sector", by.y = "Code", all.x = TRUE)
  df[, "Context"] <- "Economic"
  df[, "Unit"] <- "USD"
  df[, "Year"] <- model$specs$SatelliteTable$VADD$SectorListYear
  df[, "MetaSources"] <- model$specs$SatelliteTable$VADD$SectorListSource
  df[, "Location"] <- model$specs$SatelliteTable$VADD$Locations
  df[, c("DataReliability", "TemporalCorrelation", "GeographicalCorrelation",
         "TechnologicalCorrelation", "DataCollection")] <- 1
  rownames(df) <- NULL
  return(df)
}

#' Check duplicates across satellite tables.
#' @param sattable_ls A list of satellite tables
#' @return Messages about whether there are duplicates across satellite tables
checkDuplicateFlowsBySector <- function(sattable_ls) {
  # Extract unique Flowable and Context combination from each sat table
  for (table_name in names(sattable_ls)){
    # Update context to reflect only primary context (e.g. emission/air)
    sattable_ls[[table_name]][, "Context"] <- stringr::str_match(sattable_ls[[table_name]][, "Context"],"\\w*\\/?\\w*")
    # Store only flow information for each table
    sattable_ls[[table_name]] <- unique(sattable_ls[[table_name]][, c("Flowable", "Context", "Sector")])
    sattable_ls[[table_name]][, "name"] <- table_name
  }
  unique_flows <- do.call(rbind, sattable_ls)
  # Check duplicates in all unique flows
  duplicates <- unique_flows[duplicated(unique_flows[, c("Flowable", "Context", "Sector")]) |
                               duplicated(unique_flows[, c("Flowable", "Context", "Sector")], fromLast = TRUE), ]
  duplicates <- duplicates[order(duplicates$Context, duplicates$Flowable, duplicates$Sector), ]
  rownames(duplicates) <- NULL
  
  if (nrow(duplicates) > 0){
    logging::logdebug("Duplicate flows exist across satellite tables and should be reviewed.")
    logging::logdebug(duplicates)
  } else {
    logging::loginfo("No duplicate flows exist across satellite tables.")
  }
}

#' Map a satellite table from BEA Detail industry 2007 schema to 2012 schema.
#' @param totals_by_sector A standardized satellite table with resource and emission names from original sources.
#' @return A satellite table aggregated by the USEEIO model sector codes.
mapFlowTotalsbySectorfromBEASchema2007to2012 <- function(totals_by_sector) {
  # Load pre-saved mapping between BEA Detail Industry under 2007 and 2012 schemas
  mapping_file <- "Crosswalk_DetailIndustry2007and2012Schemas.csv"
  mapping <- utils::read.table(system.file("extdata", mapping_file, package = "useeior"),
                               sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Determine sectors that need allocation
  allocation_sectors <- mapping[duplicated(mapping$BEA_2007_Code) | duplicated(mapping$BEA_2007_Code, fromLast = TRUE), ]
  # Calculate allocation factors for the industries of 2012 schema that need allocation
  totals_by_sector_new <- data.frame()
  for (year in unique(totals_by_sector$Year)) {
    mapping_year <- mapping
    totals_by_sector_year <- totals_by_sector[totals_by_sector$Year==year, ]
    for (industry in unique(allocation_sectors$BEA_2007_Code)) {
      # For each 2007 schema industry, find its corresponding 2012 schema industries
      industries <- mapping[mapping$BEA_2007_Code==industry, "BEA_2012_Code"]
      # Use useeior::Detail_GrossOutput_IO as weight to allocate
      # Do not use model$MultiYearIndustryOutput because model level may not be Detail
      weight <- useeior::Detail_GrossOutput_IO[industries, as.character(year)]
      mapping_year[mapping_year$BEA_2007_Code==industry, "Ratio"] <- weight/sum(weight)
    }
    # Map totals_by_sector from BEA 2007 schema to 2012 schema
    totals_by_sector_year <- merge(totals_by_sector_year, mapping_year,
                                   by.x = "Sector", by.y = "BEA_2007_Code", all.x = TRUE)
    totals_by_sector_year[is.na(totals_by_sector_year$Ratio), "Ratio"] <- 1
    totals_by_sector_year$FlowAmount <- totals_by_sector_year$FlowAmount*totals_by_sector_year$Ratio
    totals_by_sector_year$Sector <- ifelse(is.na(totals_by_sector_year$BEA_2012_Code), totals_by_sector_year$Sector, totals_by_sector_year$BEA_2012_Code)
    totals_by_sector_year[, c("BEA_2012_Code", "Ratio")] <- NULL
    totals_by_sector_new <- rbind(totals_by_sector_new, totals_by_sector_year)
  }
  return(totals_by_sector_new)
}

#'Checks flow amounts are equal in totals by sector after conforming to model schema
#'@param tbs0, totals-by-sector df in source schema
#'@param tbs, totals-by-sector df in model schema
#'@param tolerance, tolerance level for data loss
checkSatelliteFlowLoss <- function(tbs0, tbs, tolerance=0.005) {
  tbs0 <- tbs0[!is.na(tbs0$Sector), ]
  tbs <- tbs[!is.na(tbs$Sector), ]
  
  tbs0 <- tbs0[, c("Flowable", "Context", "FlowAmount")]
  tbs <- tbs[, c("Flowable", "Context", "FlowAmount")]
  tbs0_agg <- dplyr::group_by(tbs0, Flowable, Context)   
  tbs0_agg <- dplyr::summarize(tbs0_agg,
                               FlowAmount = sum(FlowAmount)
                               )
  tbs_agg <- dplyr::group_by(tbs, Flowable, Context)   
  tbs_agg <- dplyr::summarize(tbs_agg,
                               FlowAmount = sum(FlowAmount)
                              )
  tbs0_agg$Flow <- apply(tbs0_agg[, c('Context', 'Flowable')],1, FUN = joinStringswithSlashes)
  tbs_agg$Flow <- apply(tbs_agg[, c('Context', 'Flowable')], 1, FUN = joinStringswithSlashes)
  lost_flows <- setdiff(tbs0_agg$Flow, tbs_agg$Flow)

  if(length(lost_flows) > 0){
    df <- data.frame(Flow = lost_flows, FlowAmount = 0)
    tbs_agg <- rbind(tbs_agg, df)
    logging::logdebug("Flows lost upon conforming to model schema  :")
    logging::logdebug(lost_flows)
  }

  tbs_agg[order(match(tbs_agg$Flow, tbs0_agg$Flow)),1, drop=FALSE]
  rel_diff <- abs((tbs_agg$FlowAmount - tbs0_agg$FlowAmount)/tbs0_agg$FlowAmount)
  n <- length(subset(rel_diff, rel_diff > tolerance))

  if(n > 0){
    logging::logdebug("Data loss on conforming to model schema")    
  }

}

#' Sets the Year in a tbs to be the year of the highest frequency for a given flow when that flow is reported
#' in more than a single year
#' @param tbs, a model total by sector file
#' @return df, the tbs
setCommonYearforFlow <- function(tbs) {
  # Add new column Flow to tbs
  tbs$Flow <- apply(tbs[, c("Flowable", "Context", "Unit")], 1, FUN = joinStringswithSlashes)
  # Create flow_year_df to determine whether each flow has single year
  flow_year_df <- reshape2::dcast(tbs[, c("Year", "Flow")], Flow ~ Year, value.var = "Flow", length)
  rownames(flow_year_df) <- flow_year_df$Flow
  flow_year_df$Flow <- NULL
  # For each flow with multiple years, get the year that has the highest frequency
  # Then in the original tbs, set Year to this year for these rows
  for (flow in rownames(flow_year_df[rowSums(flow_year_df != 0) > 1, ])) {
    year <- colnames(flow_year_df[flow, ])[max.col(as.matrix(flow_year_df[flow, ]), ties.method = c("last"))]
    tbs[tbs$Flow==flow, "Year"] <- year
    logging::logdebug(paste("Flow year of", flow, "changed to", year))
  }
  return(tbs)
}

#' Removes flow data where sectors are NA after mapping. Should only be used after checkSatelliteFlowLoss
#' @param tbs, totals-by-sector df in model schema
#' @return df, the modified tbs
removeMissingSectors <- function(tbs) {
  df <- tbs[!is.na(tbs$Sector), ]
  n <- nrow(tbs) - nrow(df)
  if(n > 0){
    logging::logdebug(paste0(n, "records dropped with no sector"))
  }
  return(df)
}
