# Extract national total GHG by gas in physical unit (KT, or thousand metri ton) from the National GHG Industry Attribution Model
extractfromNationalGHGIndustryAttributionModel <- function(GHG_file) {

  # Extract national total GHG by gas in physical unit (KT, or thousand metri ton)
  GHG <- readxl::read_excel(GHG_file, sheet = "RESULTs_KT_gas")
  # Extract year information
  GHG$Year <- colnames(readxl::read_excel(GHG_file, sheet = "Year Input", range = "B1:B1"))
  # Reshape the table from wide to long
  GHG_long <- reshape2::melt(GHG, id.vars = c("BEA_2012_Sector_Code", "BEA_2012_Sector_Name", "Year"))
  # Modify column names
  colnames(GHG_long) <- c("SectorCode", "SectorName", "Year", "FlowName", "FlowAmount")
  # Convert unit from KT to KG
  GHG_long$FlowAmount <- GHG_long$FlowAmount*1E6
  
  # Extract ReliabilityScore
  ReliabilityScore <- readxl::read_excel(GHG_file, sheet = "RESULTs_ReliabiltyScores")
  ReliabilityScore_long <- reshape2::melt(ReliabilityScore, id.vars = c("BEA_2012_Sector_Code", "BEA_2012_Sector_Name"))
  colnames(ReliabilityScore_long) <- c("SectorCode", "SectorName", "FlowName", "ReliabilityScore")
  
  # Extract TechnologicalCorrelationScore
  TechnologicalCorrelation <- readxl::read_excel(GHG_file, sheet = "RESULTs_TechnologicalCorr") #range = "A1:Q407"
  TechnologicalCorrelation_long <- reshape2::melt(TechnologicalCorrelation, id.vars = c("BEA_2012_Sector_Code", "BEA_2012_Sector_Name"))
  colnames(TechnologicalCorrelation_long) <- c("SectorCode", "SectorName", "FlowName", "TechnologicalCorrelation")
  
  # Extract DataCollectionScore
  DataCollection <- readxl::read_excel(GHG_file, sheet = "RESULTs_DataCollectionScores")
  DataCollection_long <- reshape2::melt(DataCollection, id.vars = c("BEA_2012_Sector_Code", "BEA_2012_Sector_Name"))
  colnames(DataCollection_long) <- c("SectorCode", "SectorName", "FlowName", "DataCollection")
  
  # Extract GeographicalCorrelationScore
  GeographicalCorrelation <- readxl::read_excel(GHG_file, sheet = "RESULTs_GeographicalCorrScores")
  GeographicalCorrelation_long <- reshape2::melt(GeographicalCorrelation, id.vars = c("BEA_2012_Sector_Code", "BEA_2012_Sector_Name"))
  colnames(GeographicalCorrelation_long) <- c("SectorCode", "SectorName", "FlowName", "GeographicalCorrelation")
  
  # Extract TemporalCorrelationScore
  TemporalCorrelation <- readxl::read_excel(GHG_file, sheet = "RESULTs_TemporalCorrScores")
  TemporalCorrelation_long <- reshape2::melt(TemporalCorrelation, id.vars = c("BEA_2012_Sector_Code", "BEA_2012_Sector_Name"))
  colnames(TemporalCorrelation_long) <- c("SectorCode", "SectorName", "FlowName", "TemporalCorrelation")
  
  # Merge GHG amount with DQ scores
  merge_by_columns <- c("SectorCode", "SectorName", "FlowName")
  US_GHG <- merge(GHG_long, 
                  merge(ReliabilityScore_long,
                        merge(TechnologicalCorrelation_long,
                              merge(DataCollection_long,
                                    merge(GeographicalCorrelation_long, TemporalCorrelation_long, by = merge_by_columns),
                                    by = merge_by_columns), by = merge_by_columns), by = merge_by_columns), by = merge_by_columns)
  
  # Convert DQ scores to nearest int
  DQ_scores <- c("ReliabilityScore", "TechnologicalCorrelation", "DataCollection", "GeographicalCorrelation", "TemporalCorrelation")
  US_GHG[, DQ_scores] <- sapply(US_GHG[, DQ_scores], function(x) round(x, 0))
  US_GHG[, DQ_scores] <- sapply(US_GHG[, DQ_scores], as.integer)
  
  # Add Location, Compartment, Unit, and MetaSources columns
  US_GHG$Location <- "US"
  US_GHG$Compartment <- "air"
  US_GHG$Unit <- "kg"
  US_GHG$MetaSources <- "GHG"
  
  return(US_GHG)
}


# Standardize totals_by_sector
getStandardTotalsBySector <- function(totals_by_sector) {
  # Define standard column names
  stdcolnames <- c("Sector", "SectorName", "Flowable", "Year", "FlowAmount",
                   "DataReliability", "TechnologicalCorrelation", "DataCollection",
                   "GeographicalCorrelation", "TemporalCorrelation",
                   "Location", "Context", "Unit", "MetaSources")
  # Standardize Context column
  if ("Compartment"%in%colnames(totals_by_sector)) {
    totals_by_sector$Context <- totals_by_sector$Compartment
  } else if (isTRUE("Compartment"%in%colnames(totals_by_sector) & "Subcompartment"%in%colnames(totals_by_sector))) {
    totals_by_sector$Context <- apply(totals_by_sector[, c("Compartment", "Subcompartment")], 1, FUN = joinStringswithSlashes)
  }
  # Standardize DQ columns
  if ("ReliabilityScore"%in%colnames(totals_by_sector)) {
    totals_by_sector[, "DataReliability"] <- totals_by_sector[, "ReliabilityScore"]
  }
  if ("Temporal correlation"%in%colnames(totals_by_sector)) {
    totals_by_sector[, "TemporalCorrelation"] <- totals_by_sector[, "Temporal correlation"]
  }
  if ("Geographical correlation"%in%colnames(totals_by_sector)) {
    totals_by_sector[, "GeographicalCorrelation"] <- totals_by_sector[, "Geographical correlation"]
  }
  if ("Technological Correlation"%in%colnames(totals_by_sector)) {
    totals_by_sector[, "TechnologicalCorrelation"] <- totals_by_sector[, "Technological Correlation"]
  }
  if ("Data Collection Methods"%in%colnames(totals_by_sector)) {
    totals_by_sector[, "DataCollection"] <- totals_by_sector[, "Data Collection Methods"]
  }
  # Standardize all othr columns
  totals_by_sector$Flowable <- totals_by_sector$FlowName
  totals_by_sector$Sector <- totals_by_sector$SectorCode
  totals_by_sector[setdiff(stdcolnames, colnames(totals_by_sector))] <- NA
  totals_by_sector <- totals_by_sector[, stdcolnames]
  return(totals_by_sector)
}

