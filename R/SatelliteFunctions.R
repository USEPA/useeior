#' Load the template of standard satellite table.
#' @return A dataframe with the columns of the standard sat table format from the IO model builder.
getStandardSatelliteTableFormat <- function () {
  sat <- utils::read.table(system.file("extdata", "IOMB_Satellite_fields.csv", package = "useeior"),
                           sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  sat[1, ] <- NA
  return(sat)
}

#' Map a satellite table from NAICS-coded format to BEA-coded format.
#' @param totals_by_sector A standardized satellite table with resource and emission names from original sources.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param satellitetableyear Year of the satellite table.
#' @return A satellite table aggregated by the USEEIO model sector codes.
mapFlowTotalsbySectorandLocationfromNAICStoBEA <- function (totals_by_sector, totals_by_sector_year, model) {
  # Generate NAICS-to-BEA mapping dataframe based on MasterCrosswalk2012, assuming NAICS are 2012 NAICS.
  NAICStoBEA <- unique(useeior::MasterCrosswalk2012[, c("NAICS_2012_Code", paste("BEA", model$specs$BaseIOSchema,model$specs$BaseIOLevel, "Code", sep = "_"))])
  colnames(NAICStoBEA) <- c("NAICS", "BEA")
  # Modify TechnologicalCorrelation score based on the the correspondence between NAICS and BEA code
  # If there is allocation (1 NAICS to 2 or more BEA), add one to score = 2
  
  #Drop any rows without matches between NAICS and BEA in the mapping
  NAICStoBEA <- na.omit(NAICStoBEA)
  
  # Assign TechnologicalCorrelationAdjustment to NAICS
  NAICS_duplicates <- unique(NAICStoBEA[duplicated(NAICStoBEA$NAICS), "NAICS"])
  NAICStoBEA[NAICStoBEA$NAICS%in%NAICS_duplicates, "TechnologicalCorrelationAdjustment"] <- 1
  NAICStoBEA[!NAICStoBEA$NAICS%in%NAICS_duplicates, "TechnologicalCorrelationAdjustment"] <- 0
  
  #Rename the existing SectorCode field to NAICS
  names(totals_by_sector)[names(totals_by_sector)=="SectorCode"] <- "NAICS"
  
  # Merge totals_by_sector table with NAICStoBEA mapping
  totals_by_sector_BEA <- merge(totals_by_sector, NAICStoBEA,by="NAICS", all.x = TRUE)
  #the BEA sector codes are now added as Sector.y
  
  # Generate allocation_factor dataframe containing allocation factors between NAICS and BEA sectors
  allocation_factor <- getNAICStoBEAAllocation(totals_by_sector_year, model)
  colnames(allocation_factor) <- c("NAICS", "BEA", "allocation_factor")
  # Merge the BEA-coded satellite table with allocation_factor dataframe
  totals_by_sector_BEA <- merge(totals_by_sector_BEA, allocation_factor, by = c("NAICS", "BEA"), all.x = TRUE)
  # Replace NA in allocation_factor with 1
  totals_by_sector_BEA[is.na(totals_by_sector_BEA$allocation_factor), "allocation_factor"] <- 1
  # Calculate FlowAmount for BEA-coded sectors using allocation factors
  totals_by_sector_BEA$FlowAmount <- totals_by_sector_BEA$FlowAmount*totals_by_sector_BEA$allocation_factor
  
  # Apply tech correlation adjustment
  totals_by_sector_BEA$TechnologicalCorrelation <- totals_by_sector_BEA$TechnologicalCorrelation + totals_by_sector_BEA$TechnologicalCorrelationAdjustment
  
  # Drop unneeded cols
  cols_to_drop <- c("NAICS","TechnologicalCorrelationAdjustment","allocation_factor")
  totals_by_sector_BEA <- totals_by_sector_BEA[,-which(names(totals_by_sector_BEA) %in% cols_to_drop)]
  
  #Rename BEA to SectorCode
  names(totals_by_sector_BEA)[names(totals_by_sector_BEA)=="BEA"] <- "SectorCode"
  
  # Add in BEA industry names
  industrynames <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
  colnames(industrynames) <- c("SectorCode","SectorName")
  totals_by_sector_BEA <- merge(totals_by_sector_BEA,industrynames,by="SectorCode",all.x=TRUE)
  
  # Aggregate to BEA sectors
  # Unique aggregation functions are used depending on the quantitive variable
  totals_by_sector_BEA_agg <- dplyr::group_by(totals_by_sector_BEA, FlowName,Compartment,SectorCode,SectorName,Location,Unit,Year,DistributionType) 
  totals_by_sector_BEA_agg <- dplyr::summarize(
    totals_by_sector_BEA_agg,
    FlowAmountAgg = sum(FlowAmount),
    Min = min(Min),
    Max = max(Max),
    ReliabilityScore = weighted.mean(ReliabilityScore, FlowAmount),
    TemporalCorrelation = weighted.mean(TemporalCorrelation, FlowAmount),
    GeographicalCorrelation = weighted.mean(GeographicCorrelation, FlowAmount),
    TechnologicalCorrelation = weighted.mean(TechnologicalCorrelation, FlowAmount),
    DataCollection = weighted.mean(DataCollection, FlowAmount)
  )

  names(totals_by_sector_BEA_agg)[names(totals_by_sector_BEA_agg)=="FlowAmountAgg"] <- "FlowAmount"
  
  return(totals_by_sector_BEA_agg)
}

#' Calculates intensity coefficient (kg/$) for a standard satellite table.
#'
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#'
#' @return A dataframe contains intensity coefficient (kg/$).
generateFlowtoDollarCoefficient <- function (sattable, outputyear, referenceyear, location_acronym, IsRoUS = FALSE, model) {
  # Generate adjusted industry output
  Output_adj <- getAdjustedOutput(outputyear, referenceyear, location_acronym, IsRoUS, model)
  # Merge the satellite table with the adjusted industry output
  Sattable_USEEIO_wOutput <- merge(sattable, Output_adj, by.x = "SectorCode", by.y = 0, all.x = TRUE)
  # Drop rows where output is zero
  outputcolname <- paste(outputyear, "IndustryOutput", sep = "")
  Sattable_USEEIO_wOutput <- Sattable_USEEIO_wOutput[Sattable_USEEIO_wOutput[, outputcolname] != 0, ]
  # Drop rows where output is NA
  Sattable_USEEIO_wOutput <- Sattable_USEEIO_wOutput[!is.na(Sattable_USEEIO_wOutput[, outputcolname]), ]
  # Calculate FlowAmount by dividing the original FlowAmount by the adjusted industry output
  Sattable_USEEIO_wOutput$FlowAmount <- Sattable_USEEIO_wOutput$FlowAmount/Sattable_USEEIO_wOutput[, outputcolname]
  Sattable_USEEIO_wOutput[, outputcolname] <- NULL
  return(Sattable_USEEIO_wOutput)
}

#' Generate a standard satellite table with coefficients (kg/$) and only columns completed in the original satellite table.
#' @param sattable A statellite table contains FlowAmount already aggregated and transformed to coefficients.
#' @param mapbyname A logical parameter indicating whether to map the satellite table by FlowName.
#' @param sattablemeta Meta data of the satellite table.
#' @return A standard satellite table with coefficients (kg/$) and only columns completed in the original satellite table.
generateStandardSatelliteTable <- function (sattable, sattablemeta) {
  # Get standard sat table format
  Sattable_standardformat <- getStandardSatelliteTableFormat()
  # Make room for new rows
  Sattable_standardformat[nrow(sattable), ] <- NA
  # Transfer values from unformatted table
  Sattable_standardformat[, "ProcessCode"] <- sattable[, "SectorCode"]
  Sattable_standardformat[, "ProcessName"] <- sattable[, "SectorName"]
  Sattable_standardformat[, "ProcessLocation"] <- sattable[, "Location"]
  Sattable_standardformat[, "FlowName"] <- sattable[, "FlowName"]
  Sattable_standardformat[, "FlowCategory"] <- sattable[, "Compartment"]  
  Sattable_standardformat[, "FlowSubCategory"] <- NA  
  Sattable_standardformat[, "FlowAmount"] <- sattable[, "FlowAmount"]   
  Sattable_standardformat[, "FlowUnit"] <- sattable[, "Unit"]

  
  #Map data quality fields
  Sattable_standardformat[, "DQReliability"] <- sattable[, "ReliabilityScore"]
  Sattable_standardformat[, "DQTemporal"] <- sattable[, "TemporalCorrelation"]
  Sattable_standardformat[, "DQGeographical"] <- sattable[, "GeographicalCorrelation"]
  Sattable_standardformat[, "DQTechnological"] <- sattable[, "TechnologicalCorrelation"]
  Sattable_standardformat[, "DQDataCollection"] <- sattable[, "DataCollection"]
  
  if("MetaSources" %in% colnames(sattable)) {
    Sattable_standardformat[, "MetaSources"] <- sattable[, "MetaSources"]
  }
  # Sort the satellite table sector code
  Sattable_standardformat <- Sattable_standardformat[order(Sattable_standardformat$ProcessCode), ]
  return(Sattable_standardformat)
}


#' Stacks two tables up
#' @param sattable1 A standardized statellite table.
#' @param sattable2 Another standardized statellite table.
#' @return A complete standardized statellite table.
stackSatelliteTables <- function (sattable1, sattable2) {
  return(rbind(sattable1, sattable2))
}

#' Aggreagte (FlowAmount in) satellite tables from one BEA level to another
#' @param sattable A satellite table to be aggregated based on the level (Detail, Summary, or Sector) of BEA code.
#' @param from_level The level of BEA code in the satellite table.
#' @param to_level The level of BEA code this satellite table will be aggregated to.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A more aggregated satellite table.
aggregateSatelliteTable <- function(sattable, from_level, to_level, model) {
  # Determine the columns within MasterCrosswalk that will be used in aggregation
  from_code <- paste("BEA", model$specs$BaseIOSchema, from_level, "Code", sep = "_")
  to_code <- paste("BEA", model$specs$BaseIOSchema, to_level, "Code", sep = "_")
  # Merge the satellite table with MasterCrosswalk2012
  sattable <- merge(sattable, unique(useeior::MasterCrosswalk2012[, c(from_code, to_code)]), by.x = "SectorCode", by.y = from_code)
  # Replace NA in DQ cols with 5
  dq_fields <- getDQfields(sattable)
  for (f in dq_fields) {
    sattable[is.na(sattable[, f]), f] <- 5
  }
  # Aggregate FlowAmount by specified columns
  DQscores <- c("ReliabilityScore", "TemporalCorrelation", "GeographicalCorrelation", "TechnologicalCorrelation", "DataCollection")
  # Need particular aggregation functions, e.g. sum, weighted avg on ReliabilityScore
  sattable_agg <- stats::aggregate(sattable$FlowAmount, by = sattable[, c(to_code, "FlowName", "Compartment", "Unit", DQscores, "Year", "MetaSources", "Location")], sum)
  colnames(sattable_agg)[c(1, ncol(sattable_agg))] <- c("SectorCode", "FlowAmount")
  return(sattable_agg)
}

