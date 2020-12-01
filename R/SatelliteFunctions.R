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
  NAICStoBEA <- model$crosswalk
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
  
  # Generate allocation_factor data frame containing allocation factors between NAICS and BEA sectors
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
  totals_by_sector_BEA[, c("NAICS", "TechnologicalCorrelationAdjustment", "allocation_factor")] <- NULL
  # Rename BEA to Sector
  colnames(totals_by_sector_BEA)[colnames(totals_by_sector_BEA)=="BEA"] <- "Sector"
  
  # Add in BEA industry/commodity names
  sectornames <- model$SectorNames
  # Add F01000 or F010 to sectornames
  if (model$specs$BaseIOLevel=="Detail") {
    sectornames <- rbind.data.frame(sectornames, c("F01000", "Household"))
  } else {
    sectornames <- rbind.data.frame(sectornames, c("F010", "Household"))
  }
  # Assign sector names to totals_by_sector_BEA
  totals_by_sector_BEA <- merge(totals_by_sector_BEA, sectornames,
                                by.x = "Sector", by.y = "Sector", all.x = TRUE)
  
  # Aggregate to BEA sectors using unique aggregation functions depending on the quantitive variable
  totals_by_sector_BEA_agg <- dplyr::group_by(totals_by_sector_BEA,
                                              Flowable, Context, Sector, SectorName,
                                              Location, Unit, Year, DistributionType) 
  totals_by_sector_BEA_agg <- dplyr::summarize(
    totals_by_sector_BEA_agg,
    FlowAmountAgg = sum(FlowAmount),
    Min = min(Min),
    Max = max(Max),
    DataReliability = weighted.mean(DataReliability, FlowAmount),
    TemporalCorrelation = weighted.mean(TemporalCorrelation, FlowAmount),
    GeographicalCorrelation = weighted.mean(GeographicalCorrelation, FlowAmount),
    TechnologicalCorrelation = weighted.mean(TechnologicalCorrelation, FlowAmount),
    DataCollection = weighted.mean(DataCollection, FlowAmount)
  )
  colnames(totals_by_sector_BEA_agg)[colnames(totals_by_sector_BEA_agg)=="FlowAmountAgg"] <- "FlowAmount"
  return(totals_by_sector_BEA_agg)
}

#' Calculates intensity coefficient (kg/$) for a standard satellite table.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains intensity coefficient (kg/$).
generateFlowtoDollarCoefficient <- function (sattable, outputyear, referenceyear, location_acronym, IsRoUS = FALSE, model) {
  # Generate adjusted industry output
  Output_adj <- getAdjustedOutput(outputyear, referenceyear, location_acronym, IsRoUS, model)
  # Merge the satellite table with the adjusted industry output
  Sattable_USEEIO_wOutput <- merge(sattable, Output_adj, by.x = "Sector", by.y = 0, all.x = TRUE)
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
#' @return A standard satellite table with coefficients (kg/$) and only columns completed in the original satellite table.
generateStandardSatelliteTable <- function (sattable) {
  # Get standard sat table fields
  fields <- getStandardSatelliteTableFormat()
  # Add missing fields as new columns to sattable
  sattable[, setdiff(fields, colnames(sattable))] <- ""
  # Sort by satellite table sector code
  Sattable_standardformat <- sattable[order(sattable$Sector), fields]
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
  sattable <- merge(sattable, unique(useeior::MasterCrosswalk2012[, c(from_code, to_code)]), by.x = "Sector", by.y = from_code)
  # Replace NA in DQ cols with 5
  dq_fields <- getDQfields(sattable)
  for (f in dq_fields) {
    sattable[is.na(sattable[, f]), f] <- 5
  }
  # Aggregate FlowAmount by specified columns
  aggbycolumns <- c(to_code, "Flowable", "Context", "Unit", dq_fields,
                    "Year", "MetaSources", "Location")
  # Need particular aggregation functions, e.g. sum, weighted avg on ReliabilityScore
  sattable_agg <- stats::aggregate(sattable$FlowAmount, by = sattable[, aggbycolumns], sum)
  colnames(sattable_agg)[c(1, ncol(sattable_agg))] <- c("Sector", "FlowAmount")
  return(sattable_agg)
}



#' Adds an indicator score to a totals by sector table. A short cut alternative to getting totals before model result
#' @param model A EEIO model with IOdata, satellite tables, and indicators loaded
#' @param totals_by_sector_name The name of one of the totals by sector tables available in model$SatelliteTables$totals_by_sector
#' @param indicator_code The code of the indicator of interest from the model$Indicators
#' @return a totals_by_sector table with fields from the Indicator table "Code" and "Amount", and calculated "IndicatorScore" added
calculateIndicatorScoresforTotalsBySector <- function(model, totals_by_sector_name, indicator_code) {
  # Define indicator variables
  indicator_vars <- c("Flowable", "Context", "Unit", "Amount", "Code")
  # Extract flows_in_indicator and totals_by_sector from model
  flows_in_indicator <- model$indicators[model$indicators["Code"]==indicator_code, indicator_vars]
  totals_by_sector <-  model$SatelliteTables$totals_by_sector[[totals_by_sector_name]]
  # Mergeflows_in_indicator and totals_by_sector and calculate IndicatorScore
  df <- merge(totals_by_sector, flows_in_indicator, by = c("Flowable", "Context", "Unit")) 
  df$IndicatorScore <- df$FlowAmount*df$Amount
  return(df)
}


