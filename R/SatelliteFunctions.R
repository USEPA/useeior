#' Load the template of standard satellite table.
#' @return A dataframe with the columns of the standard sat table format from the IO model builder.
getStandardSatelliteTableFormat <- function () {
  sat <- utils::read.table(system.file("extdata", "IOMB_Satellite_fields.csv", package = "useeior"),
                           sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  sat[1, ] <- NA
  return(sat)
}

#' Map a satellite table from NAICS-coded format to BEA-coded format.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param satellitetableyear Year of the satellite table.
#' @return A satellite table aggregated by the USEEIO model sector codes.
mapSatTablefromNAICStoBEA <- function (sattable, satellitetableyear) {
  # Generate NAICS-to-BEA mapping dataframe based on MasterCrosswalk2012, assuming NAICS are 2012 NAICS.
  NAICStoBEA <- unique(useeior::MasterCrosswalk2012[, c("NAICS_2012_Code", paste("BEA", model$specs$BaseIOSchema, "Detail_Code", sep = "_"))])
  colnames(NAICStoBEA) <- c("NAICS", "SectorCode")
  # Assign DQTechnological score based on the the correspondence between NAICS and BEA code:
  # If there is allocation (1 NAICS to 2 or more BEA), DQTechnological score = 2, otherwise, 1.
  for (NAICS in unique(NAICStoBEA$NAICS)) {
    N_BEA <- nrow(NAICStoBEA[NAICStoBEA$NAICS == NAICS, ])
    if (N_BEA == 1) {
      NAICStoBEA[NAICStoBEA$NAICS == NAICS, "TechnologicalCorrelation"] <- 1
    } else {
      NAICStoBEA[NAICStoBEA$NAICS == NAICS, "TechnologicalCorrelation"] <- 2
    }
  }
  # Merge satellite table with NAICStoBEA dataframe
  Sattable_BEA <- merge(sattable, NAICStoBEA, by = "NAICS", all.x = TRUE)
  # Generate allocation_factor dataframe containing allocation factors between NAICS and BEA sectors
  allocation_factor <- getNAICStoBEAAllocation(satellitetableyear)
  colnames(allocation_factor) <- c("NAICS", "SectorCode", "allocation_factor")
  # Merge the BEA-coded satellite table with allocation_factor dataframe
  Sattable_BEA <- merge(Sattable_BEA, allocation_factor, by = c("NAICS", "SectorCode"), all.x = TRUE)
  # Replace NA in allocation_factor with 1
  Sattable_BEA[is.na(Sattable_BEA$allocation_factor), "allocation_factor"] <- 1
  # Calculate FlowAmount for BEA-coded sectors using allocation factors
  Sattable_BEA$FlowAmount <- Sattable_BEA$FlowAmount*Sattable_BEA$allocation_factor
  # Aggregate FlowAmount to BEA sectors
  Sattable_BEA <- stats::aggregate(FlowAmount~SectorCode+SectorName+FlowName+ReliabilityScore+TechnologicalCorrelation, Sattable_BEA, sum)
  return(Sattable_BEA)
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
generateStandardSatelliteTable <- function (sattable, mapbyname = FALSE, sattablemeta) {
  # Get standard sat table format
  Sattable_standardformat <- getStandardSatelliteTableFormat()
  # Make room for new rows
  Sattable_standardformat[nrow(sattable), ] <- NA
  # Transfer values from unformatted table
  Sattable_standardformat[, "ProcessCode"] <- sattable[, "SectorCode"]
  Sattable_standardformat[, "ProcessName"] <- sattable[, "SectorName"]
  Sattable_standardformat[, "ProcessLocation"] <- sattable[, "Location"]
  if(mapbyname) {
    Sattable_standardformat[, "FlowName"] <- sattable[, "FlowName"]
  } else {
    Sattable_standardformat[, "CAS"] <- sattable[, "FlowName"]
  }
  Sattable_standardformat[, "FlowAmount"] <- sattable[, "FlowAmount"]
  
  #Map data quality fields
  Sattable_standardformat[, "DQReliability"] <- sattable[, "ReliabilityScore"]
  Sattable_standardformat[, "DQTemporal"] <- sattable[, "TemporalCorrelation"]
  Sattable_standardformat[, "DQGeographical"] <- sattable[, "GeographicalCorrelation"]
  Sattable_standardformat[, "DQTechnological"] <- sattable[, "TechnologicalCorrelation"]
  Sattable_standardformat[, "DQDataCollection"] <- sattable[, "DataCollection"]
  
  if("MetaSources" %in% colnames(sattable)) {
    Sattable_standardformat[, "MetaSources"] <- sattable[, "MetaSources"]
  }
  # Apply flow mapping
  if (mapbyname) {
    # Use new mapping
    Sattable_standardformat <- mapListbyName(Sattable_standardformat, sattablemeta)
  } else {
    columns <- c("FlowName","CAS","FlowCategory","FlowSubCategory","FlowUUID","FlowUnit")
    Sattable_standardformat[, columns] <- t(apply(Sattable_standardformat, 1, function(x) mapFlowbyCodeandCategory(x["CAS"], originalcategory = "")))
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
