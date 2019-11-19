#' Load the template of standard satellite table.
#' @return A dataframe with the columns of the standard sat table format from the IO model builder.
getStandardSatelliteTableFormat <- function () {
  sat <- utils::read.table(system.file("extdata", "IOMB_Satellite_fields.csv", package = "useeior"),
                           sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  sat[1, ] <- NA
  return(sat)
}

#' Map a satellite table from NAICS-coded format to USEEIO-coded format.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param majormodelversionnumber Model's major version number.
#' @param satellitetableyear Year of the satellite table.
#' @return A satellite table aggregated by the USEEIO model sector codes.
mapSatTablefromNAICStoUSEEIO <- function (sattable, majormodelversionnumber, satellitetableyear) {
  modelcodelistname <- paste("USEEIO", majormodelversionnumber, "_Code", sep = "")
  modelcommoditylistname <- paste("USEEIO", majormodelversionnumber, "_Commodity", sep = "")
  # Load MasterCrosswalk
  crosswalk <- utils::read.table(system.file("extdata", "Crosswalk_MasterCrosswalk.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Generate NAICS-to-USEEIO mapping dataframe, assuming NAICS are 2012 NAICS.
  NAICStoUSEEIO <- unique(crosswalk[!is.na(crosswalk$`2012_NAICS_Code`), c("2012_NAICS_Code", modelcodelistname, modelcommoditylistname)])
  colnames(NAICStoUSEEIO) <- c("2012_NAICS_Code", "SectorCode", "SectorName")
  # Assign DQTechnological score based on the the correspondence between NAICS and USEEIO code:
  # If there is allocation (1 NAICS to 2 or more USEEIO), DQTechnological score = 2, otherwise, 1.
  for (NAICS in unique(NAICStoUSEEIO$`2012_NAICS_Code`)) {
    N_USEEIO <- nrow(NAICStoUSEEIO[NAICStoUSEEIO$`2012_NAICS_Code` == NAICS, ])
    if (N_USEEIO == 1) {
      NAICStoUSEEIO[NAICStoUSEEIO$`2012_NAICS_Code` == NAICS, "DQTechnological"] <- 1
    } else {
      NAICStoUSEEIO[NAICStoUSEEIO$`2012_NAICS_Code` == NAICS, "DQTechnological"] <- 2
    }
  }
  # Merge satellite table with NAICStoUSEEIO dataframe
  Sattable_USEEIO <- merge(sattable, NAICStoUSEEIO, by.x = "NAICS", by.y = "2012_NAICS_Code", all.x = TRUE)
  # Generate allocation_factor dataframe containing allocation factors between NAICS and BEA sectors
  allocation_factor <- getNAICStoBEAAllocation(satellitetableyear)
  colnames(allocation_factor) <- c("2012_NAICS_Code", "SectorCode", "allocation_factor")
  # Merge the USEEIO-coded satellite table with allocation_factor dataframe
  Sattable_USEEIO <- merge(Sattable_USEEIO, allocation_factor,
                           by.x = c("NAICS", "SectorCode"), by.y = c("2012_NAICS_Code", "SectorCode"), all.x = TRUE)
  # Replace NA in allocation_factor with 1
  Sattable_USEEIO[is.na(Sattable_USEEIO$allocation_factor), "allocation_factor"] <- 1
  # Calculate FlowAmount for USEEIO-coded sectors using allocation factors
  Sattable_USEEIO$FlowAmount <- Sattable_USEEIO$FlowAmount*Sattable_USEEIO$allocation_factor
  # Aggregate FlowAmount to USEEIO sectors
  Sattable_USEEIO <- stats::aggregate(FlowAmount~SectorCode+SectorName+FlowName+ReliabilityScore+DQTechnological, Sattable_USEEIO, sum)
  return(Sattable_USEEIO)
}

#' Calculates intensity coefficient (kg/$) for a standard satellite table.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoU A logical parameter indicating whether to adjust Industry output for Rest of US (RoU).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains intensity coefficient (kg/$).
generateFlowtoDollarCoefficient <- function (sattable, outputyear, referenceyear, location_acronym, IsRoUS = FALSE, model) {
  # Generate adjusted industry output
  Output_adj <- getAdjustedOutput(outputyear, location_acronym, IsRoUS, model)
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
  Sattable_standardformat[, "DQReliability"] <- sattable[, "ReliabilityScore"]
  Sattable_standardformat[, "DQTechnological"] <- sattable[, "DQTechnological"]
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
#' @return A more aggregated satellite table.
aggregateSatelliteTable <- function(sattable, from_level, to_level) {
  # Load MasterCrosswalk
  crosswalk <- utils::read.table(system.file("extdata", "Crosswalk_MasterCrosswalk.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Determine the columns within MasterCrosswalk that will be used in aggregation
  from_code <- paste("BEA", model$specs$BaseIOSchema, from_level, "Code", sep = "_")
  to_code <- paste("BEA", model$specs$BaseIOSchema, to_level, "Code", sep = "_")
  # Merge the satellite table with MasterCrosswalk
  sattable <- merge(sattable, unique(crosswalk[, c(from_code, to_code)]), by.x = "SectorCode", by.y = from_code)
  # Replace NA in ReliabilityScore column with 5
  sattable[is.na(sattable$ReliabilityScore), "ReliabilityScore"] <- 5
  # Aggregate FlowAmount by specified columns
  # Need particular aggregation functions, e.g. sum, weighted avg on ReliabilityScore
  sattable_agg <- stats::aggregate(sattable$FlowAmount, by = sattable[, c(to_code, "FlowName", "Compartment", "Unit", "ReliabilityScore", "Year", "MetaSources", "Location")], sum)
  colnames(sattable_agg)[c(1, ncol(sattable_agg))] <- c("SectorCode", "FlowAmount")
  return(sattable_agg)
}
