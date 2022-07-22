# Functions that use sector crosswalks

#' Function to externalize the BEA to NAICS crosswalk
#' @return A crosswalk linking 2007 and 2012 NAICS codes to 2012 Sector, Summary, and Detail BEA codes
loadMasterCrosswalk <- function(){
  # Pull the mastercrosswalk created in the data-raw subdirectory
  BEAtoNAICSCrosswalk <- useeior::MasterCrosswalk2012
  return(BEAtoNAICSCrosswalk)
}

#' Determine allocation factors between NAICS and BEA sectors based on Industry output.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param year Year of model Industry output.
#' @return A table of allocation factors between NAICS and BEA sectors.
getNAICStoBEAAllocation <- function (year, model) {
  # Keep USEEIO and NAICS columns in MasterCrosswalk2012 table based on the model specs
  NAICStoBEA <- unique(model$crosswalk[, c("NAICS", "USEEIO")])
  colnames(NAICStoBEA) <- c("NAICS_Code", "BEA_Code")
  # Drop 2-digit NAICS code
  NAICStoBEA <- NAICStoBEA[nchar(NAICStoBEA$NAICS_Code) > 2, ]
  # Select the repeated NAICS codes that need allocation
  AllocationCodes <- NAICStoBEA[duplicated(NAICStoBEA$NAICS_Code) | duplicated(NAICStoBEA$NAICS_Code, fromLast = TRUE), ]
  AllocationCodes <- stats::na.omit(AllocationCodes)
  # Merge AllocationCodes with Gross Output table to calculate allocation factors
  output <- model$MultiYearIndustryOutput[, as.character(year), drop = FALSE]
  output$Code <- gsub("/.*", "", row.names(output))
  output$Location <- gsub(".*/", "", row.names(output))
  AllocationTable <- merge(AllocationCodes, output,
                           by.x = "BEA_Code", by.y = "Code", all.x = TRUE)
  AllocationTable$Output <- AllocationTable[, as.character(year)]
  # Insert placeholders for NAs in the "Output" column
  AllocationTable[is.na(AllocationTable)] <- 1
  # Aggregate Output for the same NAICS code
  sum_temp <- stats::aggregate(AllocationTable$Output, by = list(AllocationTable$NAICS_Code), sum)
  colnames(sum_temp) <- c("NAICS_Code", "SumOutput")
  AllocationTable <- merge(AllocationTable, sum_temp, by = "NAICS_Code", all.x = TRUE)
  # Calculate allocation factors
  AllocationTable$allocation_factor <- AllocationTable$Output/AllocationTable$SumOutput
  # Keep wanted columns
  AllocationTable <- AllocationTable[, c("NAICS_Code", "BEA_Code", "Location", "allocation_factor")]
  return(AllocationTable)
}


#' Get 2-6 digit NAICS codes and names for year specified.
#' @param year int. 2012 or 2007 accepted.
#' @return dataframe with columns NAICS_year_Code and NAICS_year_Name.
getNAICS2to6DigitsCodeName <- function (year) {
  # Download 2-6 digits NAICS table
  if (year == 2012) {
    FileName <- "inst/extdata/2-digit_2012_Codes.xls"
    url <- "https://www.census.gov/eos/www/naics/2012NAICS/2-digit_2012_Codes.xls"
  } else {
    FileName <- "inst/extdata/naics07.xls"
    url <- "https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07.xls"
  }
  if(!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  
  # Load 2-6 digits NAICS table
  NAICS <- as.data.frame(readxl::read_excel(FileName, sheet = 1, col_names = TRUE))[-1,-1]
  colnames(NAICS) <- c("NAICS_Code", "NAICS_Name")
  # Split the NAICS code with dash ("-)
  DashSplit <- do.call("rbind.data.frame", apply(do.call("rbind", strsplit(NAICS$NAICS_Code, "-")),
                                                 1, function(x) seq(x[1], x[2], 1)))
  colnames(DashSplit) <- c(paste("V", 1:ncol(DashSplit), sep=""))
  # Merge back with NAICS
  NAICSCodeName <- cbind(NAICS, DashSplit)
  # Reshape to long table
  NAICSCodeName <- reshape2::melt(NAICSCodeName[, -1], id.vars = "NAICS_Name")
  # Drop unwanted column and duplicated rows
  NAICSCodeName$variable <- NULL
  NAICSCodeName$value <- as.integer(NAICSCodeName$value)
  NAICSCodeName <- unique(NAICSCodeName)
  # Re-order and rename columns
  NAICSCodeName <- NAICSCodeName[, c("value", "NAICS_Name")]
  colnames(NAICSCodeName) <- paste("NAICS", year, c("Code", "Name"), sep = "_")
  return(NAICSCodeName)
}

#' Get 2-6 digit NAICS codes in a crosswalk format for year specified.
#' @param year int, 2012 or 2007 accepted.
#' @return data frame with columns NAICS_2, NAICS_3, NAICS_4, NAICS_5, NAICS_6.
getNAICS2to6Digits <- function (year) {
  # Download 2-6 digits NAICS table
  if (year == 2012) {
    FileName <- "inst/extdata/2-digit_2012_Codes.xls"
    url <- "https://www.census.gov/eos/www/naics/2012NAICS/2-digit_2012_Codes.xls"
  } else {
    FileName <- "inst/extdata/naics07.xls"
    url <- "https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07.xls"
  }
  if(!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  
  # Load 2-6 digits NAICS table
  NAICS <- as.data.frame(readxl::read_excel(FileName, sheet = 1, col_names = TRUE))[-1,-1]
  colnames(NAICS) <- c("NAICS_Code", "NAICS_Name")
  NAICS$NAICS_Code <- suppressWarnings(as.integer(NAICS$NAICS_Code))
  NAICS <- NAICS[!is.na(NAICS$NAICS_Code), ]
  # Reshape the table
  NAICSwide <- as.data.frame(NAICS[nchar(NAICS$NAICS_Code)==6, ])
  NAICSwide[, paste0("NAICS_", c(2:5))] <- cbind(substr(NAICSwide$NAICS_Code, 1, 2),
                                                 substr(NAICSwide$NAICS_Code, 1, 3),
                                                 substr(NAICSwide$NAICS_Code, 1, 4),
                                                 substr(NAICSwide$NAICS_Code, 1, 5))
  NAICSwide$NAICS_6 <- NAICSwide$NAICS_Code
  NAICSwide <- NAICSwide[, -c(1:2)]
  return(NAICSwide)
}

#' Get 7-10 digit NAICS codes and names (agricultural, manufacturing, and mining industries) for year specified.
#' @param year int. 2012 or 2007 accepted.
#' @return data frame with columns NAICS_year_Code and NAICS_year_Name.
getNAICS7to10DigitsCodeName <- function (year) {
  if (year==2012) {
    # Download Census 2012 Numerical List of Manufactured and Mineral Products
    SectorList <- c(211, 212, 213,
                    311, 312, 313, 314, 315, 316,
                    321, 322, 323, 324, 325, 326, 327,
                    331, 332, 333, 334, 335, 336, 337, 339)
    CensusNAICS <- data.frame()
    for(sector in SectorList) {
      FileName <- paste0("inst/extdata/CensusManufacturing2012NAICS_", sector, ".xls")
      url <- paste0("https://www.census.gov/manufacturing/numerical_list/", sector, ".xls")
      if(!file.exists(FileName)) {
        utils::download.file(url, FileName, mode = "wb")
      }
      CensusNAICS_sector <- as.data.frame(readxl::read_excel(FileName, sheet = 1,
                                                             col_names = TRUE, skip = 2))[, 1:2]
      colnames(CensusNAICS_sector) <- c("NAICS_2012_Code", "NAICS_2012_Name")
      CensusNAICS <- rbind(CensusNAICS, CensusNAICS_sector)
    }
    # NAICS from USDA
    coaNAICS <- utils::read.table(system.file("extdata", "Crosswalk_COAtoNAICS.csv", package = "useeior"),
                                  sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    # Subset dataset and change column names to match other NAICS datasets
    coaNAICS <- coaNAICS[, c("NAICS_2012_Code", "Activity")]
    colnames(coaNAICS) <- c("NAICS_2012_Code", "NAICS_2012_Name")
    # Create 10 digit NAICS out of the 8 digit so Code name isn't dropped in future function
    coaNAICS10 <- coaNAICS
    coaNAICS10$NAICS_2012_Code <- stringr::str_pad(coaNAICS10$NAICS_2012_Code, width=10, side="right", pad="0")
    # row bind 8 digit and 10 digit Census of Ag NAICS
    coaNAICS <- rbind(coaNAICS, coaNAICS10)
    # row bind NAICS data from different sources
    CensusNAICS <- rbind(CensusNAICS, coaNAICS)
  }
  return(CensusNAICS)
}

#' Get 7-10 digit NAICS codes in a crosswalk format for year specified.
#' @param year int. 2012 or 2007 accepted.
#' @return data frame with columns NAICS_7, NAICS_8, NAICS_9, NAICS_10.
getNAICS7to10Digits <- function (year) {
  NAICSCodeName <- getNAICS7to10DigitsCodeName(year)
  # Change column name from year-specific to generic
  colnames(NAICSCodeName) <- c("NAICS_Code", "NAICS_Name")
  # Reshape the table
  NAICSwide <- as.data.frame(NAICSCodeName[nchar(NAICSCodeName$NAICS_Code)==10, ])
  NAICSwide[, paste0("NAICS_", c(7:9))] <- cbind(substr(NAICSwide$NAICS_Code, 1, 7),
                                                 substr(NAICSwide$NAICS_Code, 1, 8),
                                                 substr(NAICSwide$NAICS_Code, 1, 9))
  NAICSwide$NAICS_10 <- NAICSwide$NAICS_Code
  NAICSwide <- NAICSwide[, -c(1:2)]
  return(NAICSwide)
}

#' Get 2012 2-10 digit NAICS codes in a crosswalk format for year specified.
#' @param year int. 2012 or 2007 accepted.
#' @return data frame with columns NAICS_2, NAICS_3, NAICS_4, NAICS_5, NAICS_6, NAICS_7, NAICS_8, NAICS_9, NAICS_10.
getNAICSCrosswalk <- function(year) {
  # 2-6 digit
  NAICS_2to6 <- getNAICS2to6Digits(year)
  # 7-10 digit
  NAICS_7to10 <- getNAICS7to10Digits(year)
  NAICS_7to10$NAICS_6 <- substr(NAICS_7to10$NAICS_7, 1, 6)
  # Combine
  NAICS_2to10 <- merge(NAICS_2to6, NAICS_7to10, by = "NAICS_6", all.x = TRUE)
  # Re-order columns
  NAICS_2to10 <- NAICS_2to10[, paste("NAICS", c(2:10), sep = "_")]
  return(NAICS_2to10)
}

#' Get 2012 2-10 digit NAICS codes in a crosswalk format.
#' @param year int. 2012 or 2007 accepted.
#' @return data frame with columns NAICS_2, NAICS_3, NAICS_4, NAICS_5, NAICS_6,
#' NAICS_7, NAICS_8, NAICS_9, NAICS_10.
getNAICSCodeName <- function(year) {
  # 2-6 digit
  NAICSCodeName_2to6 <- getNAICS2to6DigitsCodeName(year)
  # 7-10 digit
  NAICSCodeName_7to10 <- getNAICS7to10DigitsCodeName(year)
  # Combine
  NAICSCodeName_2to10 <- rbind.data.frame(NAICSCodeName_2to6, NAICSCodeName_7to10)
  return(NAICSCodeName_2to10)
}

#' Get 2012 NAICS to 2007 NAICS concordances at 6-digit level.
#' @return data frame with columns '2012 NAICS Code', '2012 NAICS Title',
#' '2007 NAICS Code', and '2007 NAICS Title'.
getNAICS2012to2007Concordances <- function() {
  filename <- "inst/extdata/2012_to_2007_NAICS.xls"
  if(!file.exists(filename)) {
    utils::download.file("https://www.census.gov/naics/concordances/2012_to_2007_NAICS.xls",
                         filename, mode = "wb")
  }
  df <- as.data.frame(readxl::read_excel(filename, sheet = 1, col_names = TRUE, skip = 2))
  df <- df[, startsWith(colnames(df), "20")]
  return(df)
}

#' Get 2012 NAICS to 2017 NAICS concordances at 6-digit level.
#' @return data frame with columns '2012 NAICS Code', '2012 NAICS Title',
#' '2017 NAICS Code', and '2017 NAICS Title'.
getNAICS2012to2017Concordances <- function() {
  filename <- "inst/extdata/2012_to_2017_NAICS.xlsx"
  if(!file.exists(filename)) {
    utils::download.file("https://www.census.gov/naics/concordances/2012_to_2017_NAICS.xlsx",
                         filename, mode = "wb")
  }
  df <- as.data.frame(readxl::read_excel(filename, sheet = 1, col_names = TRUE, skip = 2))
  df <- df[, startsWith(colnames(df), "20")]
  return(df)
}
