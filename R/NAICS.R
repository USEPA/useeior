#' Determine allocation factors between NAICS and BEA sectors based on IO output.
#' @param year Year of model Industry output.
#' @return A table of allocation factors between NAICS and BEA sectors.
getNAICStoBEAAllocation <- function (year) {
  # Keep USEEIO and NAICS columns in MasterCrosswalk2012 table based on the model specs
  NAICStoBEA <- unique(MasterCrosswalk2012[, c(paste("NAICS", model$specs$BaseIOSchema, "Code", sep = "_"),
                                               paste("BEA", model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Code", sep = "_"))])
  colnames(NAICStoBEA) <- c("NAICS_Code", "BEA_Code")
  # Drop 2-digit NAICS code
  NAICStoBEA <- NAICStoBEA[nchar(NAICStoBEA$NAICS_Code) > 2, ]
  # Select the repeated NAICS codes that need allocation
  AllocationCodes <- NAICStoBEA[duplicated(NAICStoBEA$NAICS_Code) | duplicated(NAICStoBEA$NAICS_Code, fromLast = TRUE), ]
  # Merge AllocationCodes with Gross Output table to calculate allocation factors
  AllocationTable <- merge(AllocationCodes, model$GDP$BEAGrossOutputIO[, as.character(year), drop = FALSE], by.x = "BEA_Code", by.y = 0, all.x = TRUE)
  colnames(AllocationTable)[3] <- "Output"
  # Insert placeholders for NAs in the "Output" column
  AllocationTable[is.na(AllocationTable)] <- 1
  # Aggregate Output for the same NAICS code
  sum_temp <- stats::aggregate(AllocationTable$Output, by = list(AllocationTable$NAICS_Code), sum)
  colnames(sum_temp) <- c("NAICS_Code", "SumOutput")
  AllocationTable <- merge(AllocationTable, sum_temp, by="NAICS_Code", all.x = TRUE)
  # Calculate allocation factors
  AllocationTable$allocation_factor <- AllocationTable$Output/AllocationTable$SumOutput
  # Keep wanted columns
  AllocationTable <- AllocationTable[, c("NAICS_Code", "BEA_Code", "allocation_factor")]

  return(AllocationTable)
}


#' Get NAICS code names for year specified.
#' @param year int. 2012 or 2007 accepted.
#' @return dataframe with columns NAICS_year_Code and NAICS_year_Name.
#' @export 
getNAICSCodeName <- function (year) {
  if (year == 2012) {
    # Download the 2-6 digits NAICS table
    FileName <- "2-digit_2012_Codes.xls"
    if(!file.exists(FileName)) {
      download.file("https://www.census.gov/eos/www/naics/2012NAICS/2-digit_2012_Codes.xls", FileName, mode = "wb")
    }
    NAICS <- as.data.frame(readxl::read_excel(FileName, sheet = 1, col_names = TRUE))[-1,-1]
  } else { #year = 2007
    # Download the 2-6 digits NAICS table
    FileName <- "naics07.xls"
    if(!file.exists(FileName)) {
      download.file("https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07.xls", FileName, mode = "wb")
    }
    NAICS <- as.data.frame(readxl::read_excel("naics07.xls", sheet = 1, col_names = TRUE))[-1,-1]
  }
  colnames(NAICS) <- c("NAICS_Code", "NAICS_Name")
  # Split the NAICS code with dash ("-)
  DashSplit <- do.call("rbind.data.frame", apply(do.call("rbind", strsplit(NAICS$NAICS_Code, "-")), 1, function(x) seq(x[1], x[2], 1)))
  colnames(DashSplit) <- c(paste("V", 1:ncol(DashSplit), sep=""))
  # Merge back with NAICS
  NAICSCodeName <- cbind(NAICS, DashSplit)
  # Reshape to long table
  NAICSCodeName <- reshape2::melt(NAICSCodeName[, -1], id.vars = "NAICS_Name")
  # Drop unwanted column and duplicated rows
  NAICSCodeName$variable <- NULL
  NAICSCodeName$value <- as.integer(NAICSCodeName$value)
  NAICSCodeName <- unique(NAICSCodeName)
  # Rename columns
  colnames(NAICSCodeName) <- c(paste("NAICS_", year, "_Name", sep = ""), paste("NAICS_", year, "_Code", sep = ""))
  
  return(NAICSCodeName)
}

#' Gets NAICS codes from 2 to 6 digits.
#' @param year int, 2012 or 2007 accepted.
#' @return data frame with columns NAICS_2, NAICS_3, NAICS_4, NAICS_5, NAICS_6.
#' @export
getNAICS2to6Digits <- function (year) {
  if (year == 2012) {
    # Download the 2-6 digits NAICS table
    FileName <- "2-digit_2012_Codes.xls"
    if(!file.exists(FileName)) {
      download.file("https://www.census.gov/eos/www/naics/2012NAICS/2-digit_2012_Codes.xls", FileName, mode = "wb")
    }
    NAICS <- as.data.frame(readxl::read_excel(FileName, sheet = 1, col_names = TRUE))[-1,-1]
  } else { #year = 2007
    # Download the 2-6 digits NAICS table
    FileName <- "naics07.xls"
    if(!file.exists(FileName)) {
      download.file("https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07.xls", FileName, mode = "wb")
    }
    NAICS <- as.data.frame(readxl::read_excel("naics07.xls", sheet = 1, col_names = TRUE))[-1,-1]
  }
  colnames(NAICS) <- c("NAICS_Code", "NAICS_Name")
  NAICS$NAICS_Code <- as.integer(NAICS$NAICS_Code)
  NAICS <- NAICS[!is.na(NAICS$NAICS_Code), ]
  # Reshape the table
  NAICSwide <- as.data.frame(NAICS[nchar(NAICS$NAICS_Code)==6, ])
  NAICSwide[,c("NAICS_2", "NAICS_3", "NAICS_4", "NAICS_5")] <- cbind(substr(NAICSwide$NAICS_Code, 1, 2), substr(NAICSwide$NAICS_Code, 1, 3),
                                                                     substr(NAICSwide$NAICS_Code, 1, 4), substr(NAICSwide$NAICS_Code, 1, 5))
  NAICSwide$NAICS_6 <- NAICSwide$NAICS_Code
  NAICSwide <- NAICSwide[, -c(1:2)]
  
  return(NAICSwide)
}

#' Gets 2012 NAICS codes from 7 to 12 digits in the form of a crosswalk.
#' @return data frame with columns NAICS_year_Code and NAICS_year_Name.
#' @export
get2012NAICS7to12Digits <- function () {
  # Download Census 2012 Numerical List of Manufactured and Mineral Products
  SectorList <- c(211, 212, 213, 311, 312, 313, 314, 315, 316, 321, 322, 323, 324, 325, 326, 327, 331, 332, 333, 334, 335, 336, 337, 339)
  CensusNAICSList <- list()
  td <- tempdir()
  tf <- tempfile(tmpdir = tempdir(), fileext = ".csv")
  for(sector in SectorList) {
    download.file(paste("https://www.census.gov/manufacturing/numerical_list/", sector, ".xls", sep = ""), tf, mode = "wb")
    CensusNAICSList[[sector]] <- as.data.frame(readxl::read_excel(tf, sheet = 1, col_names = TRUE, skip = 2))[, 1:2]
    colnames(CensusNAICSList[[sector]]) <- c("NAICS_2012_Code", "NAICS_2012_Name")
  }
  CensusNAICS <- do.call(rbind, CensusNAICSList)
  
  return(CensusNAICS)
}