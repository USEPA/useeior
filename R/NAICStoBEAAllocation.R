#' Determine allocation factors between NAICS and BEA sectors based on IO output.
#' @param year Year of model Industry output.
#' @return A table of allocation factors between NAICS and BEA sectors.
getNAICStoBEAAllocation <- function (year) {
  # Keep USEEIO and NAICS columns in MasterCrosswalk2012 table based on the model specs
  NAICStoBEA <- MasterCrosswalk2012[, c(paste("NAICS", model$specs$BaseIOSchema, "Code", sep = "_"),
                                        paste("BEA", model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Code", sep = "_"))]
  colnames(NAICStoBEA) <- c("NAICS_Code", "BEA_Code")
  # Drop 2-digit NAICS code
  NAICStoBEA <- NAICStoBEA[nchar(NAICStoBEA$NAICS_Code) > 2, ]
  # Drop duplicated NAICStoBEA rows
  NAICStoBEA <- unique(NAICStoBEA)
  # Select the repeated NAICS codes that need allocation
  AllocationCodes <- NAICStoBEA[duplicated(NAICStoBEA$NAICS_Code) | duplicated(NAICStoBEA$NAICS_Code, fromLast = TRUE), ]
  # Merge AllocationCodes with Gross Output table to calculate allocation factors
  AllocationTable <- merge(AllocationCodes, model$IndustryOutput[, as.character(year), drop = FALSE], by.x = "BEA_Code", by.y = 0, all.x = TRUE)
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
