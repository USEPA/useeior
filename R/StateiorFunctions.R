# Functions for handling data from stateior, https://github.com/USEPA/stateior

#' Load two-region IO data of model iolevel and year from user's local directory
#' or the EPA Data Commons.
#' @description Load two-region IO data of model iolevel and year from user's
#' local directory or the EPA Data Commons.
#' @param model An EEIO form USEEIO model object with model specs and IO meta data loaded.
#' @param dataname Name of desired IO data, can be "Make", "Use", "DomesticUse",
#' "UseTransactions", "FinalDemand", "InternationalTradeAdjustment,
#' "DomesticUseTransactions", "DomesticFinalDemand",
#' "CommodityOutput, "IndustryOutput", and "DomesticUsewithTrade".
#' @return A list of two-region IO data of model iolevel and year.
getTwoRegionIOData <- function(model, dataname) {
  # Define state, year and iolevel
  if (!"US-DC" %in% model$specs$ModelRegionAcronyms) {
    state <- state.name[state.abb == gsub(".*-", "", model$specs$ModelRegionAcronyms[1])]
  } else {
    state <- "District of Columbia"
  }
  # Define data file name
  filename <- paste(lapply(c("TwoRegion", model$specs$BaseIOLevel, dataname,
                              model$specs$DisaggregationSpecs, model$specs$IOYear,
                              model$specs$IODataVersion),  
                  function(x) x[!is.na(x)]), collapse = "_")
  # Adjust filename to fit what is on the Data Commons
  if (dataname %in% c("UseTransactions", "FinalDemand")) {
    filename <- gsub(dataname, "Use", filename)
  } else if (dataname %in% c("DomesticUseTransactions", "DomesticFinalDemand")) {
    filename <- gsub(dataname, "DomesticUse", filename)
  } else if (dataname %in% c("DomesticUseTransactionswithTrade")){
    filename <- gsub(dataname, "DomesticUsewithTrade", filename)
  }
  # Load data
  TwoRegionIOData <- readRDS(loadDataCommonsfile(paste0("stateio/", filename, ".rds")))
  # Keep SoI and RoUS only
  TwoRegionIOData <- TwoRegionIOData[[state]]
  if (dataname %in% c("UseTransactions", "DomesticUseTransactions")) {
    TwoRegionIOData <- TwoRegionIOData[, !(colnames(TwoRegionIOData) 
                                           %in% model$FinalDemandMeta$Code_Loc)]
  } else if (dataname %in% c("FinalDemand", "DomesticFinalDemand")) {
    TwoRegionIOData <- TwoRegionIOData[, model$FinalDemandMeta$Code_Loc]
  } else if (dataname == "ValueAdded") {
    TwoRegionIOData <- TwoRegionIOData[model$ValueAddedMeta$Code_Loc, ]
  }
  return(TwoRegionIOData)
}

#' @description Disaggregate CPI table to ensure the correct dimensions
#' @param df, CPI table
#' @param model An EEIO form USEEIO model object with model specs and IO meta data loaded.
#' @return An expanded CPI table with values replicated for disaggregated sectors.
disaggregateCPI <- function(df, model){
  ## Find rows in IndustryCPI not in IndustryOutput, and duplicate them 
  sector_index <- !(rownames(df) %in% names(model$IndustryOutput))
  disagg_sectors <- rownames(df)[sector_index]
  
  numNewSectors <- (length(model$IndustryOutput) - nrow(df)) / 2
  for (row in disagg_sectors){
    originalIndex <- which(rownames(df)==row)
    originalRowVector <- df[originalIndex,]
    disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors + 1),,drop=FALSE]
    
    df <- rbind(df[1:originalIndex-1,,drop=FALSE],  #from 1st row to row right before disaggregation
                disaggRows,
                df[-(1:originalIndex),,drop=FALSE])
  }
  return(df)
}
