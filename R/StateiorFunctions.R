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
  filename <- paste("TwoRegion", model$specs$BaseIOLevel, dataname, model$specs$IOYear,
                    model$specs$IODataVersion, sep = "_")
  # Adjust filename to fit what is on the Data Commons
  if (dataname %in% c("UseTransactions", "FinalDemand")) {
    filename <- gsub(dataname, "Use", filename)
  } else if (dataname %in% c("DomesticUseTransactions", "DomesticFinalDemand")) {
    filename <- gsub(dataname, "DomesticUse", filename)
  }
  # Load data
  TwoRegionIOData <- readRDS(loadDataCommonsfile(paste0("stateio/", filename, ".rds")))
  # Keep SoI and RoUS only
  TwoRegionIOData <- TwoRegionIOData[[state]]
  if (dataname %in% c("UseTransactions", "DomesticUseTransactions")) {
    TwoRegionIOData <- TwoRegionIOData[model$Commodities$Code_Loc,
                                       model$Industries$Code_Loc]
  } else if (dataname %in% c("FinalDemand", "DomesticFinalDemand")) {
    TwoRegionIOData <- TwoRegionIOData[model$Commodities$Code_Loc,
                                       model$FinalDemandMeta$Code_Loc]
  } else if (dataname == "ValueAdded") {
    TwoRegionIOData <- TwoRegionIOData[model$ValueAddedMeta$Code_Loc,
                                       model$Industries$Code_Loc]
  }
  return(TwoRegionIOData)
}
