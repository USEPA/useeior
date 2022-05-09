# Functions for handling data from stateior, https://github.com/USEPA/stateior

#' Load two-region IO data of model iolevel and year from user's local directory
#' or the EPA Data Commons.
#' @description Load two-region IO data of model iolevel and year from user's
#' local directory or the EPA Data Commons.
#' @param specs Model specifiations.
#' @param dataname Name of desired IO data, can be "Make", "Use", "DomesticUse",
#' "UseTransactions", "FinalDemand", "InternationalTradeAdjustment,
#' "DomesticUseTransactions", "DomesticFinalDemand",
#' "CommodityOutput, "IndustryOutput", and "DomesticUsewithTrade".
#' @return A list of two-region IO data of model iolevel and year.
getTwoRegionIOData <- function(specs, dataname) {
  # Define data file name
  filename <- paste("TwoRegion", specs$BaseIOLevel, dataname, specs$IOYear,
                    specs$IODataVersion, sep = "_")
  # Adjust filename to fit what is on the Data Commons
  if (dataname %in% c("UseTransactions", "FinalDemand")) {
    filename <- gsub(dataname, "Use", filename)
  } else if (dataname %in% c("DomesticUseTransactions", "DomesticFinalDemand")) {
    filename <- gsub(dataname, "DomesticUse", filename)
  }
  TwoRegionIOData <- loadDataCommonsfile(paste0("stateio/", filename, ".rds"))
  # Try loading data from local folder
  return(TwoRegionIOData)
}
