#' Generate a dataframe containing US total consumption and production.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe of US total consumption and production.
getUSTotalConsProd <- function (model) {
  # Generate US total consumption and production
  USConsProd <- cbind.data.frame(rowSums(model$FinalDemand[, model$BEA$TotalConsumptionCodes]),
                                 as.matrix(rowSums(model$FinalDemand)))
  colnames(USConsProd) <- paste(model$specs$IOYear, c("_US_Consumption", "_US_Production"), sep = "")
  # Merge USConsProd with model$SectorNames
  USConsProd <- merge(model$SectorNames, USConsProd, by.x = "SectorCode", by.y = 0)
  ConsProdcolumns <- paste(model$specs$IOYear, "US", c("Consumption", "Production"), sep = "_")
  # Add Location column, re-order columns (drop useless columns), and modify colnames
  USConsProd$Location <- "US"
  USTotalConsProd <- USConsProd[c("SectorCode", "SectorName", "Location", ConsProdcolumns)]
  colnames(USTotalConsProd)[1:2] <- c("BEA_Code", "BEA_Name")
  # # Remove Scrap row if ScrapIncluded is TRUE in model configuration
  # if (model$specs$ScrapIncluded == TRUE) {
  #   USTotalConsProd <- USTotalConsProd[!USTotalConsProd$BEA_Code=="S00401", ]
  # }
  # Make BEA Detail Code to "factor"
  USTotalConsProd$BEA_Code <- as.factor(USTotalConsProd$BEA_Code)

  return(USTotalConsProd)
}

#