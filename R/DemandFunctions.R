#' Generate a dataframe containing US total consumption and production.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe of US total consumption and production.
getUSTotalConsProd <- function (model) {
  # Generate US total consumption and production
  USConsProd <- cbind.data.frame(rowSums(model$FinalDemand[, model$BEA$TotalConsumptionCodes]), model$f)
  colnames(USConsProd) <- paste(model$specs$IOYear, c("_US_Consumption", "_US_Production"), sep = "")
  # Attach Industry/Commodity Name to the model$IndustryCPI/model$CommodityCPI
  if (model$specs$CommoditybyIndustryType == "Commodity") {
    CPI <- cbind.data.frame(generatePriceAdjustedCommodityCPIforYear(model$specs$ReferenceCurrencyYear, model),
                            generatePriceAdjustedCommodityCPIforYear(model$specs$IOYear, model))
  } else {
    CPI <- model$GDP$BEACPIIO[, as.character(c(model$specs$ReferenceCurrencyYear, model$specs$IOYear))]
  }
  CPIwithName <- merge(model$SectorNames, model$GDP$BEACPIIO, by.x = "SectorCode", by.y = 0, all = TRUE)
  # Merge USConsProd with CPIwithName
  AdjustedUSConsProd <- merge(USConsProd, CPIwithName, by.x = 0, by.y = "SectorCode", all.x = TRUE)
  # Adjust consumption and production according to CPI of RefYear
  AdjustedUSConsProd$ReferenceCurrencyYeartoIOYearRatio <- AdjustedUSConsProd[, as.character(model$specs$ReferenceCurrencyYear)]/AdjustedUSConsProd[, as.character(model$specs$IOYear)]
  ConsProdcolumns <- paste(model$specs$IOYear, "US", c("Consumption", "Production"), sep = "_")
  AdjustedUSConsProd[, ConsProdcolumns] <- AdjustedUSConsProd[, ConsProdcolumns]*AdjustedUSConsProd$ReferenceCurrencyYeartoIOYearRatio
  # Add Location column, re-order columns (drop useless columns), and modify colnames
  AdjustedUSConsProd$Location <- "US"
  USTotalConsProd <- AdjustedUSConsProd[c("Row.names", "SectorName", "Location", ConsProdcolumns)]
  colnames(USTotalConsProd)[1:2] <- c("BEA_Code", "BEA_Name")
  # # Remove Scrap row if ScrapIncluded is TRUE in model configuration
  # if (model$specs$ScrapIncluded == TRUE) {
  #   USTotalConsProd <- USTotalConsProd[!USTotalConsProd$BEA_Code=="S00401", ]
  # }
  # Make BEA Detail Code to "factor"
  USTotalConsProd$BEA_Code <- as.factor(USTotalConsProd$BEA_Code)

  return(USTotalConsProd)
}

