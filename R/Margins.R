#' Generate Margins table using either Industry Margins (BEA Margins) or Final Consumer Margins (BEA PCE and PEQ Bridge data).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe containing CommodityCode, and margins for ProducersValue, Transportation, Wholesale, Retail and PurchasersValue.
getFinalConsumerMarginsTable <- function (model) {
  # Use PCE and PEQ Bridge tables
  if (model$specs$BaseIOSchema==2012) {
    PCE <- useeior::Detail_PCE_2012[, 3:9]
    PEQ <- useeior::Detail_PEQ_2012[, 3:9]
    MarginsTable <- rbind(PCE, PEQ)
  }
  # Map to Summary and Sector level
  crosswalk <- unique(model$crosswalk[startsWith(colnames(model$crosswalk), "BEA")])
  MarginsTable <- merge(MarginsTable, crosswalk, by.x = "CommodityCode", by.y = "BEA_Detail")
  # Aggregate by CommodityCode (dynamic to model BaseIOLevel) and CommodityDescription
  if (!model$specs$BaseIOLevel=="Detail") {
    MarginsTable$CommodityCode <- MarginsTable[, paste0("BEA_", model$specs$BaseIOLevel)]
  }
  value_columns <- c("ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
  MarginsTable <- stats::aggregate(MarginsTable[, value_columns], by = list(MarginsTable$CommodityCode), sum)
  colnames(MarginsTable)[1] <- "CommodityCode"
  # Keep the Commodities specified in model
  MarginsTable <- merge(MarginsTable, model$Commodities, by.x = "CommodityCode", by.y = "Code", all.y = TRUE)
  MarginsTable[is.na(MarginsTable)] <- 0
  MarginsTable <- MarginsTable[match(model$Commodities$Code, MarginsTable$CommodityCode), ]
  # Transform MarginsTable from Commodity to Industry format
  if (model$specs$CommoditybyIndustryType=="Industry") {
    # Generate a commodity x industry commodity mix matrix, see Miller and Blair section 5.3.2
    CommodityMix <- generateCommodityMixMatrix(model)
    MarginsTable_Industry <- as.data.frame(model$Industries)
    colnames(MarginsTable_Industry) <- "IndustryCode"
    # Transform ProducerValue from Commodity to Industry format
    # ! Not transforming Transportation, Wholesale and Retail to Industry format now
    MarginsTable_Industry[, "ProducersValue"] <- as.vector(MarginsTable[, "ProducersValue"]%*%CommodityMix)
    # Merge Industry Margins Table with Commodity Margins Table
    MarginsTable <- merge(MarginsTable_Industry, MarginsTable[, -which(names(MarginsTable)=="ProducersValue")],
                          by.x = "IndustryCode", by.y = "CommodityCode", all.x = TRUE)
    # Replace NA with zero
    MarginsTable[is.na(MarginsTable)] <- 0
  }
  MarginsTable$PurchasersValue <- rowSums(MarginsTable[, c("ProducersValue", "Transportation", "Wholesale", "Retail")])
  # Rename code column from CommodityCode/IndustryCode to SectorCode
  colnames(MarginsTable)[1] <- "SectorCode"
  return(MarginsTable)
}

