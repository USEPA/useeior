#' Generate Margins table using BEA Margin Details table which include all industries and final demand.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A data.frame containing CommodityCode, and margins for ProducersValue, Transportation, Wholesale, Retail and PurchasersValue.
getMarginsTable <- function (model) {
  # Use BEA Margin Details table
  if (model$specs$BaseIOSchema==2012) {
    MarginsTable <- useeior::Detail_Margins_2012_BeforeRedef
  }
  # Remove Imports records
  MarginsTable <- MarginsTable[MarginsTable$NIPACode!="F05000", ]
  # Remove Scrap, Used and secondhand goods, and Rest of world adjustment commodities
  MarginsTable <- MarginsTable[!MarginsTable$CommodityCode%in%c("S00401", "S00402", "S00900"), ]
  # Convert negative values to non-negative
  value_columns <- c("ProducersValue", "Transportation", "Wholesale", "Retail")
  MarginsTable[, value_columns] <- abs(MarginsTable[, value_columns])
  # Map to Summary and Sector level
  crosswalk <- unique(model$crosswalk[startsWith(colnames(model$crosswalk), "BEA")])
  MarginsTable <- merge(MarginsTable, crosswalk, by.x = "CommodityCode", by.y = "BEA_Detail")
  # Aggregate value_columns by CommodityCode (dynamic to model BaseIOLevel) and CommodityDescription
  if (!model$specs$BaseIOLevel=="Detail") {
    MarginsTable$CommodityCode <- MarginsTable[, paste0("BEA_", model$specs$BaseIOLevel)]
  }
  MarginsTable <- stats::aggregate(MarginsTable[, value_columns], by = list(MarginsTable$CommodityCode), sum)
  colnames(MarginsTable)[1] <- "CommodityCode"
  # Keep model Commodities
  MarginsTable <- merge(MarginsTable, model$Commodities, by.x = "CommodityCode", by.y = "Code", all.y = TRUE)
  MarginsTable[is.na(MarginsTable)] <- 0
  MarginsTable <- MarginsTable[match(model$Commodities$Code, MarginsTable$CommodityCode), ]
  # Transform MarginsTable from Commodity to Industry format
  if (model$specs$CommoditybyIndustryType=="Industry") {
    # Generate a commodity x industry commodity mix matrix, see Miller and Blair section 5.3.2
    CommodityMix <- generateCommodityMixMatrix(model)
    MarginsTable_Industry <- model$Industries[, "Code", drop = FALSE]
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
  # Calculate Purchaser's value
  MarginsTable$PurchasersValue <- rowSums(MarginsTable[, c("ProducersValue", "Transportation", "Wholesale", "Retail")])
  # Rename code column from CommodityCode/IndustryCode to SectorCode
  colnames(MarginsTable)[1] <- "SectorCode"
  return(MarginsTable)
}

