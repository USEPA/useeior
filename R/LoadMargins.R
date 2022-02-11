# Functions to load margins data

#' Generate Margins table using BEA Margin Details table which include all industries and final demand.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A data.frame containing CommodityCode, and margins for ProducersValue, Transportation, Wholesale, Retail and PurchasersValue.
getMarginsTable <- function (model) {
  # Define value_columns in Margins table
  value_columns <- c("ProducersValue", "Transportation", "Wholesale", "Retail")
  # Use BEA Margin Details table
  if (model$specs$BaseIOSchema==2012) {
    MarginsTable <- useeior::Detail_Margins_2012_BeforeRedef
    MarginsTable[, value_columns] <- MarginsTable[, value_columns]*1E6
  }
  # Remove Export, Import and Change in Inventory records.
  # Exports do not reflect what a US consumer would pay for margins, hence the removal.
  # Imports have negative PRO price which impacts calculations. 
  # Change in inventory has negative margins for positive change, which does not accurately portray actual margins either.
  purchaser_removal <- sapply(list("Export", "Import", "ChangeInventories"), getVectorOfCodes,
                              ioschema = model$specs$BaseIOSchema, iolevel = "Detail")
  MarginsTable <- MarginsTable[!MarginsTable$NIPACode%in%purchaser_removal,]
  # Remove Scrap, Used and secondhand goods, and Non-comparable imports, and Rest of world adjustment commodities
  commodity_removal <- sapply(list("Scrap", "UsedGoods", "NonComparableImport", "RoWAdjustment"), getVectorOfCodes,
                              ioschema = model$specs$BaseIOSchema, iolevel = model$specs$BaseIOLevel)
  MarginsTable <- MarginsTable[!MarginsTable$CommodityCode%in%commodity_removal, ]
  # Convert negative PRO values to non-negative
  # This addresses remaining negative PRO values for cases like subsidies
  MarginsTable[, "ProducersValue"] <- abs(MarginsTable[, "ProducersValue"])
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
  MarginsTable <- merge(MarginsTable, model$Commodities[,c("Code","Name", "Code_Loc")], by.x = "CommodityCode", by.y = "Code", all.y = TRUE)
  MarginsTable[is.na(MarginsTable)] <- 0
  MarginsTable <- MarginsTable[match(model$Commodities$Code_Loc, MarginsTable$Code_Loc), ]
  # Transform MarginsTable from Commodity to Industry format
  if (model$specs$CommodityorIndustryType=="Industry") {
    # Generate a commodity x industry commodity mix matrix, see Miller and Blair section 5.3.2
    CommodityMix <- generateCommodityMixMatrix(model)
    #Create a margins table for industries based on model industries
    MarginsTable_Industry <- model$Industries[, "Code", drop = FALSE]
    colnames(MarginsTable_Industry) <- "IndustryCode"

    # Transform PRO value and Margins for Commodities from Commodity to Industry format, (Margins' * C_m )'
    Margins_values_com <- as.matrix(MarginsTable[, value_columns])
    Margins_values_ind <- t(t(Margins_values_com) %*% CommodityMix )
  
    # Merge Industry Margins Table with Commodity Margins Table to add in metadata columns
    MarginsTable_Industry[, value_columns] <- Margins_values_ind 
    MarginsTable <- merge(MarginsTable_Industry, MarginsTable[, -which(names(MarginsTable)%in%value_columns)],
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

