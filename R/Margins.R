#' Prepare M and N matrices with sector margin impacts
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param margin_type A character value: can be "intermediate" or "final consumer".
#' @export
#' @return A list with M_margin and N_margin
deriveFinalConsumerMarginSectorImpacts <- function(model) {
  # Load final consumer margins
  Margins <- model$FinalConsumerMargins
  ## Add in impacts of margin sectors
  # Calculate fractions (in absolute value) of producer price for each margin
  MarginCoefficients <- abs(as.matrix(Margins[, c("Transportation", "Wholesale", "Retail")]/Margins[, c("ProducersValue")]))
  rownames(MarginCoefficients) <- Margins$SectorCode
  
  # Allocate coefficients to margin sectors using margin allocation matrix
  all_margin_sectors <- model$MarginSectors$Code
  margin_allocation <- buildMarginAllocationMatrix(all_margin_sectors, model)
  margins_by_sector <- MarginCoefficients %*% margin_allocation
  
  # Put margins_by_sector into a matrix in the form of A
  A_margin <- model$A
  # Make sure sector ordering is the same
  A_margin[,] <- 0 
  for (s in all_margin_sectors) {
    A_margin[s, ] <- margins_by_sector[, s]
  }
  # Multiply M and N by margins_by_sector to derive M_margin and U_margin
  model$M_margin <- model$M %*% A_margin
  colnames(model$M_margin) <- tolower(paste(colnames(model$M_margin), model$specs$PrimaryRegionAcronym, sep = "/"))
  model$N_margin <- model$N %*% A_margin
  colnames(model$N_margin) <- tolower(paste(colnames(model$N_margin), model$specs$PrimaryRegionAcronym, sep = "/"))
  logging::loginfo("Model final consumer margin impacts derived")
  return(model)
}

#'Create margin_allocation matrix to allocate fractions by margin sector
#'Currently uses sector output to provide that allocation
#'@param all_margin_sectors, vector of sector codes
#'@param model, a fully built model
#'@return matrix, margin types x margin sectors with values being fractions of type to each sector
buildMarginAllocationMatrix <- function(all_margin_sectors, model) {
  
  margin_allocation <- matrix(nrow = 3, ncol = length(all_margin_sectors), 0)
  rownames(margin_allocation) <- colnames(MarginCoefficients)
  colnames(margin_allocation) <- all_margin_sectors
  # Assign allocation factors to margin sectors based on total Commodity output
  output_ratio <- calculateOutputRatio(model, output_type="Commodity")
  transportation_code <- model$MarginSectors[model$MarginSectors$Name=="Transportation", "Code"]
  wholesale_code <- model$MarginSectors[model$MarginSectors$Name=="Wholesale", "Code"]
  retail_code <- model$MarginSectors[model$MarginSectors$Name=="Retail", "Code"]
  margin_allocation["Transportation", transportation_code] <- output_ratio[output_ratio$SectorCode%in%transportation_code, "toSectorRatio"]
  margin_allocation["Wholesale", wholesale_code] <- output_ratio[output_ratio$SectorCode%in%wholesale_code, "toSectorRatio"]
  margin_allocation["Retail", retail_code] <- output_ratio[output_ratio$SectorCode%in%retail_code, "toSectorRatio"]
  return(margin_allocation)
  
}

#' Generate Margins table using either Industry Margins (BEA Margins) or Final Consumer Margins (BEA PCE and PEQ Bridge data).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param marginsource A character indicating the source of Margins, either "Industry" or "FinalConsumer".
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
  MarginsTable <- merge(MarginsTable, crosswalk, by.x = "CommodityCode", by.y = paste0("BEA_", model$specs$BaseIOLevel))
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

