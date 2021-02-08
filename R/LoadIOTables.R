#' Prepare economic components of an EEIO form USEEIO model.
#' @param modelname Name of the model from a config file.
#' @return A list with USEEIO model economic components.
#' @export
loadIOData <- function(modelname) {
  startLogging()
  logging::loginfo('Begin model initialization...')
  model <- list()
  # Get model specs
  model$specs <- getModelConfiguration(modelname)
  # Get model crosswalk
  model$crosswalk <- get(paste0("MasterCrosswalk", model$specs$BaseIOSchema))
  model$crosswalk <- unique(model$crosswalk[, c("NAICS_2012_Code", colnames(model$crosswalk)[startsWith(colnames(model$crosswalk), "BEA")])])
  colnames(model$crosswalk) <- gsub(paste0("_", model$specs$BaseIOSchema, "|_Code"), "", colnames(model$crosswalk))
  # Get BEA IO tables
  model$BEA <- loadBEAtables(model$specs)
  # Get model$Industries and model$Commodities
  if (model$specs$ModelType=="US") {
    model$Commodities <- model$BEA$Commodities
    model$Industries <- model$BEA$Industries
  } else if (model$specs$ModelType=="State2R") {
    # Fork for state model here
  }
  
  # Get model$MakeTransactions, model$UseTransactions, and model$UseValueAdded
  model$MakeTransactions <- model$BEA$MakeTransactions
  model$UseTransactions <- model$BEA$UseTransactions
  model$DomesticUseTransactions <- model$BEA$DomesticUseTransactions
  model$UseValueAdded <- model$BEA$UseValueAdded
  # Get GDP tables
  model$GDP <- loadGDPtables(model$specs)
  # Replace Gross Output with Industry Output from Make if modellevel is "Detail"
  if (model$specs$BaseIOLevel=="Detail") {
    MakeIndustryOutput <- model$BEA$MakeIndustryOutput
    MakeIndustryOutput <- MakeIndustryOutput[rownames(model$GDP$BEAGrossOutputIO), colnames(MakeIndustryOutput), drop = FALSE]
    model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear)] <- MakeIndustryOutput[, 1]
  }
  # Get Commodity/Industry Output, Commodity/Industry CPI, FinalDemand
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    if (model$specs$PrimaryRegionAcronym=="US") {
      model$CommodityOutput <- generateCommodityOutputforYear(model)
      # Change commodity output of 'customs duties' (4200ID/42/42) to 0
      if (model$specs$BaseIOLevel=="Detail") {
        model$CommodityOutput["4200ID", ] <- 0
      }
    } else {
      # Add RoUS in CommodityOutput table
      model$CommodityOutput <- getStateCommodityOutputEstimates(model$specs$PrimaryRegionAcronym)
    }
    model$CPI <- generateCommodityCPIforYear(model$specs$IOYear, model) # return a one-column table for IOYear
    # Get model$FinalDemand
    model$FinalDemand <- model$BEA$UseFinalDemand
    # Get model$DomesticFinalDemand
    model$DomesticFinalDemand <- model$BEA$DomesticFinalDemand
  } else {
    # Get model$IndustryOutput from GDP tables
    if (model$specs$PrimaryRegionAcronym=="US") {
      model$IndustryOutput <- model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE]
    } else {
      # Add RoUS in IndustryOutput table
      model$IndustryOutput <- getStateIndustryOutput(model$specs$PrimaryRegionAcronym)
    }
    # Get model$IndustryCPI from GDP tables
    model$CPI <- model$GDP$BEACPIIO[, as.character(model$specs$IOYear), drop = FALSE]
    # Transform model$BEA$UseFinalDemand with MarketShares
    model$FinalDemand <- transformFinalDemandwithMarketShares(model$BEA$UseFinalDemand, model)#This output needs to be tested - producing strange results
    # Transform model$BEA$DomesticFinalDemand with MarketShares
    model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(model$BEA$DomesticFinalDemand, model)#This output needs to be tested - producing strange results
  }
  # Get model$SectorNames
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    USEEIONames <- utils::read.table(system.file("extdata", "USEEIO_Commodity_Code_Name.csv", package = "useeior"),
                                     sep = ",", header = TRUE, stringsAsFactors = FALSE)
    model$SectorNames <- merge(as.data.frame(model$Commodities, stringsAsFactors = FALSE), USEEIONames,
                               by.x = "model$Commodities", by.y = "Code", all.x = TRUE, sort = FALSE)
  } else {
    model$SectorNames <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
  }
  colnames(model$SectorNames) <- c("Sector", "SectorName")
  # Get model$IntermediateMargins and model$FinalConsumerMargins
  model$IntermediateMargins <- getMarginsTable(model, "intermediate")
  model$FinalConsumerMargins <- getMarginsTable(model, "final consumer")
  
  # Check for disaggregation
  if(!is.null(model$specs$DisaggregationSpecs)){
    model <- disaggregateModel(model)
      }
  
  return(model)
}


#' Load BEA IO tables in a list based on model config.
#' @param specs Model specifications.
#' @return A list with BEA IO tables.
loadBEAtables <- function(specs) {
  BEA <- list()
  logging::loginfo("Initializing IO tables...")

  # Get BEA sectors by group
  BEA$Commodities <-getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Commodity")
  BEA$Industries <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Industry")
  BEA$ValueAddedCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "ValueAdded")
  BEA$HouseholdDemandCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "HouseholdDemand")
  BEA$InvestmentDemandCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "InvestmentDemand")
  BEA$ChangeInventoriesCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "ChangeInventories")
  BEA$ExportCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Export")
  BEA$ImportCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Import")
  BEA$GovernmentDemandCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "GovernmentDemand")
  BEA$FinalDemandCodes <- c(BEA$HouseholdDemandCodes, BEA$InvestmentDemandCodes,
                            BEA$ChangeInventoriesCodes, BEA$ExportCodes,
                            BEA$ImportCodes, BEA$GovernmentDemandCodes)
  BEA$TotalConsumptionCodes <- c(BEA$HouseholdDemandCodes, BEA$InvestmentDemandCodes,
                                 BEA$GovernmentDemandCodes)
  BEA$ScrapCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Scrap")
  BEA$TransportationCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Distribution")
  BEA$WholesaleCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Wholesale")
  BEA$RetailCodes <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Retail")

  # Load pre-saved Make and Use tables
  Redef <- ifelse(specs$BasewithRedefinitions, "AfterRedef", "BeforeRedef")
  BEA$Make <- get(paste(specs$BaseIOLevel, "Make", specs$IOYear, Redef, sep = "_"))
  BEA$Use <-  get(paste(specs$BaseIOLevel, "Use", specs$IOYear, specs$BasePriceType, Redef, sep = "_"))
  
  # Separate Make and Use tables into specific tables
  BEA$MakeTransactions <- BEA$Make[BEA$Industries, BEA$Commodities] * 1E6 # data frame, values are in dollars ($)
  BEA$MakeIndustryOutput <- as.data.frame(rowSums(BEA$MakeTransactions)) # data frame, values are in dollars ($)
  BEA$UseTransactions <- BEA$Use[BEA$Commodities, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
  BEA$UseFinalDemand <- BEA$Use[BEA$Commodities, BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  BEA$UseValueAdded <- BEA$Use[BEA$ValueAddedCodes, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
  BEA$UseCommodityOutput <- as.data.frame(rowSums(cbind(BEA$UseTransactions, BEA$UseFinalDemand))) # data frame, values are in dollars ($)
  # Generate domestic Use transaction and final demand
  DomesticUse <- generatDomesticUse(cbind(BEA$UseTransactions, BEA$UseFinalDemand), specs)
  BEA$DomesticUseTransactions <- DomesticUse[, BEA$Industries]
  BEA$DomesticFinalDemand <- DomesticUse[, BEA$FinalDemandCodes]
  # Replace NA with 0 in IO tables
  if(specs$BaseIOSchema==2007){
    BEA$MakeTransactions[is.na(BEA$MakeTransactions)] <- 0
    BEA$UseTransactions[is.na(BEA$UseTransactions)] <- 0
    BEA$UseFinalDemand[is.na(BEA$UseFinalDemand)] <- 0
  }
  
  return(BEA)
}


#' Externalize the BEA make and use tables
#' @param iolevel "Detail", "Sector", or "Summary" level
#' @param makeoruse Select either "Make" or "Use"
#' @param year If iolevel = "Detail", then has to be 2012, otherwise select between 2010--2018
#' @param redef Select either "BeforeRedef" or "AfterRedef"
#' @return A dataframe of BEA make or use tables
#' @export
loadBEAMakeorUseTable <- function (iolevel, makeoruse, year, redef){
  
  if(makeoruse == "Make"){
    filename <- paste(iolevel, "_", makeoruse, "_", year, "_", redef, sep = "")
    BEA_Table <- get(filename)
  } else {
    filename <- paste(iolevel, "_", makeoruse, "_", year, "_PRO_", redef, sep = "")
    BEA_Table <- get(filename)
  }
  
  return(BEA_Table)
}



