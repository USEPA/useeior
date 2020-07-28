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
  # Get BEA IO tables
  model$BEA <- loadBEAtables(model$specs)
  # Get model$Industries and model$Commodities
  if (model$specs$ModelType=="US") {
    model$Commodities <- model$BEA$Commodities
    model$Industries <- model$BEA$Industries
  } else if (model$specs$ModelType=="State2R") {
    # Fork for state model here
  }
  
  # Get model$Make, model$Use, model$MakeTransactions, model$UseTransactions, and model$UseValueAdded
  model$Make <- model$BEA$Make
  model$Use <- model$BEA$Use
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
  # Get model$CommodityOutput, model$CommodityCPI, model$IndustryOutput, model$IndustryCPI, and model$FinalDemand
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    if (model$specs$PrimaryRegionAcronym=="US") {
      model$CommodityOutput <- generateCommodityOutputforYear(model$specs$PrimaryRegionAcronym, IsRoUS = FALSE, model)
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
  colnames(model$SectorNames) <- c("SectorCode", "SectorName")
  # Get model$IntermediateMargins and model$FinalConsumerMargins
  model$IntermediateMargins <- getMarginsTable(model, "intermediate")
  model$FinalConsumerMargins <- getMarginsTable(model, "final consumer")
  
  # Check for disaggregation
  if(!is.null(model$specs$disaggregation)){
    #! TO DO - point to DisaggregationFunctions.R
    
  }
  
  return(model)
}


#' Load BEA IO tables in a list based on model config.
#' @param specs Model specifications.
#' @return A list with BEA IO tables.
loadBEAtables <- function(specs) {
  BEA <- list()
  logging::loginfo("Initializing IO tables...")

  # Load BEA schema_info based on model BEA
  SchemaInfoFile <- paste0(specs$BaseIOSchema, "_", specs$BaseIOLevel, "_Schema_Info.csv")
  SchemaInfo <- utils::read.table(system.file("extdata", SchemaInfoFile, package = "useeior"),
                                  sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Extract desired columns from SchemaInfo, return vectors with strings of codes
  getVectorOfCodes <- function(colName) {
    return(as.vector(stats::na.omit(SchemaInfo[, c("Code", colName)])[, "Code"]))
  }

  # Get BEA sectors by group
  BEA$Commodities <-getVectorOfCodes("Commodity")
  BEA$Industries <- getVectorOfCodes("Industry")
  BEA$ValueAddedCodes <- getVectorOfCodes("ValueAdded")
  BEA$HouseholdDemandCodes <- getVectorOfCodes("HouseholdDemand")
  BEA$InvestmentDemandCodes <- getVectorOfCodes("InvestmentDemand")
  BEA$ChangeInventoriesCodes <- getVectorOfCodes("ChangeInventories")
  BEA$ImportCodes <- getVectorOfCodes("Import")
  BEA$ExportCodes <- getVectorOfCodes("Export")
  BEA$GovernmentDemandCodes <- getVectorOfCodes("GovernmentDemand")
  BEA$FinalDemandCodes <- c(BEA$HouseholdDemandCodes, BEA$InvestmentDemandCodes, BEA$ChangeInventoriesCodes, BEA$ImportCodes, BEA$ExportCodes, BEA$GovernmentDemandCodes)
  BEA$TotalConsumptionCodes <- c(BEA$HouseholdDemandCodes, BEA$InvestmentDemandCodes, BEA$GovernmentDemandCodes)
  BEA$ScrapCodes <- getVectorOfCodes("Scrap")
  BEA$TransportationCodes <- getVectorOfCodes("Distribution")
  BEA$WholesaleCodes <- getVectorOfCodes("Wholesale")
  BEA$RetailCodes <- getVectorOfCodes("Retail")
  

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
  if (specs$BaseIOLevel=="Sector") {
    Import <- get(paste("Summary_Import", specs$IOYear, "BeforeRedef", sep = "_"))
    # Aggregate ImportTrasactions from Summary to Sector
    ImportTrasactions <- Import[useeior::Summary_CommodityCodeName_2012$BEA_2012_Summary_Commodity_Code,
                                useeior::Summary_IndustryCodeName_2012$BEA_2012_Summary_Industry_Code] * 1E6 # data frame, values are in dollars ($)
    BEA$ImportTransactions <- as.data.frame(aggregateMatrix(as.matrix(ImportTrasactions), "Summary", "Sector", specs))[BEA$Commodities, BEA$Industries]
    # Aggregate ImportFinalDemand from Summary to Sector
    SchemaInfo <- utils::read.table(system.file("extdata", "2012_Summary_Schema_Info.csv", package = "useeior"),
                                    sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    SummaryFinalDemandCodes <- c(getVectorOfCodes("HouseholdDemand"), getVectorOfCodes("InvestmentDemand"), getVectorOfCodes("ChangeInventories"),
                                 getVectorOfCodes("Import"), getVectorOfCodes("Export"), getVectorOfCodes("GovernmentDemand"))
    ImportFinalDemand <- Import[useeior::Summary_CommodityCodeName_2012$BEA_2012_Summary_Commodity_Code, SummaryFinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
    BEA$ImportFinalDemand <- as.data.frame(aggregateMatrix(as.matrix(ImportFinalDemand), "Summary", "Sector", specs))[BEA$Commodities, BEA$FinalDemandCodes]
  } else {
    # Extract ImportTransactions from Import matrix
    Import <- get(paste(specs$BaseIOLevel, "Import", specs$IOYear, Redef, sep = "_"))
    BEA$ImportTransactions <- Import[BEA$Commodities, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
    # Extract ImportFinalDemand from Import matrix
    BEA$ImportFinalDemand <- Import[BEA$Commodities, BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  }
  BEA$DomesticUseTransactions <- BEA$UseTransactions - BEA$ImportTransactions
  BEA$DomesticFinalDemand <- BEA$UseFinalDemand - BEA$ImportFinalDemand
  # For Detail model, set used goods (S00402) and noncomparable imports (S00300) in UseCommodityOutput to 1 to prevent dividing by zero when creating market shares
  if (specs$BaseIOLevel == "Detail") {
    BEA$UseCommodityOutput[c("S00402", "S00300"), ] <- 1
  }
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



