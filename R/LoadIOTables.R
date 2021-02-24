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
  # Get BEA IO tables and GDP tables
  model$BEA <- loadBEAtables(model$specs)
  model$GDP <- loadGDPtables(model$specs)
  # Declare model IO objects
  if (model$specs$ModelType=="US") {
    model$Industries <- toupper(apply(cbind(model$BEA$Industries, model$specs$PrimaryRegionAcronym), 1, FUN = joinStringswithSlashes))
    model$Commodities <- toupper(apply(cbind(model$BEA$Commodities, model$specs$PrimaryRegionAcronym), 1, FUN = joinStringswithSlashes))
    
    model$MakeTransactions <- model$BEA$MakeTransactions
    model$UseTransactions <- model$BEA$UseTransactions
    model$DomesticUseTransactions <- model$BEA$DomesticUseTransactions
    model$UseValueAdded <- model$BEA$UseValueAdded
    model$FinalDemand <- model$BEA$UseFinalDemand
    model$DomesticFinalDemand <- model$BEA$DomesticFinalDemand
    ## Modify row and column names in the IO tables
    # Use model$Industries
    rownames(model$MakeTransactions) <- colnames(model$UseTransactions) <- colnames(model$DomesticUseTransactions) <-
      colnames(model$UseValueAdded) <- model$Industries
    # Use model$Commodities
    colnames(model$MakeTransactions) <- rownames(model$UseTransactions) <- rownames(model$DomesticUseTransactions) <- 
      rownames(model$FinalDemand) <- rownames(model$DomesticFinalDemand) <- model$Commodities
    # Apply joinStringswithSlashes based on original row/column names
    rownames(model$UseValueAdded) <- toupper(apply(cbind(rownames(model$UseValueAdded), model$specs$PrimaryRegionAcronym),
                                                   1, FUN = joinStringswithSlashes))
    colnames(model$FinalDemand) <- colnames(model$DomesticFinalDemand) <- toupper(apply(cbind(colnames(model$FinalDemand),
                                                                                              model$specs$PrimaryRegionAcronym),
                                                                                        1, FUN = joinStringswithSlashes))
    
    model$IndustryOutput <- colSums(model$UseTransactions) + colSums(model$UseValueAdded)
    model$CommodityOutput <- rowSums(model$UseTransactions) + rowSums(model$FinalDemand)
    
    model$MultiYearIndustryOutput <- model$GDP$BEAGrossOutputIO[model$BEA$Industries, ]
    rownames(model$MultiYearIndustryOutput) <- model$Industries
    model$MultiYearIndustryOutput[, as.character(model$specs$IOYear)] <- model$IndustryOutput
    # Transform multi-year industry output to commodity output
    model$MultiYearCommodityOutput <- as.data.frame(model$CommodityOutput)[, FALSE]
    for (year_col in colnames(model$MultiYearIndustryOutput)) {
      model$MultiYearCommodityOutput[, year_col] <- transformIndustryOutputtoCommodityOutputforYear(as.numeric(year_col), model)
    }
    model$MultiYearCommodityOutput[, as.character(model$specs$IOYear)] <- model$CommodityOutput
    
    model$MultiYearIndustryCPI <- model$GDP$BEACPIIO[model$BEA$Industries, ]
    rownames(model$MultiYearIndustryCPI) <- model$Industries
    # Transform industry CPI to commodity CPI
    model$MultiYearCommodityCPI <- as.data.frame(model$MultiYearIndustryCPI)[, FALSE]
    for (year_col in colnames(model$MultiYearIndustryCPI)) {
      model$MultiYearCommodityCPI[, year_col] <- transformIndustryCPItoCommodityCPIforYear(as.numeric(year_col), model)
    }
    
    # Transform model objects from by-industry to by-commodity, or vice versa
    if (model$specs$CommoditybyIndustryType=="Commodity") {
      # Get model$SectorNames
      USEEIONames <- utils::read.table(system.file("extdata", "USEEIO_Commodity_Code_Name.csv", package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
      model$SectorNames <- merge(as.data.frame(model$BEA$Commodities, stringsAsFactors = FALSE), USEEIONames,
                                 by.x = "model$BEA$Commodities", by.y = "Code", all.x = TRUE, sort = FALSE)
    } else {
      # Transform model$BEA$UseFinalDemand with MarketShares
      model$FinalDemand <- transformFinalDemandwithMarketShares(model$FinalDemand, model)#This output needs to be tested - producing strange results
      # Transform model$BEA$DomesticFinalDemand with MarketShares
      model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(model$DomesticFinalDemand, model)#This output needs to be tested - producing strange results
      # Get model$SectorNames
      model$SectorNames <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
    }
    colnames(model$SectorNames) <- c("Sector", "SectorName")
    
    # Get model$IntermediateMargins and model$FinalConsumerMargins
    model$IntermediateMargins <- getMarginsTable(model, "intermediate")
    model$FinalConsumerMargins <- getMarginsTable(model, "final consumer")
    
  } else if (model$specs$ModelType=="State2R") {
    # Fork for state model here
  }
  
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



