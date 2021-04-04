#' Prepare economic components of an EEIO model.
#' @param model A list of model specs
#' @return A list with EEIO model economic components.
#' @export
loadIOData <- function(model) {
  # Declare model IO objects
  logging::loginfo("Initializing IO tables...")
  if (model$specs$ModelType=="US") {
    model <- loadNationalIOData(model)
  } else if (model$specs$ModelType=="State2R") {
    # Fork for state model here
  }
  
  # Add Chain Price Index (CPI) to model
  model$MultiYearIndustryCPI <- loadChainPriceIndexTable(model$specs)[model$Industries$Code, ]
  rownames(model$MultiYearIndustryCPI) <- model$Industries$Code_Loc
  # Transform industry CPI to commodity CPI
  model$MultiYearCommodityCPI <- as.data.frame(model$Commodities, row.names = model$Commodities$Code_Loc)[, FALSE]
  for (year_col in colnames(model$MultiYearIndustryCPI)) {
    model$MultiYearCommodityCPI[, year_col] <- transformIndustryCPItoCommodityCPIforYear(as.numeric(year_col), model)
  }
  
  # Check for disaggregation
  if(!is.null(model$specs$DisaggregationSpecs)){
    model <- disaggregateModel(model)
  }
  
  return(model)
}

#' Prepare economic components of an EEIO form USEEIO model.
#' @param model A model object with model specs loaded.
#' @return A list with USEEIO model economic components.
loadNationalIOData <- function(model) {
  # Load BEA IO and gross output tables
  BEA <- loadBEAtables(model$specs)
  # model$Commodities
  model$Commodities <- merge(as.data.frame(BEA$Commodities, stringsAsFactors = FALSE),
                             utils::read.table(system.file("extdata", "USEEIO_Commodity_Code_Name.csv", package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE),
                             by.x = "BEA$Commodities", by.y = "Code", all.x = TRUE, sort = FALSE)
  colnames(model$Commodities) <- c("Code", "Name")
  model$Commodities <- model$Commodities[order(match(BEA$Commodities, model$Commodities$Code)), ]
  model$Commodities$Code_Loc <- apply(cbind(model$Commodities$Code, model$specs$PrimaryRegionAcronym), 1, FUN = joinStringswithSlashes)
  
  # model$Industries
  model$Industries <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
  colnames(model$Industries) <- c("Code", "Name")
  model$Indicators$meta[order(match(model$Indicators$meta$Name, colnames(df))), "Code"]
  model$Industries <- model$Industries[order(match(BEA$Industries, model$Industries$Code)), ]
  model$Industries$Code_Loc <- apply(cbind(model$Industries$Code, model$specs$PrimaryRegionAcronym), 1, FUN = joinStringswithSlashes)
  
  # model$FinalDemandSectors
  model$FinalDemandSectors <- utils::stack(BEA[c("HouseholdDemandCodes", "InvestmentDemandCodes", "ChangeInventoriesCodes",
                                                 "ExportCodes", "ImportCodes", "GovernmentDemandCodes")])
  model$FinalDemandSectors[] <- lapply(model$FinalDemandSectors, as.character)
  colnames(model$FinalDemandSectors) <- c("Code", "Name")
  model$FinalDemandSectors$Name <- gsub(c("Codes|DemandCodes"), "", model$FinalDemandSectors$Name)
  model$FinalDemandSectors$Code_Loc <- apply(cbind(model$FinalDemandSectors$Code, model$specs$PrimaryRegionAcronym),
                                             1, FUN = joinStringswithSlashes)
  
  # model$MarginSectors
  model$MarginSectors <- utils::stack(BEA[c("TransportationCodes", "WholesaleCodes", "RetailCodes")])
  model$MarginSectors[] <- lapply(model$MarginSectors, as.character)
  colnames(model$MarginSectors) <- c("Code", "Name")
  model$MarginSectors$Name <- gsub(c("Codes"), "", model$MarginSectors$Name)
  model$MarginSectors$Code_Loc <- apply(cbind(model$MarginSectors$Code, model$specs$PrimaryRegionAcronym),
                                        1, FUN = joinStringswithSlashes)
  
  # IO tables
  model$MakeTransactions <- BEA$MakeTransactions
  model$UseTransactions <- BEA$UseTransactions
  model$DomesticUseTransactions <- BEA$DomesticUseTransactions
  model$UseValueAdded <- BEA$UseValueAdded
  model$FinalDemand <- BEA$UseFinalDemand
  model$DomesticFinalDemand <- BEA$DomesticFinalDemand
  ## Modify row and column names in the IO tables
  # Use model$Industries
  rownames(model$MakeTransactions) <- colnames(model$UseTransactions) <- colnames(model$DomesticUseTransactions) <-
    colnames(model$UseValueAdded) <- model$Industries$Code_Loc
  # Use model$Commodities
  colnames(model$MakeTransactions) <- rownames(model$UseTransactions) <- rownames(model$DomesticUseTransactions) <- 
    rownames(model$FinalDemand) <- rownames(model$DomesticFinalDemand) <- model$Commodities$Code_Loc
  # Apply joinStringswithSlashes based on original row/column names
  rownames(model$UseValueAdded) <- apply(cbind(rownames(model$UseValueAdded), model$specs$PrimaryRegionAcronym),
                                         1, FUN = joinStringswithSlashes)
  colnames(model$FinalDemand) <- colnames(model$DomesticFinalDemand) <- apply(cbind(colnames(model$FinalDemand),
                                                                                    model$specs$PrimaryRegionAcronym),
                                                                              1, FUN = joinStringswithSlashes)
  
  model$IndustryOutput <- colSums(model$UseTransactions) + colSums(model$UseValueAdded)
  model$CommodityOutput <- rowSums(model$UseTransactions) + rowSums(model$FinalDemand)
  
  model$MultiYearIndustryOutput <- loadNationalGrossOutputTable(model$specs)[model$Industries$Code, ]
  rownames(model$MultiYearIndustryOutput) <- model$Industries$Code_Loc
  model$MultiYearIndustryOutput[, as.character(model$specs$IOYear)] <- model$IndustryOutput
  # Transform multi-year industry output to commodity output
  model$MultiYearCommodityOutput <- as.data.frame(model$CommodityOutput)[, FALSE]
  for (year_col in colnames(model$MultiYearIndustryOutput)) {
    model$MultiYearCommodityOutput[, year_col] <- transformIndustryOutputtoCommodityOutputforYear(as.numeric(year_col), model)
  }
  model$MultiYearCommodityOutput[, as.character(model$specs$IOYear)] <- model$CommodityOutput
  
  # Transform model FinalDemand and DomesticFinalDemand to by-industry form
  if (model$specs$CommoditybyIndustryType=="Industry") {
    model$FinalDemand <- transformFinalDemandwithMarketShares(model$FinalDemand, model)#This output needs to be tested - producing strange results
    model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(model$DomesticFinalDemand, model)#This output needs to be tested - producing strange results
  }
  
  # Add Final Consumer Margins table
  model$FinalConsumerMargins <- getFinalConsumerMarginsTable(model)
  
  return(model)
}

#' Load BEA IO tables in a list based on model config.
#' @param specs Model specifications.
#' @return A list with BEA IO tables.
loadBEAtables <- function(specs) {
  BEA <- list()
  
  # Get BEA sectors by group
  BEA$Commodities <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Commodity")
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

