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
  BEA$CommodityTotalOutputCodes <- getVectorOfCodes("CommodityTotalOutput")
  BEA$IndustryTotalOutputCodes <- getVectorOfCodes("IndustryTotalOutput")
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
  BEA$MakeIndustryOutput <- BEA$Make[BEA$Industries, BEA$IndustryTotalOutputCodes, drop = FALSE] * 1E6 # data frame, values are in dollars ($)
  BEA$UseTransactions <- BEA$Use[BEA$Commodities, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
  BEA$UseFinalDemand <- BEA$Use[BEA$Commodities, BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  BEA$UseValueAdded <- BEA$Use[BEA$ValueAddedCodes, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
  BEA$UseCommodityOutput <- BEA$Use[BEA$Commodities, BEA$CommodityTotalOutputCodes, drop = FALSE] * 1E6 # data frame, values are in dollars ($)
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
