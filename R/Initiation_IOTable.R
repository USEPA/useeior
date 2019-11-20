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

  # Load pre-saved Make and Use tables
  Redef <- ifelse(specs$BasewithRedefinitions, "AfterRedef", "BeforeRedef")
  #MakeFile <- paste0("data/", specs$BaseIOLevel, "_Make_", specs$IOYear, "_", Redef, ".rda")
  UseFile <- paste0("data/", specs$BaseIOLevel, "_Use_", specs$IOYear, "_", specs$BasePriceType, "_", Redef, ".rda")
  MakeData <- paste0(specs$BaseIOLevel, "_Make_", specs$IOYear, "_", Redef)
  #BEA$Make <- get(load(MakeFile))
  BEA$Make <- get(MakeData)
  BEA$Use <-  get(load(UseFile))

  # Separate Make and Use tables into specific tables
  BEA$MakeTransactions <- BEA$Make[BEA$Industries, BEA$Commodities] * 1E6 # data frame, values are in dollars ($)
  BEA$MakeIndustryOutput <- BEA$Make[BEA$Industries, BEA$IndustryTotalOutputCodes, drop = FALSE] * 1E6 # data frame, values are in dollars ($)
  BEA$UseTransactions <- BEA$Use[BEA$Commodities, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
  BEA$UseFinalDemand <- BEA$Use[BEA$Commodities, BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  BEA$UseValueAdded <- BEA$Use[BEA$ValueAddedCodes, BEA$Industries] * 1E6 # data frame, values are in dollars ($)
  BEA$UseCommodityOutput <- BEA$Use[BEA$Commodities, BEA$CommodityTotalOutputCodes, drop = FALSE] * 1E6 # data frame, values are in dollars ($)
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
