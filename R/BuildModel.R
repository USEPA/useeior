#' Prepare economic components of an EEIO form USEEIO model.
#' @param modelname Name of the model from a config file.
#' @return A list with USEEIO model economic components.
prepareEEIOModel <- function(modelname) {
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
  # Get model$CommodityOutput, model$CommodityCPI, model$IndustryOutput, model$IndustryCPI, and model$FinalDemand
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    model$CommodityOutput <- generatePriceAdjustedCommodityOutputforYear(model$specs$IOYear, model$specs$PrimaryRegionAcronym, IsRoU = FALSE, model)
    model$CommodityCPI <- generatePriceAdjustedCommodityCPIforYear(model$specs$IOYear, model) # return a one-column table for IOYear
    # Adjust model$FinalDemand by CPI
    ReferenceCurrencyYearCPI <- generatePriceAdjustedCommodityCPIforYear(model$specs$ReferenceCurrencyYear, model)
    ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio <- ReferenceCurrencyYearCPI[, as.character(model$specs$ReferenceCurrencyYear)]/model$CommodityCPI[, as.character(model$specs$IOYear)]
    if (model$specs$BaseIOLevel=="Detail") {
      ReferenceCurrencyYearCPI[ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio=="NaN", "ReferenceCurrencyYeartoOutputYearCPIRatio"] <- 1
    }
    FinalDemand <- model$BEA$UseFinalDemand[rownames(ReferenceCurrencyYearCPI), ]
    model$FinalDemand <- FinalDemand*ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio
    DomesticFinalDemand <- model$BEA$DomesticFinalDemand[rownames(ReferenceCurrencyYearCPI), ]
    model$DomesticFinalDemand <- DomesticFinalDemand*ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio
  } else {
    # Get model$IndustryOutput from GDP tables
    if (model$specs$PrimaryRegionAcronym=="US") {
      model$IndustryOutput <- model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE]
    }
    # Get model$IndustryCPI from GDP tables
    model$IndustryCPI <- model$GDP$BEACPIIO[, as.character(model$specs$IOYear), drop = FALSE]
    # Adjust model$FinalDemand by CPI
    ModelIndustryCPI <- model$IndustryCPI
    ModelIndustryCPI$ReferenceCurrencyYeartoOutputYearRatio <- model$GDP$BEACPIIO[, as.character(model$specs$ReferenceCurrencyYear)]/model$GDP$BEACPIIO[, as.character(model$specs$IOYear)]
    FinalDemand <- merge(model$BEA$UseFinalDemand, ModelIndustryCPI[, "ReferenceCurrencyYeartoOutputYearRatio", drop = FALSE], by = 0, all.x = TRUE)
    rownames(FinalDemand) <- FinalDemand$Row.names
    # Modify ReferenceCurrencyYeartoOutputYearRatio of Used and Other sectors to 1
    FinalDemand[is.na(FinalDemand$ReferenceCurrencyYeartoOutputYearRatio), "ReferenceCurrencyYeartoOutputYearRatio"] <- 1
    FinalDemand <- FinalDemand[, model$BEA$FinalDemandCodes]*ModelIndustryCPI$ReferenceCurrencyYeartoOutputYearRatio
    FinalDemand <- FinalDemand[, model$BEA$FinalDemandCodes]
    # Transform ModelFinalDeamnd with MS
    model$FinalDemand <- transformFinalDemandwithMarketShares(FinalDemand, model)#This output needs to be tested - producing strange results
    # Adjust model$DomesticFinalDemand by CPI
    DomesticFinalDemand <- merge(model$BEA$DomesticFinalDemand, ModelIndustryCPI[, "ReferenceCurrencyYeartoOutputYearRatio", drop = FALSE], by = 0, all.x = TRUE)
    rownames(DomesticFinalDemand) <- DomesticFinalDemand$Row.names
    # Modify ReferenceCurrencyYeartoOutputYearRatio of Used and Other sectors to 1
    DomesticFinalDemand[is.na(DomesticFinalDemand$ReferenceCurrencyYeartoOutputYearRatio), "ReferenceCurrencyYeartoOutputYearRatio"] <- 1
    DomesticFinalDemand <- DomesticFinalDemand[, model$BEA$FinalDemandCodes]*ModelIndustryCPI$ReferenceCurrencyYeartoOutputYearRatio
    DomesticFinalDemand <- DomesticFinalDemand[, model$BEA$FinalDemandCodes]
    # Transform ModelFinalDeamnd with MS
    model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(DomesticFinalDemand, model)#This output needs to be tested - producing strange results
  }
  # Get model$SectorNames
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    USEEIONames <- utils::read.table(system.file("extdata", "USEEIO_Commodity_Code_Name.csv", package = "useeior"),
                                     sep = ",", header = TRUE, stringsAsFactors = FALSE)
    model$SectorNames <- merge(as.data.frame(model$Commodities, stringsAsFactors = FALSE), USEEIONames,
                               by.x = "model$Commodities", by.y = "Code", all.x = TRUE)
  } else {
    model$SectorNames <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
  }
  colnames(model$SectorNames) <- c("SectorCode", "SectorName")
  # Get model$IndustryMargins and model$FinalConsumerMargins
  model$IndustryMargins <- getMarginsTable(model$specs, "Industry") # Magrins Matrix
  model$IndustryMargins <- merge(as.data.frame(model$BEA$Commodities), model$IndustryMargins, by.x = "model$BEA$Commodities", by.y = "CommodityCode", all.x = TRUE)
  model$IndustryMargins[is.na(model$IndustryMargins$PRObyPURRatios), "PRObyPURRatios"] <- 1
  colnames(model$IndustryMargins)[1] <- "SectorCode"
  model$IndustryMargins$SectorCode <- as.character(model$IndustryMargins$SectorCode)
  
  model$FinalConsumerMargins <- getMarginsTable(model$specs, "FinalConsumer") # PCE&PEQ bridge df
  model$FinalConsumerMargins <- merge(as.data.frame(model$BEA$Commodities), model$FinalConsumerMargins, by.x = "model$BEA$Commodities", by.y = "CommodityCode", all.x = TRUE)
  model$FinalConsumerMargins[is.na(model$FinalConsumerMargins$PRObyPURRatios), "PRObyPURRatios"] <- 1
  colnames(model$FinalConsumerMargins)[1] <- "SectorCode"
  model$FinalConsumerMargins$SectorCode <- as.character(model$FinalConsumerMargins$SectorCode)
  return(model)
}

#' Build an EEIO form USEEIO model.
#' @param modelname Name of the model from a config file.
#' @export
#' @return A list with USEEIO model components and attributes.
buildEEIOModel <- function(modelname) {
  # Prepare model
  model <- prepareEEIOModel(modelname)
  # Generate matrices
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
  model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse
  if(model$specs$CommoditybyIndustryType == 'Commodity') {
    logging::loginfo(paste("Building commodityxcommodity direct requirement matrix ..."))
    model$A <- model$U_n %*% model$V_n
    model$A_d <- model$U_d_n %*% model$V_n 
  } else if(model$specs$CommoditybyIndustryType == 'Industry') {
    logging::loginfo(paste("Building industryxindustry requirement matrix ..."))
    model$A <- model$V_n %*% model$U_n
    model$A_d <- model$V_n %*% model$U_d_n
  }
  # Generate satellite tables
  model$sattableslist <- loadsattables(model)
  # Combine satellite table dfs into a single df
  model$sattables <- data.frame()
  for (satable in model$sattableslist) {
    model$sattables <- rbind(model$sattables, satable)
  }
  # ransform into a flow x sector matrix
  model$sattables["Flow"] <- apply(model$sattables[, c("FlowName", "FlowCategory", "FlowSubCategory", "FlowUnit")], 1 ,FUN = joinStringswithSlashes)
  model$sattables["Sector"] <- apply(model$sattables[, c("ProcessCode", "ProcessLocation")], 1, FUN = joinStringswithSlashes)

  #! Needs to be cast and made into matrix, but the problem is that the sectors need to have the order and completness of the model sectors list and not just those in the sat tables
  model$sattables_cast <- reshape2::dcast(model$sattables, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount") #! check why aggregation is needed
  # Move Flow to rowname so matrix is all numbers
  rownames(model$sattables_cast) <- model$sattables_cast$Flow
  model$sattables_cast$Flow <- NULL
  # Complete sector list using model$Industries
  columns_to_add <- tolower(paste(model$Industries[!model$Industries%in%model$sattables$ProcessCode], model$specs$PrimaryRegionAcronym, sep = "/"))
  model$sattables_cast[, columns_to_add] <- 0
  # Adjust column order to be the same with V_n rownames
  model$sattables_cast <- model$sattables_cast[, tolower(paste(rownames(model$V_n), model$specs$PrimaryRegionAcronym, sep = "/"))]
  # Generate B matrix
  model$B <- as.matrix(model$sattables_cast)

  # Transform B into a flowxcommodity matrix using market shares matrix for commodity models
  if(model$specs$CommoditybyIndustryType == 'Commodity') {
    model$B <- model$B %*% model$V_n
    colnames(model$B) <- tolower(paste(colnames(model$B), model$specs$PrimaryRegionAcronym, sep = "/"))
  }

  # Generate C matrix: LCIA indicators
  factors_from_static <- loadindicators(model$specs)
  factors_from_static$GHG <- tolower(paste(factors_from_static$Name, factors_from_static$Category, factors_from_static$Subcategory,
                                           factors_from_static$Unit, sep = "/"))
  factors_from_static[factors_from_static$Name%in%c("Carbon dioxide", "Methane", "Dinitrogen monoxide"), "Group"] <- "Major GHGs"
  factors_from_static[!factors_from_static$Name%in%c("Carbon dioxide", "Methane", "Dinitrogen monoxide"), "Group"] <- "Other GHGs"
  model$C <- reshape2::dcast(factors_from_static, Group ~ GHG, value.var = "Amount")
  rownames(model$C) <- model$C$Group
  model$C <- as.matrix(model$C[, rownames(model$B)])
  model$C[is.na(model$C)] <- 0

  # Calculates total requirements matrix as Leontief inverse of A (L)
  logging::loginfo("Calculating total requirements matrix...")
  I <- diag(nrow(model$A))
  model$L <- solve(I-model$A)
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating total emissions per dollar matrix...")
  model$M <- model$B %*% model$L
  # Calculate total impacts per dollar (U), impact category x sector
  model$U <- model$C %*% model$M
  
  logging::loginfo("Model build complete.")
  return(model)
}

#' Write model components to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @export
writeModelComponents <- function(model) {
  # Define output folder
  dir.create(paste("Model Builds/", model$specs$Model, sep = ""), recursive = TRUE) # meant to be flexible up to users
  outputfolder <- paste("Model Builds/", model$specs$Model, sep = "")

  # Demand
  if(model$specs$PrimaryRegionAcronym=="US") {
    Demand <- getUSTotalConsProd(model)
    #add in food system demand
    #Demand <- addDemandforSubsystem("food",Demand)
  }
  # Format model build components for IOMB
  MarketShares <- formatIOTableforIOMB(model$V_n, model)
  DirectRequirementsCoefficients <- formatIOTableforIOMB(model$A, model)
  # Satellite table
  sattableGHG <- model$sattableslist[["GHG"]]
  # Indicators
  indicatorsGHG <- model$C
  # write model build components to csv
  utils::write.csv(Demand, paste(outputfolder, "FinalDemand.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(MarketShares, paste(outputfolder, "MarketShares.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(DirectRequirementsCoefficients, paste(outputfolder, "DirectRequirementsCoefficients.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(sattableGHG, paste(outputfolder, "Satellite_GHG.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(indicatorsGHG, paste(outputfolder, "Indicators_GHG.csv", sep = "/"), row.names = FALSE)
  # Write logs to file in Model Builds folder
  logtimestamp <- Sys.Date()
  dir.create("modelbuildlogs", recursive = TRUE) # meant to be flexible up to users
  logfilename <- paste0("modelbuildlogs/", model$specs$Model, logtimestamp, ".log")
  logging::addHandler(logging::writeToFile, file = logfilename, level = "INFO")

  logging::loginfo("Model components written to Model Build folder.")
}

