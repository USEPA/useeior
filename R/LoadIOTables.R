# Functions for loading input-output tables

#' Prepare economic components of an EEIO form USEEIO model.
#' @param model An EEIO form USEEIO model object with model specs loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A list with EEIO form USEEIO model economic components.
loadIOData <- function(model, configpaths = NULL) {
  # Declare model IO objects
  logging::loginfo("Initializing IO tables...")
  # Load model IO meta
  model <- loadIOmeta(model)
  # Define IO table names
  io_table_names <- c("MakeTransactions", "UseTransactions", "DomesticUseTransactions",
                      "UseValueAdded", "FinalDemand", "DomesticFinalDemand",
                      "InternationalTradeAdjustment")
  # Load IO data
  if (model$specs$IODataSource=="BEA") {
    io_codes <- loadIOcodes(model$specs)
    model[io_table_names] <- loadNationalIOData(model, io_codes)[io_table_names]
  } else if (model$specs$IODataSource=="stateior") {
    model[io_table_names] <- loadTwoRegionStateIOtables(model)[io_table_names]
  }
  
  # Add Industry and Commodity Output
  model <- loadCommodityandIndustryOutput(model)
  
  # Transform model FinalDemand and DomesticFinalDemand to by-industry form
  if (model$specs$CommodityorIndustryType=="Industry") {
    # Keep the orignal FinalDemand (in by-commodity form)
    model$FinalDemandbyCommodity <- model$FinalDemand
    model$DomesticFinalDemandbyCommodity <- model$DomesticFinalDemand
    model$InternationalTradeAdjustmentbyCommodity <- model$InternationalTradeAdjustment
    model$FinalDemand <- transformFinalDemandwithMarketShares(model$FinalDemand, model)
    model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(model$DomesticFinalDemand, model)
    model$InternationalTradeAdjustment <- unlist(transformFinalDemandwithMarketShares(model$InternationalTradeAdjustment, model))
    names(model$InternationalTradeAdjustment) <- model$Industries$Code_Loc
  }
  
  # Add Margins table
  model$Margins <- getMarginsTable(model)
  
  # Add Chain Price Index (CPI) to model
  model$MultiYearIndustryCPI <- loadChainPriceIndexTable(model$specs)[model$Industries$Code, ]
  rownames(model$MultiYearIndustryCPI) <- model$Industries$Code_Loc
  # Transform industry CPI to commodity CPI
  model$MultiYearCommodityCPI <- as.data.frame(model$Commodities, row.names = model$Commodities$Code_Loc)[, FALSE]
  for (year_col in colnames(model$MultiYearIndustryCPI)) {
    model$MultiYearCommodityCPI[, year_col] <- transformIndustryCPItoCommodityCPIforYear(as.numeric(year_col), model)
  }
  
  # Check for aggregation
  if(!is.null(model$specs$AggregationSpecs)){
    model <- getAggregationSpecs(model, configpaths)
    model <- aggregateModel(model)
  }
  
  # Check for disaggregation
  if(!is.null(model$specs$DisaggregationSpecs)){
    model <- getDisaggregationSpecs(model, configpaths)
    model <- disaggregateModel(model)
  }
  
  # Check for hybridization
  if(model$specs$ModelType == "EEIO-IH"){
    model <- getHybridizationSpecs(model, configpaths)
    model <- getHybridizationFiles(model, configpaths)
  }
    
  return(model)
}

#' Prepare metadata of economic components of an EEIO form USEEIO model.
#' @param model A model object with model specs loaded.
#' @return A list with USEEIO model economic components' metadata.
loadIOmeta <- function(model) {
  io_codes <- loadIOcodes(model$specs)
  model_base_elements <- names(model)
  model$Commodities <- merge(as.data.frame(io_codes$Commodities, stringsAsFactors = FALSE),
                             utils::read.table(system.file("extdata", "USEEIO_Commodity_Meta.csv",
                                                           package = "useeior"),
                                               sep = ",", header = TRUE, stringsAsFactors = FALSE),
                             by.x = "io_codes$Commodities", by.y = "Code",
                             all.x = TRUE, sort = FALSE)
  model$Industries <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName",
                                model$specs$BaseIOSchema, sep = "_"))
  model$FinalDemandMeta <- merge(get(paste(model$specs$BaseIOLevel, "FinalDemandCodeName",
                                           model$specs$BaseIOSchema, sep = "_")),
                                 utils::stack(io_codes[c("HouseholdDemandCodes",
                                                         "InvestmentDemandCodes",
                                                         "ChangeInventoriesCodes",
                                                         "ExportCodes", "ImportCodes",
                                                         "GovernmentDemandCodes")]),
                                 by = 1, sort = FALSE)
  if (model$specs$IODataSource=="BEA") {
    model$InternationalTradeAdjustmentMeta <- utils::stack(io_codes["InternationalTradeAdjustmentCodes"])
  }
  model$MarginSectors <- utils::stack(io_codes[c("TransportationCodes",
                                                 "WholesaleCodes", "RetailCodes")])
  model$ValueAddedMeta <- get(paste(model$specs$BaseIOLevel, "ValueAddedCodeName",
                                    model$specs$BaseIOSchema, sep = "_"))
  model_meta <- names(model)[!names(model) %in% model_base_elements]
  # Format model IO meta and add Code_Loc column
  for (meta in model_meta) {
    # Change column names
    if (meta=="Commodities") {
      colnames(model[[meta]])[1] <- "Code"
    } else {
      colnames(model[[meta]]) <- c("Code", "Name", "Group")[1:ncol(model[[meta]])]
    }
    # Create a code_loc table
    code_loc <- cbind(model[[meta]][["Code"]], rep(model$specs$ModelRegionAcronyms,
                                                   each = length(model[[meta]][["Code"]])))
    # Repeat model IO meta df to prepare for adding Code_Loc
    model[[meta]] <- as.data.frame(lapply(model[[meta]], rep,
                                          nrow(code_loc)/nrow(model[[meta]])))
    model[[meta]][] <- lapply(model[[meta]], as.character)
    # Add Code_Loc column
    model[[meta]][["Code_Loc"]] <- apply(code_loc, 1, FUN = joinStringswithSlashes)
  } 
  model$Commodities$Unit <- "USD"
  model$Industries$Unit <- "USD"
  # Apply final touches to FinalDemandMeta and MarginSectors
  model$FinalDemandMeta$Group <- gsub(c("Codes|DemandCodes"), "",
                                      model$FinalDemandMeta$Group)
  model$MarginSectors$Name <- gsub(c("Codes"), "", model$MarginSectors$Name)
  return(model)
}

#' Load BEA IO codes in a list based on model config.
#' @param specs Model specifications.
#' @return A list with BEA IO codes.
loadIOcodes <- function(specs) {
  io_codes <- list()
  # Get IO sector codes by group
  io_codes[c("Commodities", "Industries")] <- lapply(c("Commodity", "Industry"),
                                                     FUN = getVectorOfCodes,
                                                     ioschema = specs$BaseIOSchema,
                                                     iolevel = specs$BaseIOLevel)
  codes <- c("ValueAdded", "HouseholdDemand", "InvestmentDemand",
             "ChangeInventories", "Export", "Import", "GovernmentDemand",
             "Scrap", "Transportation", "Wholesale", "Retail")
  io_codes[paste0(codes, "Codes")] <- lapply(codes, FUN = getVectorOfCodes,
                                             ioschema = specs$BaseIOSchema,
                                             iolevel = specs$BaseIOLevel)
  io_codes$FinalDemandCodes <- unlist(io_codes[paste0(c("HouseholdDemand",
                                                        "InvestmentDemand",
                                                        "ChangeInventories",
                                                        "Export", "Import",
                                                        "GovernmentDemand"),
                                                      "Codes")],
                                      use.names = FALSE)
  io_codes$InternationalTradeAdjustmentCodes <- gsub("F050", "F051", io_codes$ImportCodes)
  return(io_codes)
}

#' Prepare economic components of an EEIO form USEEIO model.
#' @param model A model object with model specs loaded.
#' @param io_codes A list of BEA IO codes.
#' @return A list with USEEIO model economic components.
loadNationalIOData <- function(model, io_codes) {
  # Load BEA IO and gross output tables
  BEA <- loadBEAtables(model$specs, io_codes)
  # Generate domestic Use transaction and final demand
  DomesticUse <- generateDomesticUse(cbind(BEA$UseTransactions, BEA$FinalDemand), model)
  BEA$DomesticUseTransactions <- DomesticUse[, io_codes$Industries]
  BEA$DomesticFinalDemand <- DomesticUse[, io_codes$FinalDemandCodes]
  # Generate Import Cost vector
  BEA$InternationalTradeAdjustment <- generateInternationalTradeAdjustmentVector(cbind(BEA$UseTransactions, BEA$FinalDemand), model)
  # Modify row and column names to Code_Loc format in all IO tables
  # Use model$Industries
  rownames(BEA$MakeTransactions) <- colnames(BEA$UseTransactions) <-
    colnames(BEA$DomesticUseTransactions) <- colnames(BEA$UseValueAdded) <-
    model$Industries$Code_Loc
  # Use model$Commodities
  colnames(BEA$MakeTransactions) <- rownames(BEA$UseTransactions) <-
    rownames(BEA$DomesticUseTransactions) <- rownames(BEA$FinalDemand) <-
    rownames(BEA$DomesticFinalDemand) <- names(BEA$InternationalTradeAdjustment) <-
    model$Commodities$Code_Loc
  # Use model$FinalDemandMeta
  colnames(BEA$FinalDemand) <- colnames(BEA$DomesticFinalDemand) <-
    model$FinalDemandMeta$Code_Loc
  # Use model$ValueAddedMeta
  rownames(BEA$UseValueAdded) <- model$ValueAddedMeta$Code_Loc
  return(BEA)
}

#' Load BEA IO tables in a list based on model config and io_codes.
#' @param specs Model specifications.
#' @param io_codes A list of BEA IO codes.
#' @return A list with BEA IO tables
loadBEAtables <- function(specs, io_codes) {
  BEA <- list()
  # Load pre-saved Make and Use tables
  Redef <- ifelse(specs$BasewithRedefinitions, "AfterRedef", "BeforeRedef")
  BEA$Make <- get(paste(specs$BaseIOLevel, "Make", specs$IOYear, Redef, sep = "_"))
  BEA$Use <-  get(paste(specs$BaseIOLevel, "Use", specs$IOYear, specs$BasePriceType, Redef, sep = "_"))
  
  # Separate Make and Use tables into specific IO tables (all values in $)
  BEA$MakeTransactions <- BEA$Make[io_codes$Industries, io_codes$Commodities] * 1E6
  BEA$MakeIndustryOutput <- as.data.frame(rowSums(BEA$MakeTransactions))
  BEA$UseTransactions <- BEA$Use[io_codes$Commodities, io_codes$Industries] * 1E6
  BEA$FinalDemand <- BEA$Use[io_codes$Commodities, io_codes$FinalDemandCodes] * 1E6
  BEA$UseValueAdded <- BEA$Use[io_codes$ValueAddedCodes, io_codes$Industries] * 1E6
  BEA$UseCommodityOutput <- as.data.frame(rowSums(cbind(BEA$UseTransactions, BEA$FinalDemand)))
  # Replace NA with 0 in IO tables
  if(specs$BaseIOSchema==2007) {
    BEA$MakeTransactions[is.na(BEA$MakeTransactions)] <- 0
    BEA$UseTransactions[is.na(BEA$UseTransactions)] <- 0
    BEA$FinalDemand[is.na(BEA$FinalDemand)] <- 0
  }
  return(BEA)
}

#' Load two-region state IO tables in a list based on model config.
#' @param model An EEIO form USEEIO model object with model specs and IO meta data loaded.
#' @return A list with state IO tables.
loadTwoRegionStateIOtables <- function(model) {
  StateIO <- list()
  # Load IO tables from stateior
  StateIO$MakeTransactions <- getTwoRegionIOData(model, "Make")
  StateIO$UseTransactions <- getTwoRegionIOData(model, "UseTransactions")
  StateIO$FinalDemand <- getTwoRegionIOData(model, "FinalDemand")
  StateIO$DomesticUseTransactions <- getTwoRegionIOData(model, "DomesticUseTransactions")
  StateIO$DomesticFinalDemand <- getTwoRegionIOData(model, "DomesticFinalDemand")
  StateIO$UseValueAdded <- getTwoRegionIOData(model, "ValueAdded")
  StateIO$InternationalTradeAdjustment <- getTwoRegionIOData(model, "InternationalTradeAdjustment")
  return(StateIO)
}

#' Prepare commodity and industry output of an EEIO form USEEIO model.
#' @param model A model object with model specs and fundamental IO data loaded.
#' @return A list with USEEIO model economic components.
loadCommodityandIndustryOutput <- function(model) {
  if (model$specs$IODataSource=="BEA") {
    # Calculate industry and commodity output
    model <- calculateIndustryCommodityOutput(model)
    # Load multi-year industry output
    model$MultiYearIndustryOutput <- loadNationalGrossOutputTable(model$specs)[model$Industries$Code, ]
    rownames(model$MultiYearIndustryOutput) <- model$Industries$Code_Loc
    model$MultiYearIndustryOutput[, as.character(model$specs$IOYear)] <- model$IndustryOutput
    # Transform multi-year industry output to commodity output
    model$MultiYearCommodityOutput <- as.data.frame(model$CommodityOutput)[, FALSE]
    for (year_col in colnames(model$MultiYearIndustryOutput)) {
      model$MultiYearCommodityOutput[, year_col] <- transformIndustryOutputtoCommodityOutputforYear(as.numeric(year_col), model)
    }
    model$MultiYearCommodityOutput[, as.character(model$specs$IOYear)] <- model$CommodityOutput
  } else if (model$specs$IODataSource=="stateior") {
    # Define state, year and iolevel
    if (!"US-DC"%in%model$specs$ModelRegionAcronyms) {
      state <- state.name[state.abb==gsub(".*-", "", model$specs$ModelRegionAcronyms[1])]
    } else {
      state <- "District of Columbia"
    }
    # Load industry and commodity output
    model$IndustryOutput <- getTwoRegionIOData(model, "IndustryOutput")
    model$CommodityOutput <- getTwoRegionIOData(model, "CommodityOutput")
    # Load multi-year industry and commodity output
    years <- as.character(2012:2017)
    tmpmodel <- model
    model$MultiYearIndustryOutput <- as.data.frame(model$IndustryOutput)[, FALSE]
    model$MultiYearCommodityOutput <- as.data.frame(model$CommodityOutput)[, FALSE]
    for (year in years) {
      tmpmodel$specs$IOYear <- year
      model$MultiYearIndustryOutput[, year] <- getTwoRegionIOData(tmpmodel, "IndustryOutput")
      model$MultiYearCommodityOutput[, year] <- getTwoRegionIOData(tmpmodel, "CommodityOutput")
    }
  }
  return(model)
}

#' Calculate industry and commodity output vectors from model components.
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return An EEIO model with industry and commodity output added
calculateIndustryCommodityOutput <- function(model) {
  model$IndustryOutput <- colSums(model$UseTransactions) + colSums(model$UseValueAdded)
  model$CommodityOutput <- rowSums(model$UseTransactions) + rowSums(model$FinalDemand)
  return(model)
}
