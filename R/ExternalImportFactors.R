# Functions for loading external import factors

#' Load and prepare import coefficients
#' @param model An EEIO form USEEIO model object with model specs loaded
#' @param configpaths str vector, paths (including file name) of model configuration file.
#' If NULL, built-in config files are used.
#' @return M_m, matrix of import coefficients (flow x sector).
loadExternalImportFactors <- function(model, configpaths = NULL) {
  IFTable <- readImportFactorTable(IFSpec=model$specs$ImportFactors, configpaths=configpaths)
  IFTable <- processImportFactors(model, IFTable)
  M_m <- castImportFactors(IFTable, model)
  return(M_m)
}

#' Load and prepare import coefficients
#' @param IFSpec list of specs for import factor file
#' @param configpaths str vector, paths (including file name) of model configuration file.
#' If NULL, built-in config files are used.
#' @return IFtable, dataframe of unprocessed import factors
readImportFactorTable <- function(IFSpec, configpaths = NULL) {
  # Read in file with Import factors
  if(is.null(IFSpec$FileLocation)){
    filename <- getInputFilePath(configpaths, folderPath = "extdata", filename = IFSpec$StaticFile)
  } else if(IFSpec$FileLocation == "DataCommons") {
    filename <- loadDataCommonsfile(IFSpec$StaticFile)    
  } else if(IFSpec$FileLocation == "useeior") {
    filename <- getInputFilePath(configpaths, folderPath = "extdata", filename = IFSpec$StaticFile)
  }
  IFTable <- utils::read.table(filename, sep = ",", header = TRUE,
                               stringsAsFactors = FALSE)
  return(IFTable)
}

#' Load and prepare import coefficients
#' @param model An EEIO form USEEIO model object with model specs loaded
#' @param IFTable, dataframe of unprocessed import factors
#' @return IFTable, dataframe of processed of import coefficients (flow x sector).
processImportFactors <- function(model, IFTable) {
  # Store meta data
  meta <- data.frame(matrix(nrow = 0, ncol = 4))
  meta[1,1] <- model$specs$BaseIOLevel
  meta[2:4] <- IFTable[1,c("ReferenceCurrency", "Year", "PriceType")]
  colnames(meta) <- c("Sector","ReferenceCurrency", "Year", "PriceType")
  
  # Format IFTable to match model$M
  IFTable['Flow'] <- paste(IFTable$Flowable, IFTable$Context, IFTable$Unit, sep = "/")

  if(meta[1, "PriceType"] == "Basic") {
    # Convert from basic to producer price using TAU
    Tau <- model$Tau[, as.character(meta$Year)]
    names(Tau) <- gsub("/.*","",names(Tau))
    # For state models, keep only unique names
    Tau <- Tau[unique(names(Tau))]
    IFTable <- merge(IFTable, as.data.frame(Tau), by.x = 'Sector', by.y = 0, all.y = FALSE)
    IFTable['FlowAmount'] <- IFTable['FlowAmount'] * IFTable['Tau']
    IFTable['PriceType'] <- 'Producer'
  } else if (meta[1, "PriceType"] != "Producer") {
    stop("PriceType must be 'Basic' or 'Producer'")
  }
  
  if(model$specs$IODataSource =="stateior") {
    IFTable_SoI <- IFTable
    IFTable_SoI['Location'] <- model$specs$ModelRegionAcronyms[[1]]
    IFTable_RoUS <- IFTable
    IFTable_RoUS['Location'] <- model$specs$ModelRegionAcronyms[[2]]
    IFTable <- rbind(IFTable_SoI, IFTable_RoUS)
  } else {
    # assumes that if IODataSource is not stateior, it is a one a region model
    IFTable['Location'] <- "US"
  }
  return(IFTable)
}

#' Converts import factors table (of commodities) into flows x sector matrix-like format
#' @param IFTable, dataframe of import factors
#' @param model An EEIO model object with model specs, IO tables, and matrices loaded
#' @return A matrix of flows x sector 
castImportFactors <- function(IFTable, model) {
  # Add fields for sector as combinations of existing fields
  IFTable[, "Sector"] <- apply(IFTable[, c("Sector", "Location")],
                               1, FUN = joinStringswithSlashes)
  # Cast df into a flow x sector matrix
  df_cast <- reshape2::dcast(IFTable, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  # Move Flow to rowname so matrix is all numbers
  rownames(df_cast) <- df_cast$Flow
  df_cast$Flow <- NULL
  # Complete sector list according to model$Commodities
  df_cast[, setdiff(model$Commodities$Code_Loc, colnames(df_cast))] <- 0
  # Adjust column order to be the same with M_d
  df_cast <- df_cast[, colnames(model$M_d)]
  M_m <- as.matrix(df_cast)
  return(M_m)
}

#' Create import Use table and validate domestic+import against full use model .
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A model object with explicit import components.
buildModelwithImportFactors <- function(model, configpaths = NULL) {
  # (see Palm et al. 2019)

  logging::loginfo("Building A_m (import requirements) accounting for international trade adjustment in domestic final demand.\n")
  # Re-derive import values in Use and final demand
  # _m denotes import-related structures
  model$UseTransactions_m <- model$UseTransactions - model$DomesticUseTransactions
  model$U_n_m <- normalizeIOTransactions(model$UseTransactions_m, model$IndustryOutput) #normalized imported Use
  
  if(model$specs$CommodityorIndustryType == "Commodity") {
    logging::loginfo("Building commodity-by-commodity A_m matrix (imported direct requirements)...")
    model$A_m <- model$U_n_m %*% model$V_n
  } else if(model$specs$CommodityorIndustryType == "Industry") {
    logging::loginfo("Building industry-by-industry A_m matrix (imported direct requirements)...")
    model$A_m <- model$V_n %*% model$U_d_m
  }
  
  logging::loginfo("Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...")
  model$M_d <- model$B %*% model$L_d 
  
  logging::loginfo("Calculating M_m matrix (total emissions and resource use per dollar from imported activity)...")
  M_m <- loadExternalImportFactors(model, configpaths)
  
  # Fill in flows for M_m not found in Import Factors but that exist in model and align order
  M_m <- rbind(M_m, model$M_d[setdiff(rownames(model$M_d), rownames(M_m)),])
  M_m <- M_m[rownames(model$M_d), ]
  
  model$M_m <- M_m
  
  model$M <- calculateMwithImportFactors(model)
  
  return(model)
}

#' Derives an M matrix for total embodied flows from domestic and imported supply chains.
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @return An M matrix of flows x sector
calculateMwithImportFactors <- function(model) {
  logging::loginfo("Calculating M matrix (total emissions and resource use per dollar) ...")
  
  # embodied flows from the use of imports by industries to make their commodities
  # both directly (from A_m) and indirectly (by scaling it to total requirements using L_d)
  M_mi <- model$M_m %*% model$A_m %*% model$L_d
  
  M <- model$M_d + M_mi
  return(M)
}
