# Functions for loading external import factors

#' Load and prepare import coefficients
#' @param model An EEIO form USEEIO model object with model specs loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return Q_t, matrix of import coefficients (flow x sector).
loadExternalImportFactors <- function(model, configpaths = NULL) {

  # Read in file with Import factors
  IFSpec <- model$specs$ImportFactors
  if(is.null(IFSpec$FileLocation)){
    filename <- getInputFilePath(configpaths, folderPath = "extdata", filename = IFSpec$StaticFile)
  } else if(IFSpec$FileLocation == "DataCommons") {
    filename <- loadDataCommonsfile(IFSpec$StaticFile)    
  } else if(IFSpec$FileLocation == "useeior") {
    filename <- getInputFilePath(configpaths, folderPath = "extdata", filename = IFSpec$StaticFile)
  }
  IFTable <- utils::read.table(filename, sep = ",", header = TRUE,
                               stringsAsFactors = FALSE)
  
  # Store meta data
  meta <- data.frame(matrix(nrow = 0, ncol = 5))
  cnames <- colnames(IFTable[1:5])
  colnames(meta) <- cnames
  meta[1,1] <- model$specs$BaseIOLevel
  meta[2:5] <- IFTable[1,2:5]
  
  # Format IFTable to match model$M
  IFTable['Flow'] <- paste(IFTable$Flowable, IFTable$Context, IFTable$Unit, sep = "/")

  # Convert from basic to producer price using TAU of CurrencyYear
  Tau <- model$Tau[, as.character(meta$CurrencyYear)]
  names(Tau) <- gsub("/.*","",names(Tau))
  # For state models, keep only unique names
  Tau <- Tau[unique(names(Tau))]
  IFTable <- merge(IFTable, as.data.frame(Tau), by.x = 'Sector', by.y = 0, all.y = FALSE)
  IFTable['FlowAmount'] <- IFTable['FlowAmount'] * IFTable['Tau']
  IFTable['PriceType'] <- 'Producer'
  IFTable['CurrencyYear'] <- model$specs$IOYear
  
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
  
  Q_t <- standardizeandcastSatelliteTable(IFTable, model)
  # standardizeandcast prepares df for industries, convert to commodities
  Q_t[, setdiff(model$Commodities$Code_Loc, colnames(Q_t))] <- 0
  # Adjust column order to be the same with V_n rownames
  Q_t <- Q_t[, model$Commodities$Code_Loc]
  Q_t <- as.matrix(Q_t)

  return(Q_t)
}


#' Create import Use table and validate domestic+import against full use model .
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A model object with explicit import components.
buildModelwithImportFactors <- function(model, configpaths = NULL) {
  # Deriving the economic component of the Swedish equation (see Palm et al. 2019) for import factors: 
  # f^(d+m) = s^d*L^d*y^d + Q^t*A^m*L^d*y^d + Q^t*y^m + f^h
  # s^d are the domestic direct environmental coefficients, and Q are the environmental import multipliers, s_m*L_m. Dropping s_d and s_m we get
  # x^(d+m) = s^d*L^d*y^d + L^m*A^m*L^d*y^d + L^m*y^m + f^h
  # Since f^h is not currently part of the useeior model calculations, we drop it:
  # x^(d+m) = L^d*y^d + L^m*A^m*L^d*y^d + L^m*y^m 
  # The resulting expression should be equivalent to the x = L*y such that
  # x^(d+m) = x = L*y
  
  logging::loginfo("Building Import A (A_m) accounting for ITA in Domestic FD.\n")
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
  
  logging::loginfo("Calculating Q_t matrix (total emissions and resource use per dollar from imported activity)...")
  Q_t <- loadExternalImportFactors(model, configpaths)
  
  # Fill in flows for Q_t not found in Import Factors but that exist in model and align order
  Q_t <- rbind(Q_t, model$M_d[setdiff(rownames(model$M_d), rownames(Q_t)),])
  Q_t <- Q_t[rownames(model$M_d), ]
  
  model$Q_t <- Q_t
  
  return(model)
}
