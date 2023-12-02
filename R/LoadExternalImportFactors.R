# Functions for loading external import factors

#' Load and prepare import coefficients
#' @param model An EEIO form USEEIO model object with model specs loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return M_m, matrix of import coefficients (flow x sector).
loadExternalImportFactors <- function(model, configpaths = NULL) {

  # Read in file with Import factors
  IFSpec <- model$specs$ImportFactors[[1]]
  filename <- getInputFilePath(configpaths, folderPath = "extdata", filename = IFSpec$StaticFile)
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
  
  M_m <- standardizeandcastSatelliteTable(IFTable, model)
  # standardizeandcast prepares df for industries, convert to commodities
  M_m[, setdiff(model$Commodities$Code_Loc, colnames(M_m))] <- 0
  # Adjust column order to be the same with V_n rownames
  M_m <- M_m[, model$Commodities$Code_Loc]
  M_m <- as.matrix(M_m)

  return(M_m)
}
