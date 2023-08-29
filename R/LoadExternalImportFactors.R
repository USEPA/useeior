# Functions for loading external import factors


# Functions for loading input-output tables

#' Prepare economic components of an EEIO form USEEIO model.
#' @param model An EEIO form USEEIO model object with model specs loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' If NULL, built-in config files are used.
#' @return A list with EEIO form USEEIO model economic components.
loadExternalImportFactors <- function(model, configpaths = NULL) {

  temp <- 2
  # Read in file with Import factors
  IFSpec <- model$specs$ImportFactors[[1]]
  IFTable <- utils::read.table(IFSpec$StaticFile, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  # Store meta data
  meta <- data.frame(matrix(nrow = 0, ncol = 5))
  cnames <- colnames(IFTable[1:5])
  colnames(meta) <- cnames
  meta[1,1] <- model$specs$BaseIOLevel
  meta[2:5] <- IFTable[1,2:5]
  
  # Format IFTable to match model$M
  IFTable['Flow'] <- paste(IFTable$Flowable, IFTable$Context, IFTable$Unit, sep = "/")

  #TODO: Convert from basic to producer price using TAU if possible
  
  if(model$specs$IODataSource =="stateior") {
    #TODO
  } else {
    # assumes that if IODataSource is not stateior, it is a one a region model
    IFTable['Location'] <- "US"
  }
  
  M_m <- standardizeandcastSatelliteTable(IFTable, model)
  M_m <- as.matrix(M_m)

  return(M_m)
}

