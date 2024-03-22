# Functions for loading gross output and chain-type price indices

#' Load US Gross Output table based on model specifications.
#' @param specs Specifications of the model.
#' @return A data.frame of US Gross Output.
loadNationalGrossOutputTable <- function(specs) {
  logging::loginfo("Initializing Gross Output tables...")
  schema <- getSchemaCode(specs)
  # Load pre-saved Gross Output tables
  GrossOutput <- get(paste0(na.omit(c(specs$BaseIOLevel, "GrossOutput_IO", schema)),
                            collapse = "_")) * 1E6 # data frame, values are in dollars ($)
  return(GrossOutput)
}

#' Load Chain Price Index table based on model specifications.
#' @param specs Specifications of the model.
#' @return A data.frame of Chain Price Index.
loadChainPriceIndexTable <- function(specs) {
  logging::loginfo("Initializing Chain Price Index tables...")
  schema <- getSchemaCode(specs)
  ChainPriceIndex <- get(paste0(na.omit(c(specs$BaseIOLevel, "CPI_IO", schema)), collapse = "_"))
  return(ChainPriceIndex)
}
