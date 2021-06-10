# Functions for loading gross output and chain-type price indices

#' Load US Gross Output table based on model specifications.
#' @param specs Specifications of the model.
#' @return A data.frame of US Gross Output.
loadNationalGrossOutputTable <- function(specs) {
  logging::loginfo("Initializing Gross Output tables...")
  # Load pre-saved Gross Output tables
  GrossOutput <- get(paste0(specs$BaseIOLevel, "_GrossOutput_IO")) * 1E6 # data frame, values are in dollars ($)
  return(GrossOutput)
}

#' Load Chain Price Index table based on model specifications.
#' @param specs Specifications of the model.
#' @return A data.frame of Chain Price Index.
loadChainPriceIndexTable <- function(specs) {
  logging::loginfo("Initializing Chain Price Index tables...")
  ChainPriceIndex <- get(paste0(specs$BaseIOLevel, "_CPI_IO"))
  return(ChainPriceIndex)
}
