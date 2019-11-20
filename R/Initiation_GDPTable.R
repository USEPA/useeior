#' Load GDP tables in a list based on model config.
#' @param specs Specifications of the model.
#' @return A list with GDP tables.
loadGDPtables <- function(specs) {
  GDP <- list()
  logging::loginfo("Initializing GDP tables...")

  # Load pre-saved GrossOutput and CPI tables
  # GrossOutput
  GrossOutputData <- paste0(specs$BaseIOLevel, "_GrossOutput_IO")
  GDP$BEAGrossOutputIO <- get(GrossOutputData)
  GDP$BEAGrossOutputIO <- GDP$BEAGrossOutputIO * 1E6 # data frame, values are in dollars ($)
  # CPI
  CPIData <- paste0(specs$BaseIOLevel, "_CPI_IO")
  GDP$BEACPIIO <- get(CPIData)

  return(GDP)
}
