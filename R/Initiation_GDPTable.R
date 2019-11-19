#' Load GDP tables in a list based on model config.
#' @param specs Specifications of the model.
#' @return A list with GDP tables.
loadGDPtables <- function(specs) {
  GDP <- list()
  logging::loginfo("Initializing GDP tables...")

  # Load pre-saved GrossOutput and CPI tables
  # GrossOutput
  GrossOutputFile <- paste0("data/", specs$BaseIOLevel, "_GrossOutput_IO", ".rda")
  GDP$BEAGrossOutputIO <- get(load(GrossOutputFile))
  GDP$BEAGrossOutputIO <- GDP$BEAGrossOutputIO * 1E6 # data frame, values are in dollars ($)
  # CPI
  CPIFile <- paste0("data/", specs$BaseIOLevel, "_CPI_IO", ".rda")
  GDP$BEACPIIO <- get(load(CPIFile))

  return(GDP)
}
