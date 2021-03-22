#' Initialize model with specifications and fundamental crosswalk table.
#' @param modelname Name of the model from a config file.
#' @return A list of model specifications and fundamental crosswalk table.
#' @export
initializeModel <- function(modelname) {
  startLogging()
  logging::loginfo("Begin model initialization...")
  model <- list()
  # Get model specs
  model$specs <- getModelConfiguration(modelname)
  # Get model crosswalk
  model$crosswalk <- get(paste0("MasterCrosswalk", model$specs$BaseIOSchema))
  model$crosswalk <- unique(model$crosswalk[, c("NAICS_2012_Code", colnames(model$crosswalk)[startsWith(colnames(model$crosswalk), "BEA")])])
  colnames(model$crosswalk) <- gsub(paste0("_", model$specs$BaseIOSchema, "|_Code"), "", colnames(model$crosswalk))
  return(model)
}