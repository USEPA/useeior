#' Initialize model with specifications and fundamental crosswalk table.
#' @param modelname Name of the model from a config file.
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A list of model specifications and fundamental crosswalk table.
initializeModel <- function(modelname, configpaths = NULL) {
  startLogging()
  logging::loginfo("Begin model initialization...")
  model <- list()
  # Get model specs
  model$specs <- getConfiguration(modelname, "model", configpaths)
  if (rlang::is_na(model$specs)) {
    logging::logerror(paste("No configuration exists for a model named",modelname))
    stop()
  } else {
    # Get model crosswalk
    model$crosswalk <- get(paste0("MasterCrosswalk", model$specs$BaseIOSchema),
                           as.environment("package:useeior"))
    model$crosswalk <- unique(model$crosswalk[, c("NAICS_2012_Code", colnames(model$crosswalk)[startsWith(colnames(model$crosswalk), "BEA")])])
    colnames(model$crosswalk) <- gsub(paste0("_", model$specs$BaseIOSchema, "|_Code"), "", colnames(model$crosswalk))
    rownames(model$crosswalk) <- NULL
    # Assign initial model crosswalk based on base schema
    modelschema <- "USEEIO"
    baseschema <- paste0("BEA_", model$specs$BaseIOLevel)
    model$crosswalk[modelschema] <- model$crosswalk[baseschema]
  }
  return(model)
}