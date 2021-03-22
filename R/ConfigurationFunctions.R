#Handle configuration files

#' Gets a model configuration
#' @param modelname The name of the model.
#' @return A list of model specifications.
#' @export
getModelConfiguration <- function(modelname) {
  configname <- paste(modelname, ".yml", sep = "")
  configfile <- system.file("extdata", configname, package="useeior")
  try(config <- configr::read.config(configfile))
  return(config)
}

#'Show model names with configuration files
#'@return Prints model names.
#'@export
seeAvailableModels <- function() {
  configfiles <- findModelConfigurationFiles()
  modelnames <- substr(configfiles,0,nchar(configfiles)-4)
  print(modelnames)
}

#' Get model config files
#' @return vector of model config files
findModelConfigurationFiles <- function() {
  packdir <- system.file("extdata", package="useeior")
  configfiles <- list.files(path=packdir,pattern="*USEEIOv.*\\.yml")
  return(configfiles)
}
