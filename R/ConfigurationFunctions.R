#Handle configuration files

#' Load model configuration.
#' @param modelname The name of the model.
#' @return A list of model specifications.
loadConfig <- function(modelname) {
  configname <- paste(modelname, ".yml", sep = "")
  configfile <- system.file("extdata", configname, package="useeior")
  try(config <- configr::read.config(configfile))
  return(config)
}

#'Show model names with configuration files
#'@return Prints model names.
#'@export
seeAvailableModels <- function() {
  packdir <- system.file("extdata", package="useeior")
  configfiles <- list.files(path=packdir,pattern=".yml") 
  modelnames <- substr(configfiles,0,nchar(configfiles)-4)
  print(modelnames)
}