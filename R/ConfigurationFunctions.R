#Handle configuration files

#' Gets a stored configuration file
#' @param modelname, str, the name of the model
#' @param spectype, str, specification type, either "model","disagg" or "newTech"
#' @return A list of model specifications.
getConfiguration <- function(modelname, spectype) {
  #browser()
  configname <- paste(modelname, ".yml", sep = "")
  configpath <- paste0("extdata/",spectype,"specs/")
  configfile <- system.file(configpath, configname, package="useeior")
  if (configfile == "") {
    config <- NA
  } else {
    config <- configr::read.config(configfile)
  }
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
  packdir <- system.file("extdata/modelspecs/", package="useeior")
  configfiles <- list.files(path=packdir,pattern="*\\.yml")
  return(configfiles)
}
