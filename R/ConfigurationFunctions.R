#Handle configuration files

#' Gets a stored or user specified model or aggregation/disaggregation configuration file
#' @param configname str, name of the configuration file
#' @param configtype str, configuration type, can be "model", "disagg", or "agg"
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @param pkg str, indicate package for access to config, either "useeior" or "stateior"
#' @return A list of model specifications.
getConfiguration <- function(configname, configtype, configpaths = NULL, pkg="useeior") {
  configfile <- paste0(configname, ".yml")
  if (is.null(configpaths)) {
    configpath <- system.file(paste0("extdata/", configtype, "specs/"), configfile, package = pkg)
  } else {
    configpath <- configpaths[endsWith(configpaths, paste0("/", configfile))]
    if (length(configpath) == 0) {
      # Specific input file not found in configpaths, assume it is in useeior
      configpath <- system.file(paste0("extdata/", configtype, "specs/"), configfile, package = "useeior")
    }
    
    if (!file.exists(configpath)) {
      stop(paste(configfile, "must be available in ", dirname(configpath)),
           call. = FALSE)
    }
  }
  config <- configr::read.config(configpath)
  if (typeof(config) == "logical" && config == FALSE) {
    logging::logwarn(paste0("Configuration not found for ", configname))
    return(NULL)
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
