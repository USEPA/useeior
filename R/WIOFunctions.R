#' Obtain WIO specs from input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified WIO specs.
getWIOSpecs <- function (model, configpaths = NULL){
  
  model$WIOSpecs <- vector(mode='list')
  
  for (configFile in model$specs$WIOSpecs){
    logging::loginfo(paste0("Loading WIO specification file for ", configFile, "..."))
    config <- getConfiguration(configFile, "WIO", configpaths)
    
    if('WIO' %in% names(config)){
      model$WIOSpecs <- append(model$WIOSpecs, config$WIO)
    }
  }
  
  return(model)
}

#' Setup the WIO specs based on the input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model object with the correct WIO specs.
getWIOFiles <- function (model, configpaths = NULL){ 
  spec <- model$WIOSpecs
  # Load Tech file
  filename <- ifelse(is.null(configpaths),
                     system.file("extdata/wiospecs", spec$TechFile, package = "useeior"),
                     file.path(dirname(configpaths)[1], spec$TechFile))
  model$WIOSpecs$TechFileDF <- utils::read.table(filename,
                                                           sep = ",", header = TRUE,
                                                           stringsAsFactors = FALSE,
                                                           check.names = FALSE)
  
  # Load Env file
  filename <- ifelse(is.null(configpaths),
                     system.file("extdata/wiospecs", spec$EnvFile, package = "useeior"),
                     file.path(dirname(configpaths)[1], spec$EnvFile))
  model$WIOSpecs$EnvFileDF <- utils::read.table(filename,
                                                          sep = ",", header = TRUE,
                                                          stringsAsFactors = FALSE,
                                                          check.names = FALSE)
  return(model)
}
