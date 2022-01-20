#' Hybridize the technology matrix based on specified source file
#' @param model An EEIO model object with model specs, IO tables, and satellite tables loaded
#' @param domestic A logical value indicating whether to hybridize domestic matrix.
#' @return The A matrix for a hybridized model.
hybridizeAMatrix <- function (model, domestic = FALSE){
  logging::loginfo("Hybridizing model for A matrix...")
  if(domestic){
    A <- model$A_d
  }
  else {
    A <- model$A
  }
  
  return(A)
}


#' Hybridize the environmental matrix based on specified source file
#' @param model An EEIO model object with model specs, IO tables, and satellite tables loaded
#' @return The B matrix for a hybridized model.
hybridizeBMatrix <- function (model){
  logging::loginfo("Hybridizing model for B matrix...")
  return(model$B)
}


#' Obtain hybridization specs from input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified hybridization specs.
getHybridizationSpecs <- function (model, configpaths = NULL){
  
  model$HybridizationSpecs <- vector(mode='list')

  for (configFile in model$specs$HybridizationSpecs){
    logging::loginfo(paste0("Loading hybridization specification file for ", configFile, "..."))
    config <- getConfiguration(configFile, "hybridization", configpaths)
    
    if('Hybridization' %in% names(config)){
      model$HybridizationSpecs <- append(model$HybridizationSpecs, config$Hybridization)
    }
  }

  return(model)
}

#' Setup the hybridization specs based on the input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model object with the correct hybridization specs.
getHybridizationFiles <- function (model, configpaths = NULL){ 
    spec <- model$HybridizationSpecs
    # Load Tech file
    filename <- ifelse(is.null(configpaths),
                       system.file("extdata/hybridizationspecs", spec$TechFile, package = "useeior"),
                       file.path(dirname(configpaths)[1], spec$TechFile))
    model$HybridizationSpecs$TechFileDF <- utils::read.table(filename,
                                                             sep = ",", header = TRUE,
                                                             stringsAsFactors = FALSE,
                                                             check.names = FALSE)

    # Load Env file
    filename <- ifelse(is.null(configpaths),
                       system.file("extdata/disaggspecs", spec$EnvFile, package = "useeior"),
                       file.path(dirname(configpaths)[1], spec$EnvFile))
    model$HybridizationSpecs$EnvFileDF <- utils::read.table(filename,
                                                            sep = ",", header = TRUE,
                                                            stringsAsFactors = FALSE,
                                                            check.names = FALSE)
  return(model)
}
