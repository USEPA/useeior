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

#' Setup the WIO specs based on the input files. 
#' This function is essentially a wrapper for disaggregateSetup() function, but included for clarifying the different code flows between the two model types.
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model object with the correct WIO specs.
getWIOFiles <- function (model, configpaths = NULL){ 
  
  model <- disaggregateSetup(model)

  return(model)
}

#' Build a WIO-style model by including data read from wiospecs folder
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the UseTransactions matrix modified with WIO specs.
assembleWIOModel <- function (model){
  temp <- 1
  
  #model$WasteTreatmentIndustries <- subset of model$NAICSSectorCW whose category column matches "Waste Treatment Industries"
  #model$WasteTreatmentCommodities <- subset of model$NAICSSectorCW whose category column matches "Waste Treatment Commodities"
  #model$WasteGenTreat <- subset of model$NAICSSectorCW whose category column matches "Waste Generation by Treatment"
  #model$WasteGenMass <- subset of model$NAICSSectorCW whose category column matches "Waste Generation by Mass"
  #model$RecyclingTreat <- subset of model$NAICSSectorCW whose category column matches "Recycling by Treatment"
  #model$RecyclingnMass <- subset of model$NAICSSectorCW whose category column matches "Recycling by Mass"
  
  # Main idea: fill out the different sections of the use table by looking for industry/commodity combinations that fall in to the model objects defined above (+ regular IO industries)
  
  # Fill out intersection of IO commodities by WasteTreatment Industries (U12)
  # Find rows in model$UseFileDF where commodity column has a code included in model$Commodities$Code_Loc and industry column has a code included in model$WasteTreatmentCommodities
  
  temp <- 2
  return(model)
}

#' Include the WIO elements of the Use table in the correct configuration
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the UseTransactions matrix modified with WIO specs.
includeUseWIO <- function (model){
  

  return(model)
}

#' Include the WIO elements of the Make table in the correct configuration
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the MakeTransactions matrix modified with WIO specs.
includeMakeWIO <- function (model){
  
  
  return(model)
}

