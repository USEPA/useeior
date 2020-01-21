#' Write model components to output folder using IO-Model-Builder format.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Only writes model economic components (DRC, Marketshares, Demand) for now.
#' @export
writeModelComponents <- function(model) {
  # Define output folder
  
  user_dir <- rappdirs::user_data_dir()
  outputfolder <- file.path(user_dir,"USEEIO","Model_Builds", model$specs$Model)
  if (!dir.exists(model_dir)) {
    dir.create(outputfolder, recursive = TRUE) 
  }
  
  # Demand
  if(model$specs$PrimaryRegionAcronym=="US") {
    Demand <- getUSTotalConsProd(model)
    #add in food system demand
    #Demand <- addDemandforSubsystem("food",Demand)
  }
  # Format model build components for IOMB
  MarketShares <- formatIOTableforIOMB(model$V_n, model)
  DirectRequirementsCoefficients <- formatIOTableforIOMB(model$A, model)
  
  # write model build components to csv
  utils::write.csv(Demand, paste(outputfolder, "FinalDemand.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(MarketShares, paste(outputfolder, "MarketShares.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(DirectRequirementsCoefficients, paste(outputfolder, "DirectRequirementsCoefficients.csv", sep = "/"), row.names = FALSE)
  
  
  # Write logs to file in Model Builds folder
  logtimestamp <- Sys.Date()
  #if (!dir.exists())
  #dir.create("modelbuildlogs", recursive = TRUE) # meant to be flexible up to users
  #logfilename <- paste0("modelbuildlogs/", model$specs$Model, logtimestamp, ".log")
  #logging::addHandler(logging::writeToFile, file = logfilename, level = "INFO")
  
  logging::loginfo(paste0("Model components written to ",outputfolder," ."))
}
