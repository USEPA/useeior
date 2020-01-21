#' Write model components to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Only writes model economic components (DRC, Marketshares, Demand) for now.
#' @export
writeModelComponents <- function(model) {
  # Define output folder
  dir.create(paste("Model Builds/", model$specs$Model, sep = ""), recursive = TRUE) # meant to be flexible up to users
  outputfolder <- paste("Model Builds/", model$specs$Model, sep = "")
  
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
  dir.create("modelbuildlogs", recursive = TRUE) # meant to be flexible up to users
  logfilename <- paste0("modelbuildlogs/", model$specs$Model, logtimestamp, ".log")
  logging::addHandler(logging::writeToFile, file = logfilename, level = "INFO")
  
  logging::loginfo("Model components written to Model Build folder.")
}
