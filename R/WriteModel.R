#' Write model components to output folder using IO-Model-Builder format.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Only writes model economic components (DRC, Marketshares, Demand) for now.
#' @export
writeModelComponents <- function(model) {
  # Define output folder
  user_dir <- rappdirs::user_data_dir()
  outputfolder <- file.path(user_dir,"USEEIO","Model_Builds", model$specs$Model)
  if (!dir.exists(outputfolder)) {
    dir.create(outputfolder, recursive = TRUE) 
  }
  
  # Sat Tables
  sattable <- do.call(rbind.data.frame, model$SatelliteTables$coeffs_by_sector)
  # LCIA
  LCIA <- formatLCIAforIOMB(model)
  # Sector meta data
  SectorMetaData <- formatSectorMetaDataforIOMB(model)
  # Demand
  if(model$specs$PrimaryRegionAcronym=="US") {
    Demand <- getUSTotalConsProd(model)
    #add in food system demand
    #Demand <- addDemandforSubsystem("food",Demand)
  }
  # Format DRC for IOMB
  DirectRequirementsCoefficients <- formatIOTableforIOMB(model$A, model)
  
  # write model build components to csv
  name_pre <- paste(outputfolder, model$specs$Model,sep="/")
  utils::write.csv(sattable, paste0(name_pre, "_sat.csv"), na = "", row.names = FALSE, fileEncoding="UTF-8") 
  utils::write.csv(LCIA, paste0(name_pre, "_LCIA.csv"), row.names = FALSE, fileEncoding="UTF-8")
  utils::write.csv(SectorMetaData, paste0(name_pre, "_sector_meta_data.csv"), row.names = FALSE, fileEncoding="UTF-8")
  utils::write.csv(Demand, paste0(name_pre, "_FinalDemand.csv"), row.names = FALSE, fileEncoding="UTF-8")
  utils::write.csv(DirectRequirementsCoefficients, paste0(name_pre,"_DRC.csv"), row.names = TRUE, fileEncoding="UTF-8") #DRC needs row indices
  
  # Write logs to file in Model Builds folder
  logtimestamp <- Sys.Date()
  #if (!dir.exists())
  #dir.create("modelbuildlogs", recursive = TRUE) # meant to be flexible up to users
  #logfilename <- paste0("modelbuildlogs/", model$specs$Model, logtimestamp, ".log")
  #logging::addHandler(logging::writeToFile, file = logfilename, level = "INFO")
  
  logging::loginfo(paste0("Model components written to ",outputfolder," ."))
}
