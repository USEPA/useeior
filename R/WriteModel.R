#' Writes all model data and metadata components to the API
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Writes all model data and metadata components to the API
#' @export
writeModelforAPI <-function(model, basedir){
  dirs <- setWriteDirsforAPI(model,basedir)
  prepareWriteDirs(dirs,model)
  writeModelMatricesforAPI(model,dirs$model)
  writeModelDemandstoJSON(model,dirs$demands)
  writeModelMetadata(model,dirs)
}

#' Write the master sector crosswalk out for the API
#' @description Writes master sector crosswalk out for the API in csv
#' @export
writeSectorCrosswalkforAPI <- function(basedir){
  dirs <- setWriteDirsforAPI(model=NA,basedir)
  prepareWriteDirs(dirs)
  utils::write.csv(MasterCrosswalk2012, paste0(dirs$data, "/sectorcrosswalk.csv"),
                   na = "", row.names = FALSE, fileEncoding = "UTF-8")
  logging::loginfo(paste0("Sector crosswalk written to ", basedir, "."))
}

#' Write model matrices as CSV files to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param outputfolder A directory to write matrices out to
#' @description Writes model matrices, including A, B, C, D, L, U, and M.
#' @export
writeModelMatrices <- function(model, outputfolder) {
  # Write model matrices to csv
  modelfolder <- file.path(outputfolder, model$specs$Model,"matrices")
  if (!dir.exists(modelfolder)) {
    dir.create(modelfolder, recursive = TRUE) 
  }
  for (matrix in c("A", "B", "C", "D", "L", "U", "M")) {
    utils::write.csv(model[[matrix]], paste0(modelfolder,"/",matrix, ".csv"),
                     na = "", row.names = TRUE, fileEncoding = "UTF-8")
  }
  logging::loginfo(paste0("Model matrices written to ", modelfolder, "."))
}


#' Write model components to output folder for useeiopy building using IO-Model-Builder format.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param modelfolder Directory to write the model components to
#' @description Only writes model economic components (DRC, Marketshares, Demand) for now.
#' @export
writeModelforUSEEIOPY <- function(model) {
  user_dir <- rappdirs::user_data_dir()
  modelfolder <- file.path(user_dir, "USEEIO", "Model_Builds", model$specs$Model)
  if (!dir.exists(modelfolder)) {
    dir.create(modelfolder, recursive = TRUE) 
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
  # Write model build components to csv
  name_pre <- paste(modelfolder, model$specs$Model, sep = "/")
  utils::write.csv(sattable, paste0(name_pre, "_sat.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8") 
  utils::write.csv(LCIA, paste0(name_pre, "_LCIA.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(SectorMetaData, paste0(name_pre, "_sector_meta_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(f, paste0(name_pre, "_FinalDemand.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(DirectRequirementsCoefficients, paste0(name_pre, "_DRC.csv"), row.names = TRUE, fileEncoding = "UTF-8") #DRC needs row indices
  # Write logs to file in Model Builds folder
  logtimestamp <- Sys.Date()
  #if (!dir.exists())
  #dir.create("modelbuildlogs", recursive = TRUE) # meant to be flexible up to users
  #logfilename <- paste0("modelbuildlogs/", model$specs$Model, logtimestamp, ".log")
  #logging::addHandler(logging::writeToFile, file = logfilename, level = "INFO")
  logging::loginfo(paste0("Model components written to ",modelfolder," ."))
}


###All functions below here are internal

#' Sets directories to write model output data to
#' @param model A complete EEIO model: a list with USEEIO model components and attributes. Optional
#' @description Sets directories to write model output data to. If model is not passed, just sets data directory. 
#' @return A named list of directories for model output writing
setWriteDirsforAPI <- function(model=NA, basedir) {
  dirs <- list()
  dirs$data <-  file.path(basedir,"build","data")
  if (!is.na(model)) {
    dirs$model <- file.path(dirs$data, model$specs$Model)
    dirs$demands <- file.path(dirs$model,"demands")    
  }
  return(dirs)
}

#' Sets directories to write model output data to
#' @param dirs A named list of directories
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Sets directories to write model output data to. If model is not passed, just uses data directory 
prepareWriteDirs <- function(dirs,model=NA) {
  if (is.na(model)) {
    if (!dir.exists(dirs$data)) {
      dir.create(dirs$data, recursive = TRUE) 
    }
  } else {
    if (!dir.exists(dirs$model)) {
      dir.create(dirs$model, recursive = TRUE) 
    }
    if (!dir.exists(dirs$demands)) {
      dir.create(dirs$demands, recursive = TRUE) 
    }
  }
}

#' Write model matrices as BIN files for API to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param modelfolder Directory to write the model components to
#' @description Writes model matrices, including A, B, C, D, L, U, M, CPI, x (Industry Output), and q (Commodity Output).
writeModelMatricesforAPI <- function(model,modelfolder) {
  # Write model matrices to .bin files for API
  MatricesforAPI <- c("A", "A_d", "B", "C", "D", "L","L_d","U", "M", "CPI")
  for (matrix in MatricesforAPI) {
    writeMatrixasBinFile(model[[matrix]], paste0(modelfolder, "/", matrix, ".bin"))
  }
  # Write x (Industry Output) or q (Commodity Output) to .bin files for API
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    writeMatrixasBinFile(model$CommodityOutput, paste0(modelfolder, "/q.bin"))
  } else {
    writeMatrixasBinFile(model$CommodityOutput, paste0(modelfolder, "/x.bin"))
  }
  logging::loginfo(paste0("Model matrices for API written to ", modelfolder, "."))
}

#' Write model demand vectors as JSON files for API to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Writes model demand vectors, including y and y_d for consumption and production.
writeModelDemandstoJSON <- function(model,demandsfolder) {
  #!WARNING: Only works for single region model
  if (model$specs$ModelType!="US") {
    logging::logerror("Currently only works for single region US models.")
    stop()
  }
  for (demand in names(model$demands)) {
    f <- model$demands[[demand]]
    f <- data.frame(amount=f)
    # Add sector name
    f <- merge(model$SectorNames, f, by.x = "Sector", by.y = 0)
    f$sector <- apply(cbind(f[, c("Sector", "SectorName")], model$specs$PrimaryRegionAcronym),
                           1, FUN = joinStringswithSlashes)
    f <- f[, c("sector", "amount")]
    filename <- tolower(paste(model$specs$IOYear, model$specs$PrimaryRegionAcronym, demand, sep = "_"))
    f <- jsonlite::toJSON(f, pretty = TRUE)
    write(f, paste0(demandsfolder, "/", filename, ".json"))
  }
  logging::loginfo(paste0("Model demand vectors for API written to ", demandsfolder, "."))
}

#' Write model metadata (indicators and demands, sectors, and flows) as CSV files to output folder
#' format for file is here https://github.com/USEPA/USEEIO_API/blob/master/doc/data_format.md
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Writes model metadata, including indicators and demands.
writeModelMetadata <- function(model,dirs) {
  outputfolder <- dirs$model
  # Write model description to models.csv
  model_desc <- file.path(dirs$data, "models.csv")
  ID <- model$specs$Model
  Name <- paste("A", substr(ID, 8, 10), "version", model$specs$PrimaryRegionAcronym,
                "EEIO model in", tolower(model$specs$CommoditybyIndustryType),
                "form at the BEA", tolower(model$specs$BaseIOLevel), "level with",
                model$specs$SatelliteTable$GHG$DataYears, 
                names(model$specs$SatelliteTable), "data")
  Location <- model$specs$PrimaryRegionAcronym
  Description <- paste("A", substr(ID, 8, 10), "version", model$specs$PrimaryRegionAcronym,
                       "EEIO model in", tolower(model$specs$CommoditybyIndustryType),
                       "form at the BEA", tolower(model$specs$BaseIOLevel), "level with",
                       model$specs$SatelliteTable$GHG$DataYears, 
                       names(model$specs$SatelliteTable), "table and customzied",
                       names(model$specs$SatelliteTable), "indicators")
  #Add in sector schema for model
  Sector_Schema <- paste0("BEA_",model$specs$BaseIOSchema,"_",model$specs$BaseIOLevel,"_Code")
  if (!file.exists(model_desc)) {
    df <- cbind.data.frame(ID, Name, Location, Description, Sector_Schema)
  } else {
    df <- utils::read.table(model_desc, sep = ",", header = TRUE,
                            stringsAsFactors = FALSE, check.names = FALSE)
    if (!ID%in%df$ID) {
      df <- rbind(df, cbind.data.frame(ID, Name, Location, Description, Sector_Schema))
    }
  }
  utils::write.csv(df, model_desc, na = "", row.names = FALSE, fileEncoding = "UTF-8")

  # Write indicators to csv
  indicators <- model$Indicators$meta  
  indicators$ID <- apply(indicators[, c("Group", "Abbreviation", "Unit")],
                         1, FUN = joinStringswithSlashes)
  indicators$Index <- c(1:nrow(indicators)-1)
  indicators <- indicators[, c("Index", "ID", "FullName", "Abbreviation", "Unit", "Group", "SimpleUnit", "SimpleName")]
  
  api_indicator_fields <- c("Index", "ID", "Name", "Code", "Unit", "Group", "SimpleUnit", "SimpleName")
  colnames(indicators) <- api_indicator_fields
  utils::write.csv(indicators, paste0(outputfolder, "indicators.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write demands to csv
  demands <- as.data.frame(gsub(".json", "", list.files(paste0(outputfolder, "/demands"))),
                           stringsAsFactors = FALSE)
  colnames(demands) <- "ID"
  for (n in 1:nrow(demands)) {
    demands[n, "Year"] <- unlist(strsplit(demands[n, "ID"], "_"))[1]
    demands[n, "Type"] <- unlist(strsplit(demands[n, "ID"], "_"))[4]
    demands[n, "System"] <- ifelse(is.na(unlist(strsplit(demands[n, "ID"], "_"))[5]),
                                   "complete", unlist(strsplit(demands[n, "ID"], "_"))[5])
    demands[n, "Location"] <- toupper(unlist(strsplit(demands[n, "ID"], "_"))[2])
    demands[n, "Scope"] <- unlist(strsplit(demands[n, "ID"], "_"))[4]
  }
  utils::write.csv(demands, paste0(outputfolder, "/demands.csv"),
                   na = "", row.names = FALSE, fileEncoding = "UTF-8")
  # Write sectors to csv
  sectors <- model$SectorNames
  colnames(sectors) <- c("Code", "Name")
  sectors$Location <- model$specs$PrimaryRegionAcronym
  sectors$Index <- c(1:nrow(sectors)-1)
  sectors$ID <- apply(sectors[, c("Code", "Name", "Location")], 1, FUN = joinStringswithSlashes)
  sectors <- sectors[, c("Index", "ID", "Name", "Code", "Location")]
  sectors$Description <- ""
  utils::write.csv(sectors, paste0(outputfolder, "sectors.csv"),
                   na = "", row.names = FALSE, fileEncoding = "UTF-8")
  # Write flows to csv
  flows <- model$SatelliteTables$flows
  flows$Index <- c(1:nrow(flows)-1)
  flows$ID <- apply(flows[, c("Flowable", "Context", "Unit")], 1, FUN = joinStringswithSlashes)
  names(flows)[names(flows) == 'FlowUUID'] <- 'UUID'
  flows <- flows[, c("Index", "ID", "Flowable", "Context", "Unit", "UUID")]
  utils::write.csv(flows, paste0(outputfolder, "/flows.csv"),
                   na = "", row.names = FALSE, fileEncoding = "UTF-8")
  logging::loginfo(paste0("Model metadata written to ", outputfolder, "."))
}

