# Functions for exporting the model to disc

#' The vector of matrices to write out
matrices <- c("V", "U", "U_d", "A", "A_d", "B", "C", "D", "L", "L_d",
              "M", "M_d", "N", "N_d", "Rho", "Phi")

#' Writes all model data and metadata components to the API
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param basedir Base directory to write the model components to
#' @description Writes all model data and metadata components to the API
#' @export
writeModelforAPI <-function(model, basedir){
  dirs <- setWriteDirsforAPI(model,basedir)
  prepareWriteDirs(dirs,model)
  writeModelMatrices(model,"bin",dirs$model)
  writeModelDemandstoJSON(model,dirs$demands)
  writeModelMetadata(model,dirs)
}

#' Write the model sector crosswalk as .csv file
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param basedir Base directory to write the model components to
#' @description Writes the model sector crosswalk as .csv file
#' @export
writeSectorCrosswalk <- function(model, basedir){
  dirs <- setWriteDirsforAPI(NULL,basedir)
  prepareWriteDirs(dirs)
  crosswalk <- prepareModelSectorCrosswalk(model)
  crosswalk$ModelSchema <- ""
  utils::write.csv(crosswalk, paste0(dirs$data, "/sectorcrosswalk.csv"),
                   na = "", row.names = FALSE, fileEncoding = "UTF-8")
  logging::loginfo(paste0("Sector crosswalk written as sectorcrosswalk.csv to ", dirs$data, "."))
}

#' Write model matrices as .csv or .bin files to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param to_format A string specifying the format of write-to file, can be "csv" or "bin".
#' @param outputfolder A directory to write matrices out to
#' @description Writes model matrices as .csv or .bin files to output folder.
#' @export
writeModelMatrices <- function(model, to_format, outputfolder) {
  if (to_format=="csv") {
    modelfolder <- file.path(outputfolder, model$specs$Model, "matrices")
    if (!dir.exists(modelfolder)) {
      dir.create(modelfolder, recursive = TRUE) 
    }
    for (matrix in matrices) {
      utils::write.csv(model[[matrix]], paste0(modelfolder, "/", matrix, ".csv"),
                       na = "", row.names = TRUE, fileEncoding = "UTF-8")
    }
  } else if (to_format=="bin") {
    modelfolder <- outputfolder
    for (matrix in matrices) {
      writeMatrixasBinFile(as.matrix(model[[matrix]]),
                           paste0(modelfolder, "/",matrix, ".bin"))
      
    }
    # Write x (Industry Output) or q (Commodity Output) to .bin files
    writeMatrixasBinFile(as.matrix(model$q), paste0(modelfolder, "/q.bin"))
    writeMatrixasBinFile(as.matrix(model$x), paste0(modelfolder, "/x.bin"))
  }
  logging::loginfo(paste0("Model matrices written to ", modelfolder, "."))
}

#' Write selected model matrices, demand vectors, and metadata as XLSX file to output folder
#' @param model, any model object
#' @param outputfolder A directory to write model matrices and metadata as XLSX file out to
#' @description Writes model matrices demand vectors, and metadata as XLSX file to output folder.
#' @export
writeModeltoXLSX <- function(model, outputfolder) {
  # List model matrices
  USEEIOtoXLSX_ls <- model[matrices]
  # Write commodity and industry output
  USEEIOtoXLSX_ls$q <- model$q
  USEEIOtoXLSX_ls$x <- model$x
  
  # List model demand vectors
  USEEIOtoXLSX_ls[names(model$DemandVectors$vectors)] <- model$DemandVectors$vectors
  # Format tables
  for (n in names(USEEIOtoXLSX_ls)) {
    if (class(USEEIOtoXLSX_ls[[n]])%in%c("matrix", "data.frame")) {
      USEEIOtoXLSX_ls[[n]] <- cbind.data.frame(as.data.frame(rownames(USEEIOtoXLSX_ls[[n]])), USEEIOtoXLSX_ls[[n]])
    } else {
      USEEIOtoXLSX_ls[[n]] <- cbind.data.frame(names(USEEIOtoXLSX_ls[[n]]), USEEIOtoXLSX_ls[[n]])
      colnames(USEEIOtoXLSX_ls[[n]])[2] <- n
    }
    colnames(USEEIOtoXLSX_ls[[n]])[1] <- ""
  }
  
  # List model metadata: demands, flows, indicators
  basedir <- file.path(rappdirs::user_data_dir(), "useeior", "Model_Builds", model$specs$Model)
  metadata_dir <- file.path(basedir, "build", "data", model$specs$Model)
  if (!dir.exists(metadata_dir)) {
    dirs <- setWriteDirsforAPI(model, basedir)
    prepareWriteDirs(dirs, model)
    writeModelMetadata(model, dirs)
  }
  for (df_name in c("demands", "flows", "indicators", "sectors")) {
    filename <- paste0(df_name, ".csv")
    USEEIOtoXLSX_ls[[df_name]] <- utils::read.table(paste(metadata_dir, filename, sep = "/"),
                                                    sep = ",", header = TRUE,
                                                    stringsAsFactors = FALSE,
                                                    check.names = FALSE)
  }
  
  # List commodities/industries meta
  sector_meta_name <- paste0(tolower(gsub("y", "ies",
                                          model$specs$CommodityorIndustryType)),
                             "_meta")
  USEEIOtoXLSX_ls[[sector_meta_name]] <- USEEIOtoXLSX_ls$sectors
  
  # List final demand meta
  final_demand_meta <- model$FinalDemandMeta
  final_demand_meta$Index <- c(1:nrow(final_demand_meta)-1)
  final_demand_meta$ID <- final_demand_meta$Code_Loc
  final_demand_meta$Location <- model$specs$ModelRegionAcronyms
  final_demand_meta$Description <- ""
  USEEIOtoXLSX_ls[["final_demand_meta"]] <- final_demand_meta[, colnames(USEEIOtoXLSX_ls$sectors)]
  
  # List value added meta
  value_added_meta <- model$ValueAddedMeta
  value_added_meta$Index <- c(1:nrow(value_added_meta)-1)
  value_added_meta$ID <- value_added_meta$Code_Loc
  value_added_meta$Location <- model$specs$ModelRegionAcronyms
  value_added_meta$Description <- ""
  USEEIOtoXLSX_ls[["value_added_meta"]] <- value_added_meta[, colnames(USEEIOtoXLSX_ls$sectors)]
  
  # Remove USEEIOtoXLSX_ls$sectors
  USEEIOtoXLSX_ls$sectors <- NULL
  
  # List reference tables
  USEEIOtoXLSX_ls[["SectorCrosswalk"]] <- prepareModelSectorCrosswalk(model)
  
  # Write to Excel workbook
  writexl::write_xlsx(USEEIOtoXLSX_ls, paste0(outputfolder, "/", model$specs$Model, "_Matrices.xlsx"),
                      format_headers = FALSE)
  logging::loginfo(paste0("Model matrices written to Excel workbook (.xlsx) in /", outputfolder, "."))
}  

###All functions below here are internal

#' Sets directories to write model output data to
#' @param model A complete EEIO model: a list with USEEIO model components and attributes. Optional
#' @param basedir Base directory to write the model components to
#' @description Sets directories to write model output data to. If model is not passed, just sets data directory. 
#' @return A named list of directories for model output writing
setWriteDirsforAPI <- function(model=NULL, basedir) {
  dirs <- list()
  dirs$data <-  file.path(basedir,"build","data")
  if (!is.null(model)) {
    dirs$model <- file.path(dirs$data, model$specs$Model)
    dirs$demands <- file.path(dirs$model,"demands")    
  }
  return(dirs)
}

#' Sets directories to write model output data to
#' @param dirs A named list of directories
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @description Sets directories to write model output data to. If model is not passed, just uses data directory 
prepareWriteDirs <- function(dirs,model) {
  if (missing(model)) {
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

#' Write model demand vectors as JSON files for API to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param demandsfolder Path to output folder.
#' @description Writes model demand vectors, including y and y_d for consumption and production.
writeModelDemandstoJSON <- function(model, demandsfolder) {
  #!WARNING: Only works for single region model
  if (model$specs$ModelRegionAcronyms!="US") {
    logging::logerror("Currently only works for single region US models.")
    stop()
  }
  
  for (n in names(model$DemandVectors$vectors)) {
    f <- model$DemandVectors$vectors[[n]]
    f <- data.frame(amount=f)
    f$sector <- rownames(f)
    rownames(f) <- NULL
    f <- f[, c("sector", "amount")]
    f <- jsonlite::toJSON(f, pretty = TRUE)
    write(f, paste0(demandsfolder, "/", n, ".json"))
  }
  logging::loginfo(paste0("Model demand vectors for API written to ", demandsfolder, "."))
}

#' Write model metadata (indicators and demands, sectors, and flows) as CSV files to output folder
#' format for file is here https://github.com/USEPA/USEEIO_API/blob/master/doc/data_format.md
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param dirs A named list of directories
#' @description Writes model metadata, including indicators and demands.
writeModelMetadata <- function(model, dirs) {
  #!WARNING: Only works for single region model
  if (model$specs$ModelRegionAcronyms!="US") {
    logging::logerror("Currently only works for single region US models.")
    stop()
  }
  
  # Load metadata fields for API
  fields <- configr::read.config(system.file("extdata/USEEIO_API_fields.yml", package="useeior"))
  # Define output folder
  outputfolder <- dirs$model
  
  # Write model description to models.csv
  model_desc <- file.path(dirs$data, "models.csv")
  ID <- model$specs$Model
  Name <- model$specs$Model
  Location <- model$specs$ModelRegionAcronyms
  Description <- ""
  #Add in sector schema for model
  if (is.null(model$specs$DisaggregationSpecs)) {
    Sector_Schema <- paste("BEA", model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Code", sep = "_")
  } else {
    Sector_Schema <- generateModelSectorSchema(model)
  }
  Hash <- generateModelIdentifier(model)
  model_fields <- list("ID"=ID, "Name"=Name, "Location"=Location, "Description"=Description, "Sector_Schema"=Sector_Schema, "Hash"=Hash)
  if (!file.exists(model_desc)) {
    df <- cbind.data.frame(model_fields)
  } else {
    df <- utils::read.table(model_desc, sep = ",", header = TRUE,
                            stringsAsFactors = FALSE, check.names = FALSE)
    if (!ID%in%df$ID) {
      df <- rbind(df, cbind.data.frame(model_fields))
    }
  }
  utils::write.csv(df, model_desc, na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write indicators to indicators.csv
  indicators <- model$Indicators$meta  
  indicators$ID <- apply(indicators[, c("Group", "Code", "Unit")], 1, FUN = joinStringswithSlashes)
  indicators <- indicators[order(indicators$Name), ]
  indicators$Index <- c(1:nrow(indicators)-1)
  indicators <- indicators[,fields$indicators]
  checkNamesandOrdering(indicators$Name, rownames(model$C), "code in indicators.csv and rows in C matrix")
  utils::write.csv(indicators, paste0(outputfolder, "/indicators.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write demands to demands.csv
  demands <- model$DemandVectors$meta
  demands$Index <- c(1:nrow(demands)-1)
  demands <- demands[,fields$demands]
  utils::write.csv(demands, paste0(outputfolder, "/demands.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write sectors to csv
  sectors <- model[[gsub("y", "ies", model$specs$CommodityorIndustryType)]]
  sectors$ID <- sectors$Code_Loc
  sectors$Location <- model$specs$ModelRegionAcronyms
  sectors$Description <- ""
  sectors$Index <- c(1:nrow(sectors)-1)
  sectors <- sectors[, fields$sectors]
  checkNamesandOrdering(sectors$ID, rownames(model$L), "code in sectors.csv and rows in L matrix")
  utils::write.csv(sectors, paste0(outputfolder, "/sectors.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write flows to csv
  flows <- model$SatelliteTables$flows
  flows$ID <- apply(flows[, c("Flowable", "Context", "Unit")], 1, FUN = joinStringswithSlashes)
  names(flows)[names(flows) == 'FlowUUID'] <- 'UUID'
  flows <- flows[order(flows$ID),]
  flows$Index <- c(1:nrow(flows)-1)
  flows <- flows[, fields$flows]
  checkNamesandOrdering(flows$ID, rownames(model$B), "flows in flows.csv and rows in B matrix")
  utils::write.csv(flows, paste0(outputfolder, "/flows.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write years to csv
  years <- data.frame(ID=colnames(model$Rho), stringsAsFactors = FALSE)
  years$Index <- c(1:length(years$ID)-1)
  years <- years[, fields$years]
  checkNamesandOrdering(years$ID, colnames(model$Phi), "years in years.csv and cols in Phi matrix")
  checkNamesandOrdering(years$ID, colnames(model$Rho), "years in years.csv and cols in Rho matrix")
  utils::write.csv(years, paste0(outputfolder, "/years.csv"), na = "", row.names = FALSE, fileEncoding = "UTF-8")
  
  # Write session info to R sessioninfo.txt inside the model folder
  writeSessionInfotoFile(dirs$model)
  
  logging::loginfo(paste0("Model metadata written to ", outputfolder, "."))
}

#'Create a unique hash identifier for a model
#'@param model, any model object
#'@return char string
#'@export
generateModelIdentifier <- function(model) {
  id <- digest::digest(model, algo="sha256")
  return(id)
}

#' Write out session information to a "Rsessioninfo.txt file in the given path
#' @param path, str, a path without the file
#' @return None
writeSessionInfotoFile <- function(path) {
  s <- utils::sessionInfo()
  f <- paste0(path,"/Rsessioninfo.txt")
  writeLines(utils::capture.output(s), f)
}

#'Create sector schema for a model
#'@param model, any model object
#'@return char string
generateModelSectorSchema <- function(model) {
  SectorSchema <- paste0(paste(model$specs$IODataSource, 
                               model$specs$BaseIOSchema, 
                               model$specs$BaseIOLevel, 
                               gsub("Disaggregation.*", "",
                                    model$specs$DisaggregationSpecs),
                               "Disagg",sep = "_"))
  return(SectorSchema)
}

#' Prepare sectorcrosswalk table for a model
#' @param model, any model object
#' @return a data.frame, sectorcrosswalk table
prepareModelSectorCrosswalk <- function(model) {
  crosswalk <- model$crosswalk
  
  # Check for disaggregation
  if(!is.null(model$specs$DisaggregationSpecs)){
    #create name to use as column header for disaggregated schema
    SectorSchema <- generateModelSectorSchema(model)
    #deterime which rows and columns to modify
    cwColIndex <- match(paste0("BEA_", model$specs$BaseIOLevel), colnames(crosswalk))#search for concatenation of "BEA" and model$specs$BaseIOlevel object in crosswalk column names
    
    #copy relevant column over as last column
    crosswalk <- cbind(model$crosswalk, model$crosswalk[, paste0("BEA_", model$specs$BaseIOLevel)])
    colnames(crosswalk)[length(crosswalk)] <- SectorSchema #rename column
    
    #replace disaggregated sector codes in original column with original sector code (e.g. 562HAZ with 562000)
    for (disagg in model$DisaggregationSpecs$Disaggregation){
      OriginalCodeLength <- regexpr(pattern ='/',disagg$OriginalSectorCode) - 1 #used to determine the length of the sector codes. E.g., detail would be 6, while summary would generally be 3 though variable, and sector would be variable
      DisaggCodeLength <- regexpr(pattern ='/',disagg$DisaggregatedSectorCodes[[1]]) - 1 #used to determine length of disaggregated sector codes.
      
      disaggNAICSIndex <- crosswalk[,cwColIndex] %in% substr(disagg$DisaggregatedSectorCodes,1,DisaggCodeLength) #find all the indeces in crosswalk where there disaggregated codes
      crosswalk[disaggNAICSIndex,cwColIndex] <- substr(disagg$OriginalSectorCode,1,OriginalCodeLength) #replace the disaggregated codes with the original sector code
      
    }
    
    
  }
  
  return(crosswalk)
  
}
