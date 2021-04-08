#' Writes all model data and metadata components to the API
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param basedir Base directory to write the model components to
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
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param basedir Base directory to write the model components to
#' @description Writes master sector crosswalk out for the API in csv
#' @export
writeSectorCrosswalkforAPI <- function(model, basedir){
  dirs <- setWriteDirsforAPI(NULL,basedir)
  prepareWriteDirs(dirs)
  crosswalk <- model$crosswalk
  crosswalk$ModelSchema <- ""
  utils::write.csv(crosswalk, paste0(dirs$data, "/sectorcrosswalk.csv"),
                   na = "", row.names = FALSE, fileEncoding = "UTF-8")
  logging::loginfo(paste0("Sector crosswalk written to ", dirs$data, "."))
}

#' Write model matrices as CSV files to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param outputfolder A directory to write matrices out to
#' @description Writes model matrices as CSV files, including A, A_d, B, C, D, L, L_d, M, M_d, N, N_d, and Rho (CPI ratio matrix)
#' @export
writeModelMatrices <- function(model, outputfolder) {
  # Write model matrices to csv
  modelfolder <- file.path(outputfolder, model$specs$Model,"matrices")
  if (!dir.exists(modelfolder)) {
    dir.create(modelfolder, recursive = TRUE) 
  }
  for (matrix in c("A", "A_d", "B", "C", "D", "L", "L_d", "M", "M_d", "N", "N_d", "Rho")) {
    utils::write.csv(model[[matrix]], paste0(modelfolder, "/", matrix, ".csv"),
                     na = "", row.names = TRUE, fileEncoding = "UTF-8")
  }
  logging::loginfo(paste0("Model matrices written to ", modelfolder, "."))
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

#' Write model matrices as BIN files for API to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param modelfolder Directory to write the model components to
#' @description Writes model matrices, including Make, Use, A, A_d, B, C, D, L, L_d, M, N, Rho (CPI ratio matrix), x (Industry Output), and q (Commodity Output).
writeModelMatricesforAPI <- function(model,modelfolder) {
  # Write model matrices to .bin files for API
  MatricesforAPI <- c("MakeTransactions", "UseTransactions", "A", "A_d",
                      "B", "C", "D", "L", "L_d", "M", "N", "Rho")
  for (matrix in MatricesforAPI) {
    writeMatrixasBinFile(as.matrix(model[[matrix]]), paste0(modelfolder, "/", matrix, ".bin"))
  }
  # Write x (Industry Output) or q (Commodity Output) to .bin files for API
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    writeMatrixasBinFile(as.matrix(model$CommodityOutput), paste0(modelfolder, "/q.bin"))
  } else {
    writeMatrixasBinFile(as.matrix(model$IndustryOutput), paste0(modelfolder, "/x.bin"))
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
  
  for (n in names(model$DemandVectors$vectors)) {
    f <- model$DemandVectors$vectors[[n]]
    f <- data.frame(amount=f)
    f$sector <- tolower(rownames(f))
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
#' @description Writes model metadata, including indicators and demands.
writeModelMetadata <- function(model,dirs) {
  #!WARNING: Only works for single region model
  if (model$specs$ModelType!="US") {
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
  Location <- model$specs$PrimaryRegionAcronym
  Description <- ""
  #Add in sector schema for model
  if (is.null(model$specs$DisaggregationSpecs)) {
    Sector_Schema <- paste("BEA", model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Code", sep = "_")
  } else {
    Sector_Schema <- model$specs$Model
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
  sectors <- model[[gsub("y", "ies", model$specs$CommoditybyIndustryType)]]
  sectors$ID <- sectors$Code_Loc
  sectors$Location <- model$specs$PrimaryRegionAcronym
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
  s <- sessionInfo()
  f <- paste0(path,"/Rsessioninfo.txt")
  writeLines(capture.output(s), f)
}
  
#' Write selected model matrices and metadata as XLSX file to output folder
#' @param model, any model object
#' @param outputfolder A directory to write model matrices and metadata as XLSX file out to
#' @description Writes model matrices as XLSX file, including A, A_d, B, C, D, L, L_d, M, M_d, N, N_d, Rho (CPI ratio), Phi (Margin ratio),
#' demand vectors, and model metadata.
#' @return None
#' @export
writeModeltoXLSX <- function(model, outputfolder) {
  # List model matrices
  USEEIOtoXLSX_ls <- model[c("A", "A_d", "B", "C", "D", "L", "L_d", "M", "M_d", "N", "N_d", "Rho", "Phi")]
  USEEIOtoXLSX_ls$Rho <- USEEIOtoXLSX_ls$Rho[, match("2007", colnames(USEEIOtoXLSX_ls$Rho)):ncol(USEEIOtoXLSX_ls$Rho)]
  USEEIOtoXLSX_ls$Phi <- USEEIOtoXLSX_ls$Phi[, match("2007", colnames(USEEIOtoXLSX_ls$Phi)):ncol(USEEIOtoXLSX_ls$Phi)]
  USEEIOtoXLSX_ls[["U"]] <- dplyr::bind_rows(cbind(model$UseTransactions, model$FinalDemand), model$UseValueAdded)
  USEEIOtoXLSX_ls[c("V", "q")] <- model[c("MakeTransactions", "CommodityOutput")]
  USEEIOtoXLSX_ls[names(model$DemandVectors$vectors)] <- model$DemandVectors$vectors
  for (n in names(USEEIOtoXLSX_ls)) {
    if (class(USEEIOtoXLSX_ls[[n]])%in%c("matrix", "data.frame")) {
      USEEIOtoXLSX_ls[[n]] <- cbind.data.frame(as.data.frame(rownames(USEEIOtoXLSX_ls[[n]])), USEEIOtoXLSX_ls[[n]])
    } else {
      USEEIOtoXLSX_ls[[n]] <- cbind.data.frame(names(USEEIOtoXLSX_ls[[n]]), USEEIOtoXLSX_ls[[n]])
      colnames(USEEIOtoXLSX_ls[[n]])[2] <- n
    }
    colnames(USEEIOtoXLSX_ls[[n]])[1] <- ""
  }
  # List model metadata
  basedir <- file.path(rappdirs::user_data_dir(), "USEEIO", "Model_Builds", model$specs$Model)
  metadata_dir <- file.path(basedir, "build", "data", model$specs$Model)
  if (!dir.exists(metadata_dir)) {
    dirs <- setWriteDirsforAPI(model, basedir)
    writeModelMetadata(model, dirs)
  }
  for (file in list.files(path = metadata_dir, pattern = "*.csv")) {
    df_name <- gsub(".csv", "", file)
    USEEIOtoXLSX_ls[[df_name]] <- utils::read.table(paste(metadata_dir, file, sep = "/"),
                                                    sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  }
  # List reference tables
  USEEIOtoXLSX_ls[["SectorCrosswalk"]] <- model$crosswalk
  
  # Write to Excel workbook
  writexl::write_xlsx(USEEIOtoXLSX_ls, paste0(outputfolder, "/", model$specs$Model, "_Matrices.xlsx"),
                      format_headers = FALSE)
  logging::loginfo(paste0("Model matrices written to Excel workbook (.xlsx) in", outputfolder, "."))
}  

