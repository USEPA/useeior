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

  A_proc <- reshape2::acast(model$HybridizationSpecs$TechFileDF,
                            FlowID ~ paste(ProcessID, Location, sep='/'), fun.aggregate = sum, value.var = "Amount")
  A_merged <- merge(A_proc, A, by="row.names", all=TRUE)
  A_merged[is.na(A_merged)] <- 0
  
  A <- as.matrix(A_merged[-1])
  rownames(A) <- A_merged[,1]
  
  # Reorder A such that process matrix (Ap) is in the upper left corner,
  # This will drop unmapped processes
  A <- A[match(colnames(A), rownames(A)), ]

  return(A)
}


#' Hybridize the environmental matrix based on specified source file
#' @param model An EEIO model object with model specs, IO tables, and satellite tables loaded
#' @return The B matrix for a hybridized model.
hybridizeBMatrix <- function (model){
  logging::loginfo("Hybridizing model for B matrix...")
  B <- model$B
  
  df <- model$HybridizationSpecs$EnvFileDF
  df$Flow <- apply(df[, c("Flowable", "Context", "Unit")],
                   1, FUN = joinStringswithSlashes)
  df$Code_Loc <- apply(df[, c("ProcessID", "Location")],
                       1, FUN = joinStringswithSlashes)
  B_proc <- reshape2::acast(df, Flow ~ Code_Loc, fun.aggregate = sum, value.var = "Amount")
  B_merged <- merge(B_proc, model$B, by="row.names", all=TRUE)
  B_merged[is.na(B_merged)] <- 0
  
  B <- as.matrix(B_merged[-1])
  rownames(B) <- B_merged[,1]

  # Some processes may not exist in B matrix file, make sure they are added as 0 columns
  B <- B[,match(colnames(model$A), colnames(B))]
  colnames(B) <- colnames(model$A)
  B[is.na(B)] <- 0
  
  return(B)
}


#' Update remaining model objects following hybridization of matrices for consistency
#' @param model A completed EEIO model object
#' @return model with remaining objects expanded to include hybrid process data
hybridizeModelObjects <- function (model) {

  # Update flows table
  new_flows <- unique(model$HybridizationSpecs$EnvFileDF[colnames(model$SatelliteTables$flows)])
  new_flows$flow <- apply(new_flows[, c("Flowable", "Context", "Unit")], 
                          1, FUN = joinStringswithSlashes)
  model_flows <- apply(model$SatelliteTables$flows[, c("Flowable", "Context", "Unit")], 
                       1, FUN = joinStringswithSlashes)
  model$SatelliteTables$flows <- rbind(model$SatelliteTables$flows,
                                       subset(new_flows[colnames(model$SatelliteTables$flows)],
                                              !(new_flows$flow %in% model_flows)))
  row.names(model$SatelliteTables$flows) <- NULL

  process_cols <- c("ProcessID", "ProcessName", "ProcessUnit", "Location")
  new_processes <- unique(model$HybridizationSpecs$TechFileDF[process_cols])
  colnames(new_processes) <- c("Code", "Name", "Unit", "Location")
  new_processes["Code_Loc"] <- paste(new_processes$Code, new_processes$Location, sep="/")
  new_processes <- new_processes[order(new_processes$Code_Loc),]
  model$HybridizationSpecs$Processes <- new_processes
  
  # Update Industry and Commodity tables
  new_processes[setdiff(names(model$Commodities), names(new_processes))] <- ""
  new_processes[setdiff(names(model$Industries), names(new_processes))] <- ""
  model$Commodities <- rbind(new_processes[colnames(model$Commodities)], model$Commodities)
  model$Industries <- rbind(new_processes[colnames(model$Industries)], model$Industries)

  # Set Margins to 0
  new_processes["SectorCode"] <- new_processes$Code_Loc
  new_processes[setdiff(names(model$Margins), names(new_processes))] <- 0
  model$Margins <- rbind(new_processes[colnames(model$Margins)], model$Margins)

  # Update MultiYear dfs, set to 0
  for (df in c('MultiYearCommodityOutput', 'MultiYearIndustryOutput',
               'MultiYearCommodityCPI', 'MultiYearIndustryCPI')){
    new_processes[setdiff(names(model[[df]]), names(new_processes))] <- 0
    rownames(new_processes) <- new_processes$Code_Loc
    model[[df]] <- rbind(new_processes[colnames(model[[df]])], model[[df]])
  }
    
  # Update matrices, set to 1
  for (table in c('Rho', 'Phi')){
    process_matrix <-  matrix(data = 1, nrow = nrow(new_processes), ncol = ncol(model[[table]]))
    rownames(process_matrix) <- new_processes$Code_Loc
    model[[table]] <- rbind(process_matrix, model[[table]])
  }

  # Expand demand vectors with values of 0
  process_demand <- vector(mode='numeric', length = nrow(new_processes))
  names(process_demand) <- new_processes$Code_Loc
  for (vector in names(model$DemandVectors$vectors)){
    model$DemandVectors$vectors[[vector]] <- c(process_demand, model$DemandVectors$vectors[[vector]])
  }

  # Expand q, x and mu with values of 0
  process_output <- vector(mode='numeric', length = nrow(new_processes))
  names(process_output) <- new_processes$Code_Loc
  model$q <- c(process_output, model$q)
  model$x <- c(process_output, model$x)
  model$mu <- c(process_output, model$mu)
  
  
  return(model)
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
                       system.file("extdata/hybridizationspecs", spec$EnvFile, package = "useeior"),
                       file.path(dirname(configpaths)[1], spec$EnvFile))
    model$HybridizationSpecs$EnvFileDF <- utils::read.table(filename,
                                                            sep = ",", header = TRUE,
                                                            stringsAsFactors = FALSE,
                                                            check.names = FALSE)
  return(model)
}
