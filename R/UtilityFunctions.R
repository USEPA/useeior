
#' Start logging 
startLogging <- function (){
  #http://logging.r-forge.r-project.org/sample_session.php
  logging::basicConfig()
  # Define output folder
  #dir.create(paste("Model Builds/", modelname, sep = ""), recursive = TRUE) # meant to be flexible up to users
  #outputfolder <- paste("Model Builds/", modelname, sep = "")
  #Write logs to file in model build folder
  #logtimestamp = Sys.Date()
  #logfilename = paste(outputfolder,"/",logtimestamp,".log",sep="")
  #addHandler(writeToFile, file=logfilename, level='INFO')
}

#' Join strings with slashes
joinStringswithSlashes <- function(...) {
  items <- list(...)
  str <- sapply(items, paste, collapse = '/')
  str <- tolower(str)
  return(str)
}

#' Aggregate matrix by rows then by columns
aggregateMatrix <- function (matrix, from_level, to_level, model) {
  # Determine the columns within MasterCrosswalk that will be used in aggregation
  from_code <- paste("BEA", model$specs$BaseIOSchema, from_level, "Code", sep = "_")
  to_code <- paste("BEA", model$specs$BaseIOSchema, to_level, "Code", sep = "_")
  # Aggregate by rows
  value_columns_1 <- colnames(matrix)
  df_fromlevel <- merge(matrix, unique(MasterCrosswalk2012[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
  df_fromlevel_agg <- aggregate(df_fromlevel[, value_columns_1], by = list(df_fromlevel[, to_code]), sum)
  rownames(df_fromlevel_agg) <- df_fromlevel_agg[, 1]
  df_fromlevel_agg[, 1] <- NULL
  # aggregate by columns
  value_columns_2 <- rownames(df_fromlevel_agg)
  df_fromlevel_agg <- merge(t(df_fromlevel_agg), unique(MasterCrosswalk2012[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
  matrix_fromlevel_agg <- aggregate(df_fromlevel_agg[, value_columns_2], by = list(df_fromlevel_agg[, to_code]), sum)
  # reshape back to orginal CxI (IxC) format
  rownames(matrix_fromlevel_agg) <- matrix_fromlevel_agg[, 1]
  matrix_fromlevel_agg <- t(matrix_fromlevel_agg[, -1])
  return(matrix_fromlevel_agg)
}
