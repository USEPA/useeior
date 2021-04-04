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
#'
#' @param ... text string
joinStringswithSlashes <- function(...) {
  items <- list(...)
  str <- sapply(items, paste, collapse = '/')
  return(str)
}

#' Aggregate matrix by rows then by columns
#'
#' @param matrix      A matrix
#' @param from_level  The level of BEA code this matrix starts at
#' @param to_level    The level of BEA code this matrix will be aggregated to
#' @param specs       Model specifications
aggregateMatrix <- function (matrix, from_level, to_level, specs) {
  # Determine the columns within MasterCrosswalk that will be used in aggregation
  from_code <- paste0("BEA_", from_level)
  to_code <- paste0("BEA_", to_level)
  # Aggregate by rows
  value_columns_1 <- colnames(matrix)
  df_fromlevel <- merge(matrix, unique(model$crosswalk[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
  df_fromlevel_agg <- stats::aggregate(df_fromlevel[, value_columns_1], by = list(df_fromlevel[, to_code]), sum)
  rownames(df_fromlevel_agg) <- df_fromlevel_agg[, 1]
  df_fromlevel_agg[, 1] <- NULL
  # aggregate by columns
  value_columns_2 <- rownames(df_fromlevel_agg)
  df_fromlevel_agg <- merge(t(df_fromlevel_agg), unique(model$crosswalk[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
  matrix_fromlevel_agg <- stats::aggregate(df_fromlevel_agg[, value_columns_2], by = list(df_fromlevel_agg[, to_code]), sum)
  # reshape back to orginal CxI (IxC) format
  rownames(matrix_fromlevel_agg) <- matrix_fromlevel_agg[, 1]
  matrix_fromlevel_agg <- t(matrix_fromlevel_agg[, -1])
  return(matrix_fromlevel_agg)
}

#' Generate Output Ratio table, flexible to Commodity/Industry output and model Commodity/Industry type
#'
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param output_type Either Commodity or Industry, default is Commodity
calculateOutputRatio <- function (model, output_type="Commodity") {
  # Generate Output based on output_type and model Commodity/Industry type 
  if (output_type=="Commodity") {
    Output <- model$CommodityOutput
  } else {
    Output <- model$IndustryOutput
  }
  # Map CommodityOutput to more aggregated IO levels
  Crosswalk <- unique(model$crosswalk[startsWith(colnames(model$crosswalk), "BEA")])
  ratio_table <- merge(Crosswalk, as.data.frame(Output, row.names = gsub("/.*", "", names(Output))),
                       by.x = paste0("BEA_", model$specs$BaseIOLevel), by.y = 0)
  # Calculate output ratios
  for (iolevel in c("Summary", "Sector")) {
    # Generate flexible sector_code
    sector_code <- paste0("BEA_", iolevel)
    # Sum Detail output to Summary/Sector
    output_sum <- stats::aggregate(ratio_table$Output, by = list(ratio_table[, sector_code]), sum)
    colnames(output_sum) <- c(sector_code, paste0(iolevel, "Output"))
    ratio_table <- merge(ratio_table, output_sum, by = sector_code)
    # Calculate DetailSummaryRatio and DetailSectorRatio
    ratio_table[, paste0("to", iolevel, "Ratio")] <- ratio_table$Output/ratio_table[, paste0(iolevel, "Output")]
    # Generate SectorCode column
    ratio_table$SectorCode <- ratio_table[, paste0("BEA_", model$specs$BaseIOLevel)]
  }
  # Keep ratio columns
  ratio_table <- unique(ratio_table[, c("SectorCode", "toSummaryRatio", "toSectorRatio")])
  if(model$specs$BaseIOLevel=="Sector") {
    ratio_table$toSectorRatio <- 1
    ratio_table <- unique(ratio_table[, c("SectorCode", "toSectorRatio")])
  }
  return(ratio_table)
}

#' Compare two matrices, calculate percentage difference (m1-m2)/m1.
#' Dimensions of the two matrices must be the same.
compareMatrices <- function(m1, m2, percentage_diff = FALSE) {
  if (dim(m1)!=dim(m2)) {
    stop("Make m1 and m2 have the same dimensions first.")
  }
  if (percentage_diff) {
    m <- (m1-m2)/m1
  } else {
    m <- m1-m2
  }
  m[is.na(m)] <- 0
  return(m)
}

#' Write matrix as bin file
writeMatrixasBinFile <- function(matrix, path) {
  out <- file(path, "wb")
  rows <- dim(matrix)[1]
  cols <- dim(matrix)[2]
  writeBin(as.integer(rows), out, size = 4, endian = "little")
  writeBin(as.integer(cols), out, size = 4, endian = "little")
  for (col in 1:cols) {
    for (row in 1:rows) {
      writeBin(as.double(matrix[row, col]), out, size = 8, endian = "little")
    }
  }
  close(out)
}

#' downloads files from the Data Commons and stores in a local temporary data directory
#' @param source The name of the source file (e.g. "TRACI_2.1_v1.parquet")
#' @param subdirectory The name of the package where the source file is stored on Data Commons including any subfolders (e.g. "lciafmt/traci_2_1")
#' @param debug_url The Data Commons base url, including directory and subdirectories
downloadDataCommonsfile <- function(source, subdirectory, debug_url) {
  
  directory <- paste0(rappdirs::user_data_dir(), "/", subdirectory)
  # Check for and create subdirectory if necessary
  if(!file.exists(directory)){
    dir.create(directory, recursive = TRUE)
  }
  
  # Download file
  download.file(paste0(debug_url, "/", source), paste0(directory, "/", source), mode = "wb", quiet = TRUE)
}

#' Load the static file originating from Data Commons either by loading from local directory or downloading from Data Commons and 
#' saving to local directory
#' @param static_file The name of a static file, including the subdirectories
#' @return The static file originating from Data Commons
loadDataCommonsfile <- function(static_file) {
  # load method name
  method_name <- static_file
  # define symbol to split method name
  pat <- "(.*)/(.*)"
  # subdirectory is the string of the method name prior to the last "/"
  subdirectory <- sub(pat, "\\1", method_name)
  # file name is the string of the method name after the last "/"
  file_name <- sub(pat, "\\2", method_name)
  
  # url for data commons
  debug_url <- paste0("https://edap-ord-data-commons.s3.amazonaws.com/", subdirectory)
  
  directory <- paste0(rappdirs::user_data_dir(), "/", subdirectory)
  
  # file must be saved in the local directory
  f <- paste0(directory,'/', file_name)
  
  if(!file.exists(f)){
    logging::loginfo(paste0("file not found, downloading from ", debug_url))
    downloadDataCommonsfile(file_name, subdirectory, debug_url)
  }
  return(f)
}

#' Maps a vector of FIPS codes to location codes
#' ! Placeholder only works for '00000' now
#' @param fipscodes A vector of 5 digit FIPS codes
#' @return A vector of location codes where matches are found
mapFIPS5toLocationCodes <- function(fipscodes) {
  mapping <- c('00000' = 'US')
  
  locations <- stringr::str_replace_all(string = fipscodes,pattern = mapping)
  return(locations)
}  
  
#' Replaces all `None` in a dataframe with the R NULL type NA
#' 
replaceNonewithNA <- function(df) {
  df[df=='None'] <- NA
  return(df)
}

#' Extract desired columns from SchemaInfo, return vectors with strings of codes.
#' @param iolevel Level of detail, can be "Sector", "Summary, "Detail".
#' @param colName A text value specifying desired column name.
#' @return A vector of codes.
getVectorOfCodes <- function(ioschema, iolevel, colName) {
  SchemaInfoFile <- paste(ioschema, iolevel, "Schema_Info.csv", sep = "_")
  SchemaInfo <- utils::read.table(system.file("extdata", SchemaInfoFile, package = "useeior"),
                                  sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  return(as.vector(stats::na.omit(SchemaInfo[, c("Code", colName)])[, "Code"]))
}

#' Calculate tolerance for RAS. Takes a target row sum vector and target colsum vector.
#' Specify either relative difference or absolute difference.
#' @param t_r A vector setting the target row sums of the matrix.
#' @param t_c A vector setting the target column sums of the matrix.
#' @param relative_diff A numeric value setting the relative difference of the two numerical vectors.
#' @param absolute_diff A numeric value setting the mean absolute difference of the two numerical vectors.
#' @return A numeric value of relative difference of t_r and t_c.
setToleranceforRAS <- function(t_r, t_c, relative_diff = NULL, absolute_diff = NULL) {
  if (!is.null(relative_diff)) {
    t <- relative_diff
  } else if (!is.null(absolute_diff)) {
    t <- absolute_diff/max(abs(t_c), abs(t_r))
  } else {
    stop("Set relative_diff or absolute_diff first.")
  }
  return(t)
}

#' Generalized RAS procedure. Takes an initial matrix, a target row sum vector
#' and target colsum vector. Iterates until all row sums of matrix equal to target row sum vector
#' and colsums of matrix equal target col sum vector, within a tolerance.
#' @param m0 A matrix object.
#' @param t_r A vector setting the target row sums of the matrix.
#' @param t_c A vector setting the target column sums of the matrix.
#' @param t A numeric value setting the tolerance of RAS.
#' @param max_itr A numeric value setting the maximum number of iterations to try for convergence.
#' Defualt: 1000000.
#' @return A RAS balanced matrix.
RAS <- function(m0, t_r, t_c, t, max_itr = 1E6) {
  m <- m0
  c_r <- as.vector(rowSums(m0))
  c_c <- as.vector(colSums(m0))
  # Check row and column conditions
  row_condition <- all.equal(t_r, c_r, tolerance = t)
  col_condition <- all.equal(t_c, c_c, tolerance = t)
  i <- 0
  while(!isTRUE(row_condition) | !isTRUE(col_condition)) {
    if(i>max_itr){
      break
    }
    # Adjust rowwise
    c_r <- as.vector(rowSums(m))
    # Replace 0 with 1 in c_r
    c_r[c_r==0] <- 1
    r_ratio <- t_r/c_r
    m <- diag(r_ratio) %*% m
    # Adjust colwise
    c_c <- as.vector(colSums(m))
    # Replace 0 with 1 in c_c
    c_c[c_c==0] <- 1
    c_ratio <- t_c/c_c
    m <- m %*% diag(c_ratio)
    # Check row and column conditions
    row_condition <- all.equal(t_r, c_r, tolerance = t)
    col_condition <- all.equal(t_c, c_c, tolerance = t)
    i <- i + 1
    
    if(i %% (max_itr/1000)==0){
      # Print on the screen some message
      cat(paste0("iteration: ", i/max_itr*100, "%\n"))
    }
    if(i %% (max_itr/10) ==0)
    {
      readline(prompt="Press [enter] to continue")#pause
    }
  }
  dimnames(m) <- dimnames(m0)
  print(paste("RAS converged after", i, "iterations."))
  return(m)
}

#' Integrate pre-adjustment of t_r, t_c and t (tolerance level) with RAS function.
#' @param m0 A matrix object.
#' @param t_r A vector setting the target row sums of the matrix.
#' @param t_c A vector setting the target column sums of the matrix.
#' @param relative_diff A numeric value setting the relative difference of the two numerical vectors.
#' @param absolute_diff A numeric value setting the mean absolute difference of the two numerical vectors.
#' @param max_itr A numeric value setting the maximum number of iterations to try for convergence.
#' Defualt: 1000000.
#' @return A RAS balanced matrix.
applyRAS <- function(m0, t_r, t_c, relative_diff, absolute_diff, max_itr) {
  # Adjust t_c/t_r, make sum(t_c)==sum(t_r)
  if (sum(t_c) > sum(t_r)) {
    t_r <- (t_r/sum(t_r))*sum(t_c)
  } else {
    t_c <- (t_c/sum(t_c))*sum(t_r)
  }
  # Generate t for RAS
  t <- setToleranceforRAS(t_r, t_c, relative_diff, absolute_diff)
  # Apply RAS
  m <- RAS(m0, t_r, t_c, t, max_itr)
  return(m)
}

#' Remove spaces around strings, like "321A "
#' @param s, string
#' @return s, string with spaces removed
removeExtraSpaces <- function(s) {
  s <- gsub("\\s", "",s)
  return(s)
}


#' Remove numbers in slashes from a string, like /1/
#' @param s, string
#' @return s, string with numbers in slashes removed
removeNumberinSlashes <- function(s) {
  s <- gsub(" /.*", "",s)
  return(s)
}

#' Forces a string encoding to ASCII from Latin-1
#' @param s, string with Latin-1 encoding
#' @return s, string with ASCII encoding
convertStrEncodingLatintoASCII <- function(s) {
  s <- iconv(s, from = 'latin1', to = 'ASCII', sub='')
  return(s)
}

