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
  str <- tolower(str)
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
  from_code <- paste("BEA", specs$BaseIOSchema, from_level, "Code", sep = "_")
  to_code   <- paste("BEA", specs$BaseIOSchema, to_level, "Code", sep = "_")
  # Aggregate by rows
  value_columns_1 <- colnames(matrix)
  df_fromlevel <- merge(matrix, unique(useeior::MasterCrosswalk2012[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
  df_fromlevel_agg <- stats::aggregate(df_fromlevel[, value_columns_1], by = list(df_fromlevel[, to_code]), sum)
  rownames(df_fromlevel_agg) <- df_fromlevel_agg[, 1]
  df_fromlevel_agg[, 1] <- NULL
  # aggregate by columns
  value_columns_2 <- rownames(df_fromlevel_agg)
  df_fromlevel_agg <- merge(t(df_fromlevel_agg), unique(useeior::MasterCrosswalk2012[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
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
    if (model$specs$CommoditybyIndustryType=="Industry") {
      Output <- generateCommodityOutputforYear(model$specs$PrimaryRegionAcronym, IsRoUS = FALSE, model)
    } else {
      Output <- model$CommodityOutput
    }
  } else {
    if (model$specs$CommoditybyIndustryType=="Commodity") {
      Output <- model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE]
    } else {
      Output <- model$IndustryOutput
    }
  }
  # Map CommodityOutput to more aggregated IO levels
  Crosswalk <- unique(useeior::MasterCrosswalk2012[, c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code", "BEA_2012_Detail_Code")])
  ratio_table <- merge(Crosswalk, Output, by.x = paste("BEA_2012", model$specs$BaseIOLevel, "Code", sep = "_"), by.y = 0)
  # Calculate output ratios
  for (iolevel in c("Summary", "Sector")) {
    # Generate flexible sector_code
    sector_code <- paste("BEA_2012", iolevel, "Code", sep = "_")
    # Sum Detail output to Summary/Sector
    output_sum <- stats::aggregate(ratio_table[, as.character(model$specs$IOYear)], by = list(ratio_table[, sector_code]), sum)
    colnames(output_sum) <- c(sector_code, paste0(iolevel, "Output"))
    ratio_table <- merge(ratio_table, output_sum, by = sector_code)
    # Calculate DetailSummaryRatio and DetailSectorRatio
    ratio_table[, paste0("to", iolevel, "Ratio")] <- ratio_table[, as.character(model$specs$IOYear)]/ratio_table[, paste0(iolevel, "Output")]
    # Generate SectorCode column
    ratio_table$SectorCode <- ratio_table[, paste("BEA_2012", model$specs$BaseIOLevel, "Code", sep = "_")]
  }
  # Keep ratio columns
  ratio_table <- unique(ratio_table[, c("SectorCode", "toSummaryRatio", "toSectorRatio")])
  if(model$specs$BaseIOLevel=="Sector") {
    ratio_table$toSectorRatio <- 1
    ratio_table <- unique(ratio_table[, c("SectorCode", "toSectorRatio")])
  }
  return(ratio_table)
}

#' Calculate tolerance for RAS. Takes a target row sum vector and target colsum vector.
#' Specify either relative difference or absolute difference .
ToleranceforRAS <- function(t_r, t_c, relative_diff = NULL, absolute_diff = NULL) {
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
  }
  dimnames(m) <- dimnames(m0)
  print(paste("RAS converged after", i, "iterations."))
  return(m)
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
