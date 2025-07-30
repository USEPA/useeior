#' Functions to support data quality assessment
#' Draws from https://github.com/USEPA/ElectricityLCI/blob/master/electricitylci/dqi.py 

#' Get DQ fields in existing data frame
#' @param df A totals_by_sector data frame
#' @return A string vector with names of data quality fields
getDQfields <- function (df) {
  flow_data_quality_fields <- c("DataReliability",
                                "TemporalCorrelation",
                                "GeographicalCorrelation",
                                "TechnologicalCorrelation",
                                "DataCollection")  
  fields_in_df <- flow_data_quality_fields[flow_data_quality_fields %in% colnames(df)]
  return(fields_in_df)
}

#' Constants to determine which bound type to use for DQ indicator
#' and for each indicator, provide those bounds
#' @return A list with DQ bounds
setDQScoringBounds <- function() {
  bound_to_dqi <- list()
  bound_to_dqi[["upper"]] <- c("TemporalCorrelation")
  bound_to_dqi[["lower"]] <- c("DataCollection")
  temp_upper_bounds <- c(3, 6, 10, 15, NA) #years from target date
  dc_lower_bounds <- c(0.8, 0.6, 0.4, 0, NA) #proportion of total industry
  bound_to_dqi[["TemporalCorrelation"]] <- temp_upper_bounds
  bound_to_dqi[["DataCollection"]] <- dc_lower_bounds
  return(bound_to_dqi)
}

#' For the data quality scores based on ranges, this provides the appropriate
#' score in relation to either an upper or lower bound
#' passed based on the data quality indicator type.
#' @param raw_score numeric. Raw value used
#' @param dqi string. Name of the DQI category and a column name
#' @param scoring_bounds List. Constant returned by setDQScoringBounds()
#' @return integer, a data quality score
lookupDQBoundScore <- function(raw_score,dqi,scoring_bounds) {
  
  score <- NA
  if (is.na(raw_score)){
    return(score)  
  }
  
  if (dqi %in% scoring_bounds["lower"]) {
    for (i in 1:4) {
      if (raw_score >= scoring_bounds[[dqi]][i]) {
        score <- i
        break
      }  
    }
    if (is.na(score)) score<-5
  } else if (dqi %in% scoring_bounds["upper"]) {
    for (i in 1:4) {
      if (raw_score <= scoring_bounds[[dqi]][i]) {
        score <- i
        break
      } 
    }
    if (is.na(score)) score<-5
  } else {
    stop(paste("No bounds defined for", dqi))
  }
  return(score)
  
}

#' Adds contextual data quality scores to a data frame with Year present
#' @param df A data frame containing a 'Year' column representing data year
#' @return A data frame with contextual data quality scores added in columns
#' with names of the indicators. Only 'TemporalCorrelation' currently added.
scoreContextualDQ <- function(df)  {
  bounds <- setDQScoringBounds()
  for (year in unique(df$Year)) {
    df[df$Year==year, "TemporalCorrelation"] <- scoreTemporalDQ(year, target_year = NA, scoring_bounds = bounds)
  }
  return(df)
}

#' Scores temporal data quality using a lookup based on difference between data year and target year
#' @param data_year integer year of data
#' @param target_year integer year of data, defaults to current year
#' @param scoring_bounds global scoring bounds
#' @return An integer data quality score 1-5 or NA
scoreTemporalDQ <- function(data_year,target_year=NA, scoring_bounds) {
  if (is.na(data_year)) {
    return(NA)
  }
  if (is.na(target_year)) {
    target_year <- as.integer(format(Sys.Date(), "%Y"))
  }
  age <- target_year - data_year
  score <- lookupDQBoundScore(age, "TemporalCorrelation", scoring_bounds) 
  return(score)
}

#' Creates a 3d matrix of data quality scores (flow by sector)
#' @param model A complete EEIO model object with TbS
#' @return A 3d matrix, third dimension is the individual data quality indicator
createBdqi <- function(model) {
  df <- model$TbS
  df[, "Sector"] <- apply(df[, c("Sector", "Location")],
                          1, FUN = joinStringswithSlashes)

  dq_fields <- getDQfields(df)
  # Default score is 5
  df[dq_fields] <- lapply(df[dq_fields], function(x) ifelse(is.na(x), 5, x))
  # Get unique Flow and Sector values
  flows <- unique(df$Flow)
  sectors <- model$Industries$Code_Loc
  
  # Initialize 3D array
  dqi_3d <- array(5, dim = c(length(flows), length(sectors), length(dq_fields)),
                  dimnames = list(Flow = flows, Sector = sectors, Variable = dq_fields))
  
  # Fill the array
  for (i in seq_along(dq_fields)) {
    df_cast <- reshape2::dcast(df, Flow ~ Sector, value.var = dq_fields[i], fun.aggregate = sum, fill = 5)
    df_cast[, setdiff(model$Industries$Code_Loc, colnames(df_cast))] <- 5
    # Adjust column order to be the same with V_n rownames
    df_cast <- df_cast[, model$Industries$Code_Loc]
    dqi_3d[,,i] <- as.matrix(df_cast)
  }
  
  ## Transform into a flow x commodity matrix using market shares matrix for commodity models
  if(model$specs$CommodityorIndustryType == "Commodity") {
    transformed_3d <- array(5, dim = c(length(flows), length(model$Commodities$Code_Loc), length(dq_fields)),
                            dimnames = list(Flow = flows, Sector = model$Commodities$Code_Loc, Variable = dq_fields))
    for (i in seq_along(dq_fields)) {
      transformed_3d[,,i] <- dqi_3d[,,i] %*% model$V_n
    }
    dqi_3d <- transformed_3d
  }
  return(dqi_3d)
}

#' Helper function to create a 3d array for dqi matrices
#' @param dqi A complete EEIO model object with TbS
#' @param num_matrices, int of number of matrices (e.g. 5)
#' @param name_matrices, name of each matrix
#' @return A 3d matrix, of correct dimensions
initializeArray <- function(dqi, num_matrices, name_matrices) {
  dqi_3d <- array(5, dim = c(nrow(dqi), ncol(dqi), num_matrices),
                  dimnames = list(rows = rownames(dqi),
                                  columns = colnames(dqi),
                                  Variable = name_matrices))
  return(dqi_3d)
}

#' Creates D_dqi matrix from B_dqi
#' 
#' Using the formula: (C \%*\% (B * B_dqi)) / D
#' @param model A complete EEIO model object.
#' @return A 3d matrix of dqi scores for D
createDdqi <- function(model) {
  B_dqi <- model$B_dqi
  if(is.null(B_dqi) || (ncol(model$B) != ncol(B_dqi))) {
    logging::logwarn("Model DQI can not be calculated.")
    return(NULL)
  }
  D_dqi <- (model$C %*% (model$B * B_dqi[,,1])) / model$D # Temporary for initialization
  dqi_3d <- initializeArray(D_dqi, dim(B_dqi)[3], dimnames(B_dqi)[[3]])
  for (i in 1:dim(B_dqi)[3]) {
    dqi_3d[,,i] <- (model$C %*% (model$B * B_dqi[,,i])) / model$D
  }
  return(dqi_3d)
}

#' Creates M_dqi matrix from B_dqi
#' 
#' Using the formula: ((B * B_dqi) \%*\% L) / M
#' @param model A complete EEIO model object.
#' @return A 3d matrix of dqi scores for M
createMdqi <- function(model) {
  B_dqi <- model$B_dqi
  if(is.null(B_dqi) || (ncol(model$B) != ncol(B_dqi))) {
    logging::logwarn("Model DQI can not be calculated.")
    return(NULL)
  }
  M_dqi <- ((model$B * B_dqi[,,1]) %*% model$L) / model$M # Temporary for initialization
  dqi_3d <- initializeArray(M_dqi, dim(B_dqi)[3], dimnames(B_dqi)[[3]])
  for (i in 1:dim(B_dqi)[3]) {
    dqi_3d[,,i] <- ((model$B * B_dqi[,,i]) %*% model$L) / model$M
  }
  return(dqi_3d)
}

#' Creates N_dqi matrix from D_dqi
#' 
#' Using the formula: ((D * D_dqi) \%*\% L) / N
#' @param model A complete EEIO model object.
#' @return A 3d matrix of dqi scores for N
createNdqi <- function(model) {
  D_dqi <- model$D_dqi
  if(is.null(D_dqi) || (ncol(model$D) != ncol(D_dqi))) {
    return(NULL)
  }
  if(!is.null(model$specs$ExternalImportFactors) && model$specs$ExternalImportFactors) {
    logging::logwarn("Some model DQI can not be calculated for models with External Import Factors .")
    return(NULL)
  }
  N_dqi <- ((model$D * D_dqi[,,1]) %*% model$L) / model$N # Temporary for initialization
  dqi_3d <- initializeArray(N_dqi, dim(D_dqi)[3], dimnames(D_dqi)[[3]])
  for (i in 1:dim(D_dqi)[3]) {
    dqi_3d[,,i] <- ((model$D * D_dqi[,,i]) %*% model$L) / model$N
  }
  return(dqi_3d)
}

#' Collapses 3d DQI matrices into a single string rounded to a single decimal
#' @param dqi_3d A 3d matrix of dqi scores.
#' @return A single matrix of collapsed dqi scores as string
combineDQItoString <- function(dqi_3d) {
  # Get dimensions
  dims <- dim(dqi_3d)
  # Initialize result matrix
  string_matrix <- matrix("", nrow =  dims[1], ncol = dims[2],
                          dimnames = list(rownames(dqi_3d), colnames(dqi_3d)))
  
  # Loop through each cell and concatenate values from all slices
  for (i in 1: dims[1]) {
    for (j in 1:dims[2]) {
      values <- round(dqi_3d[i, j, ], 1)
      string_matrix[i, j] <- paste0("(", paste(values, collapse = "; "), ")")
    }
  }
  return(string_matrix)
}
