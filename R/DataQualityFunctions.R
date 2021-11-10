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
