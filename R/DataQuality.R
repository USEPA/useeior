#'Draws from https://github.com/USEPA/ElectricityLCI/blob/master/electricitylci/dqi.py 

#'Get DQ fields in existing data frame
#'@param national totals data frame 
#'@result string vector with names of data quality fields
getDQfields <- function (df) {
  flow_data_quality_fields <- c('ReliabilityScore',
                                'TemporalCorrelation',
                                'GeographicalCorrelation',
                                'TechnologicalCorrelation',
                                'DataCollection')  
  fields_in_df <- flow_data_quality_fields[flow_data_quality_fields %in% colnames(df)]
  return(fields_in_df)
}

#' Constants to determine which bound type to use for DQ indicator
#' and for each indicator, provide those bounds
#' @result a list with DQ bounds
setDQScoringBounds <- function() {
  bound_to_dqi <- list()
  bound_to_dqi[['upper']] <- c('TemporalCorrelation')
  bound_to_dqi[['lower']] <- c('DataCollection')
  temp_upper_bounds <- c(3,6,10,15,NA) #years from target date
  dc_lower_bounds <- c(0.8,0.6,0.4,0,NA) #proportion of total industry
  bound_to_dqi[['TemporalCorrelation']] <- temp_upper_bounds
  bound_to_dqi[['DataCollection']] <-  dc_lower_bounds
  return(bound_to_dqi)
}



#' Constants to determine which bound type to use for DQ indicator
lookupDQBoundScore <- function(raw_score,dqi,scoring_bounds) {
  
  score <- NA
  if (is.na(raw_score)){
    return(score)  
  }
  
  if (dqi %in% scoring_bounds['lower']) {
    for (i in 1:4) {
      if (raw_score >= scoring_bounds[[dqi]][i]) {
        score <- i
        break
      }  
    }
    if (is.na(score)) score<-5
  } else if (dqi %in% scoring_bounds['upper']) {
    for (i in 1:4) {
      if (raw_score <= scoring_bounds[[dqi]][i]) {
        score <- i
        break
      } 
    }
    if (is.na(score)) score<-5
  } else {
    logging::logerror(paste0("No bounds defined for ",dqi))
  }
  return(score)
  
}
  
