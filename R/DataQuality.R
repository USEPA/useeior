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
