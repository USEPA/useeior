
# creating crosswalk of changes in FIPS codes over the years
getFIPSCrosswalk <- function(){
  # Download the static 2018 FIPS table and read excel sheet
  FileName <- "inst/extdata/all-geocodes-v2018.xlsx"
  if(!file.exists(FileName)) {
    utils::download.file("https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx", FileName, mode = "wb")
  }
  df <- as.data.frame(readxl::read_excel(FileName, sheet = 1, col_names = TRUE, skip = 4))
  
  ## modify the 2018 fips table to only keep columns needed
  # create new column of 5 digit fips
  df$FIPS_2018 <- paste(df$"State Code (FIPS)", df$"County Code (FIPS)", sep="") 
  # rename column and drop all but 2 columns
  names(df)[names(df) == "Area Name (including legal/statistical area description)"] <- "FIPS_2018_Name"
  df2 <- subset(df, select = c("FIPS_2018"))
  # select distint rows
  df3 <- unique(df2)
  
  ## modify columns depicting how counties have changed over the years - starting 2010
  # 2019 one FIPS code deleted and split into two FIPS
  df_19 <- df3
  df_19$FIPS_2019 <- df_19$FIPS_2018
  df_19$FIPS_2019[df_19$FIPS_2019 == "02261"] <- "02063"
  df_19[nrow(df_19) + 1,] = c("02261","02066")
  
  # 2015 had two different/renamed fips
  df_15 <- df3
  df_15$FIPS_2015 <- df_15$FIPS_2018
  df_15$FIPS_2015[df_15$FIPS_2015 == "02158"] <- "02270"
  df_15$FIPS_2015[df_15$FIPS_2015 == "46102"] <- "46113"
  
  # 2013 had a fips code that was merged with an existing fips   
  df_13 <- subset(df_15, select = c("FIPS_2015"))
  df_13$FIPS_2013 <- df_13$FIPS_2015
  df_13[nrow(df_13) + 1,] = c("51019","51515")
  
  # merge 2013 with 2015 dataframe
  df4 <- merge(df_13, df_15, by = "FIPS_2015")
  # merge 2019 with 2018
  df5 <- merge(df_19, df4, by = "FIPS_2018")
  
  # create columns for remaining years and rearrange
  df6 <- df5
  df6$FIPS_2010 <- df5$FIPS_2013
  df6$FIPS_2011 <- df5$FIPS_2013
  df6$FIPS_2012 <- df5$FIPS_2013
  df6$FIPS_2014 <- df5$FIPS_2013
  df6$FIPS_2016 <- df5$FIPS_2015
  df6$FIPS_2017 <- df5$FIPS_2015
  FIPSCrosswalk <- df6[,c('FIPS_2010', 'FIPS_2011', 'FIPS_2012', 'FIPS_2013', 'FIPS_2014', 
                          'FIPS_2015', 'FIPS_2016', 'FIPS_2017', 'FIPS_2018', 'FIPS_2019'
                          )]
  
  return(FIPSCrosswalk)
  
}
FIPSCrosswalk <- getFIPSCrosswalk()
usethis::use_data(FIPSCrosswalk, overwrite = T)

