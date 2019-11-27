# Define Model Configuration list object
ModelConfiguration <- list()
ModelConfiguration$Model <- ""
ModelConfiguration$BaseIOSchema <- 2012L
ModelConfiguration$BaseIOLevel <- "Detail"
ModelConfiguration$IOYear <- 2012L
ModelConfiguration$PrimaryRegionAcronym <- "US"
ModelConfiguration$ModelRegionAcronyms <- list()
ModelConfiguration$ModelType <- "US"
ModelConfiguration$ModelSource <- "BEA"
ModelConfiguration$BasePriceType <-  "PRO"
ModelConfiguration$BasewithRedefinitions <- FALSE
ModelConfiguration$ReferenceCurrencyYear <- 0L
ModelConfiguration$CommoditybyIndustryType <- "Commodity"
ModelConfiguration$ScrapIncluded <- TRUE
ModelConfiguration$SatelliteTable <- list()
ModelConfiguration$Demand <- list()
ModelConfiguration$Indicators <- list()

usethis::use_data(ModelConfiguration, overwrite = T)

#Write it to config file to inspect it
#! Not writing all fields correctly
#configr::write.config(ModelConfiguration,file.path = "inst/extdata/ModelConfig.yml",write.type = "yaml")

SatelliteTableConfiguration <- list()
SatelliteTableConfiguration$FullName <- ""
SatelliteTableConfiguration$Abbreviation <- ""
SatelliteTableConfiguration$StaticSource <- TRUE
SatelliteTableConfiguration$StaticFile <- ""
SatelliteTableConfiguration$DataYears <- list()
SatelliteTableConfiguration$Locations <- list()
SatelliteTableConfiguration$SectorListSource <- ""
SatelliteTableConfiguration$SectorListYear <- 2012L
SatelliteTableConfiguration$SectorListLevel <- "Detail"
SatelliteTableConfiguration$OriginalFlowSource <- ""
SatelliteTableConfiguration$DataSources <- list()

usethis::use_data(SatelliteTableConfiguration, overwrite = T)

DemandConfiguration <- list()
DemandConfiguration$DemandYear <- 2012
DemandConfiguration$DemandSubsystems <- list()

usethis::use_data(DemandConfiguration, overwrite = T)

IndicatorConfiguration <- list()
IndicatorConfiguration$FullName <- ""
IndicatorConfiguration$Abbreviation <- ""
IndicatorConfiguration$Category <- ""
IndicatorConfiguration$Unit <- ""
IndicatorConfiguration$StaticSource <- TRUE
IndicatorConfiguration$DataSources <- list()

usethis::use_data(IndicatorConfiguration, overwrite = T)

#Define a data source
DataSourceConfiguration <- list()
DataSourceConfiguration$Title <- ""
DataSourceConfiguration$Author <- ""
DataSourceConfiguration$DataYear <- ""
DataSourceConfiguration$URL <- ""
DataSourceConfiguration$Primary <- FALSE

usethis::use_data(DataSourceConfiguration, overwrite = T)
