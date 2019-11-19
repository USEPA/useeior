# Define Model Configuration list object
ModelConfiguration <- list()
ModelConfiguration$Model <- ""
ModelConfiguration$BaseIOSchema <- 2012
ModelConfiguration$BaseIOLevel <- "Detail"
ModelConfiguration$IOYear <- 2012
ModelConfiguration$PrimaryRegionAcronym <- "US"
ModelConfiguration$ModelRegionAcronyms <- list()
ModelConfiguration$ModelType <- "US"
ModelConfiguration$BasePriceType <-  "PRO"
ModelConfiguration$BasewithDefinitions <- FALSE
ModelConfiguration$ReferenceCurrencyYear <- 0
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
SatelliteTableConfiguration$DataSources <- list()
SatelliteTableConfiguration$Locations <- list()
SatelliteTableConfiguration$SectorListSource <- ""
SatelliteTableConfiguration$SectorListYear <- 2012
SatelliteTableConfiguration$SectorListLevel <- 'Detail'
SatelliteTableConfiguration$OriginalFlowSource <- ""

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
IndicatorConfiguration$DataSources <- list()

usethis::use_data(IndicatorConfiguration, overwrite = T)

#Define a data source
DataSource <- list()
DataSource$Title <- ""
DataSource$DataYear <- ""
DataSource$Author <- ""
DataSource$URL <- ""
DataSource$Primary <- FALSE

usethis::use_data(DataSource, overwrite = T)
