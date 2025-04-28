# test_model_build.R

# devtools::load_all(".")
# setwd("tests")
library(useeior)
# library(unittest, quietly = TRUE)
if (!interactive()) options(warn=1, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

## USEEIOv2.0.1-411 Detail model with waste disaggregation
m <- "USEEIOv2.0.1-411"
model <- buildModel(m)
printValidationResults(model)
testCalculationFunctions(model)
testVisualizationFunctions(model)

## USEEIOv2.0.1-411 Detail model with waste disaggregation (Economic only)
m <- "USEEIOv2.0.1-411"
model <- buildIOModel(m)
printValidationResults(model)
writeModeltoXLSX(model, ".")

## USEEIOv2.0.1-i-411 Detail, industry model with waste disaggregation
model <- useeior:::initializeModel(m)
model$specs$Model <- "USEEIOv2.0.1-i-411"
model$specs$CommodityorIndustryType <- "Industry"
model <- useeior:::loadIOData(model)
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)

## USEEIOv2.2-GHG Detail, commodity model (2017 Schema)
m <- "USEEIOv2.2-GHG"
cfg <- paste0("modelspecs/", m, ".yml")
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)

## USEEIOv2.2-s-GHG Summary, commodity model (2017 Schema)
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.2-s-GHG"
model$specs$BaseIOLevel <- "Summary"
model$crosswalk <- useeior:::getModelCrosswalk(model) # reassign for summary model
model <- useeior:::loadIOData(model)
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)

## USEEIOv2 - integrated hybrid
m <- "USEEIOv2.0-GHG-NGCombustion"
cfg <- c(paste0("modelspecs/", m, ".yml"),
         "hybridizationspecs/NG_Combustion.yml",
         "hybridizationspecs/NGCombustion_env.csv",
         "hybridizationspecs/NGCombustion_tech.csv"
         )
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)

## USEEIOv2 - mixed unit hybrid model
m <- "USEEIOv2.0-GHG-NGMUIO"
cfg <- c(paste0("modelspecs/", m, ".yml"),
         "muiospecs/NGMUIO.yml",
         "muiospecs/NGMUIO_Sectors.csv"
)
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)

## USEEIOv2.0 Detail, commodity model
m <- "USEEIOv2.0-GHG"
cfg <- paste0("modelspecs/", m, ".yml")
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)
writeModeltoXLSX(model, ".")

## USEEIOv2.0 Detail, industry model
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-i-GHG"
model$specs$CommodityorIndustryType <- "Industry"
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)


## USEEIOv2.0 Summary, commodity model with GHGs
m <- "USEEIOv2.0-s-GHG-19"
cfg <- c(paste0("modelspecs/", m, ".yml"))
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)

## USEEIOv2.0 Summary, industry model
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-is-GHG-19"
model$specs$CommodityorIndustryType <- "Industry"
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)

## USEEIOv2.0 Summary model with waste disaggregation
cfg <- c(paste0("modelspecs/", m, ".yml"),
         "disaggspecs/WasteDisaggregationSummary.yml",
         "disaggspecs/WasteDisaggregationSummary_Make.csv",
         "disaggspecs/WasteDisaggregationSummary_Use.csv"
         )
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-79-GHG-19"
model$specs$DisaggregationSpecs <- "WasteDisaggregationSummary"
model$specs$IOYear <- 2013 # TODO some years generate error
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)

## USEEIOv2.3 Detail, commodity model with GHGs and Import Factors
m <- "USEEIOv2.3-GHG"
model <- buildModel(m)
printValidationResults(model)
writeModeltoXLSX(model, ".")

## USEEIOv2.3 Summary, commodity model with GHGs and Import Factors
m <- "USEEIOv2.3-s-GHG-19"
model <- buildModel(m)
printValidationResults(model)
testCalculationFunctions(model)
testVisualizationFunctions(model)

## StateEEIOv1.0 Two-region Summary model
m <- "GAEEIOv1.0-GHG-19"
model <- buildModel(m)
printValidationResults(model)
writeModeltoXLSX(model, ".")
testCalculationFunctions(model)
testVisualizationFunctions(model)

## StateEEIOv1.0 Two-region Summary model (Economic only)
model <- buildIOModel(m)
printValidationResults(model)
writeModeltoXLSX(model, ".")

## StateEEIOv1.1 Two-region Summary model with Import Factors
cfg <- c("US_summary_import_factors_exio_2019_12sch.csv")
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "GAEEIOv1.1-GHG-19-IF"
model$specs$ExternalImportFactors <- TRUE
model$specs$ImportFactors <- list()
model$specs$ImportFactors$StaticFile <- "useeior/US_summary_import_factors_exio_2019_12sch.csv"
model$specs$ImportFactors$FileLocation <- "DataCommons"
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model, file.path(cfg))
printValidationResults(model)
testCalculationFunctions(model)
testVisualizationFunctions(model)

# ## StateEEIOv1.2 Two-region Summary model with "standard" Utility disaggregation
# model <- useeior:::initializeModel(m)
# model$specs$Model <- "GAEEIOv1.2-milkbar-19"
# model$specs$IODataVersion <- "0.3.0" # required for disaggregation
# model$specs$Alias <- "milkbar"
# model$specs$DisaggregationSpecs <- "UtilityDisaggregation"
# model <- useeior:::loadIOData(model, file.path(cfg))
# model <- useeior:::loadandbuildSatelliteTables(model)
# model <- useeior:::loadandbuildIndicators(model)
# model <- useeior:::loadDemandVectors(model)
# model <- useeior:::constructEEIOMatrices(model)
# printValidationResults(model)

## StateEEIOv1.3 Two-region Summary model, 2017 schema
m <- "GAEEIOv1.3-pecan-22"
model <- buildModel(m)
printValidationResults(model)
