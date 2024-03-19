# test_model_build.R

# devtools::load_all(".")
# setwd("tests")
library(useeior)
# library(unittest, quietly = TRUE)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

## USEEIOv2.0.1-411 Detail model with waste disaggregation
m <- "USEEIOv2.0.1-411"
model <- buildModel(m)
printValidationResults(model)

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

## USEEIOv3.0-GHG Detail, commodity model (2017 Schema)
m <- "USEEIOv3.0-GHG"
cfg <- paste0("modelspecs/", m, ".yml")
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)

## USEEIOv3.0-s-GHG Summary, commodity model (2017 Schema)
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv3.0-s-GHG"
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

## USEEIOv2.0 Summary, commodity model
m <- "USEEIOv2.0-s-GHG"
cfg <- c(paste0("modelspecs/", m, ".yml"),
         "disaggspecs/WasteDisaggregationSummary.yml",
         "disaggspecs/WasteDisaggregationSummary_Make.csv",
         "disaggspecs/WasteDisaggregationSummary_Use.csv"
         )
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)

## USEEIOv2.0 Summary, industry model
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-is-GHG"
model$specs$CommodityorIndustryType <- "Industry"
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)

## USEEIOv2.0 Summary model with waste disaggregation
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-79-GHG"
model$specs$DisaggregationSpecs <- "WasteDisaggregationSummary"
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)

## StateEEIOv1.0 Two-region Summary model
m <- "GAEEIOv1.0-s-WAT-12"
cfg <- paste0("modelspecs/", m, ".yml")
model <- buildModel(m, configpaths = file.path(cfg))
useeior::print2RValidationResults(model)
