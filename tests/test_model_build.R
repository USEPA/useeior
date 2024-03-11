# test_model_build.R

# devtools::load_all(".")
library(useeior)
# library(unittest, quietly = TRUE)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

m <- "USEEIOv2.0.1-411"
model <- buildModel(m)
printValidationResults(model)


m <- "USEEIOv2.0-GHG-NGCombustion"
cfg <- c(paste0("tests/modelspecs/", m, ".yml"),
         "tests/hybridizationspecs/NG_Combustion.yml",
         "tests/hybridizationspecs/NGCombustion_env.csv",
         "tests/hybridizationspecs/NGCombustion_tech.csv"
         )
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)


m <- "USEEIOv2.0-GHG"
cfg <- c(paste0("tests/modelspecs/", m, ".yml"),
         "tests/disaggregationspecs/WasteDisaggregationSummary.yml",
         "tests/disaggregationspecs/WasteDisaggregationSummary_Make.csv",
         "tests/disaggregationspecs/WasteDisaggregationSummary_Use.csv"
         )
model <- initializeModel(m, configpaths = file.path(cfg))
model <- loadIOData(model, file.path(cfg))
model <- loadandbuildSatelliteTables(model)
model <- loadandbuildIndicators(model)
model <- loadDemandVectors(model)
model <- constructEEIOMatrices(model)
printValidationResults(model)


model <- initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-s-GHG"
model$specs$BaseIOLevel <- "Summary"
model <- loadIOData(model, file.path(cfg))
model <- loadandbuildSatelliteTables(model)
model <- loadandbuildIndicators(model)
model <- loadDemandVectors(model)
model <- constructEEIOMatrices(model)
printValidationResults(model)