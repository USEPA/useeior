# test_model_build.R

# devtools::load_all(".")
library(useeior)
# library(unittest, quietly = TRUE)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

m <- "USEEIOv2.0.1-411"
model <- buildModel(m)
printValidationResults(model)


m <- "USEEIOv2.0-GHG-NGCombustion"
cfg <- c(paste0("modelspecs/", m, ".yml"),
         "hybridizationspecs/NG_Combustion.yml",
         "hybridizationspecs/NGCombustion_env.csv",
         "hybridizationspecs/NGCombustion_tech.csv"
         )
model <- buildModel(m, configpaths = file.path(cfg))
printValidationResults(model)


m <- "USEEIOv2.0-GHG"
cfg <- c(paste0("modelspecs/", m, ".yml"),
         "disaggregationspecs/WasteDisaggregationSummary.yml",
         "disaggregationspecs/WasteDisaggregationSummary_Make.csv",
         "disaggregationspecs/WasteDisaggregationSummary_Use.csv"
         )
model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)


model <- useeior:::initializeModel(m, configpaths = file.path(cfg))
model$specs$Model <- "USEEIOv2.0-s-GHG"
model$specs$BaseIOLevel <- "Summary"
model <- useeior:::loadIOData(model, file.path(cfg))
model <- useeior:::loadandbuildSatelliteTables(model)
model <- useeior:::loadandbuildIndicators(model)
model <- useeior:::loadDemandVectors(model)
model <- useeior:::constructEEIOMatrices(model)
printValidationResults(model)