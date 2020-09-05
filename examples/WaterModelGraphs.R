library(useeior)
matrix_list <- list()
modelspecs_list <- list()
modelnames <- c("USEEIOv2.0.10s-WAT", "USEEIOv2.0.15s-WAT") # add more model names here
for (modelname in modelnames) {
  model <- useeior::loadIOData(modelname)
  model <- loadbuildSatelliteTables(model)
  model <- loadandbuildIndicators(model)
  model <- useeior::buildEEIOModel(model)
  # Specify flows you want to plot
  flow <- c("water, fresh/resource/water/fresh water body/na/kg",
            "water, fresh/resource/water/subterranean/fresh water body/na/kg",
            "water, saline/resource/water/saline water body/na/kg",
            "water, saline/resource/water/subterranean/saline water body/na/kg",
            "water/resource/water/na/kg")
  matrix_list[[modelname]] <- model$M
  modelspecs_list[[modelname]] <- model$specs
}

p <- lineplotFlowCoefficients(flow, matrix_list, modelspecs_list) +
  labs(y = "") # Add proper y-axis title

plot(p)

# Total water (fresh and saline) by 2-digit NAICS
methodname <- "Water_national_2015_m1"
fresh_water <- c("resource/water", "resource/water/fresh water body", "resource/water/subterranean/fresh water body")
saline_water <- c("resource/water/saline water body", "resource/water/subterranean/saline water body")
group <- list("Fresh Water" = fresh_water, "Saline Water" = saline_water)

p <- barplotFlowbySector(methodname, NAICS_digit = 2, group)

plot(p)
