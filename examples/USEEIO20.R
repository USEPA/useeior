# Example for running useeior, see https://github.com/USEPA/useeior for more information on the model
# Run ?functionname to see documentation and function input options for the functions in this example
# The output csv files are the inputs for useeiopy (https://github.com/USEPA/useeiopy)

# Start clean by removing useeior package and installing latest version
remove.packages("useeior")
devtools::install_github("USEPA/useeior")

library(useeior)

# See the versioning scheme for explanation of model names (https://github.com/USEPA/USEEIO/blob/master/VersioningScheme.md)
useeior::seeAvailableModels()
model <- useeior::loadIOData('USEEIOv2.0')
model <- useeior::loadandbuildSatelliteTables(model)
model <- useeior::loadandbuildIndicators(model)
model <- useeior::loadDemandVectors(model)
model <- useeior::buildEEIOModel(model)

result <- useeior::calculateEEIOModel(model, perspective='DIRECT',demand="Consumption")

# Write model for API
useeior::writeModelforAPI(model)

# Write model matrices to csv for review
useeior::writeModelMatrices(model,getwd())
