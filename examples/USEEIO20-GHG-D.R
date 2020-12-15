# Example for running useeior, see https://github.com/USEPA/useeior for more information on the model
# Run ?functionname to see documentation and function input options for the functions in this example
# The output csv files are the inputs for useeiopy (https://github.com/USEPA/useeiopy)

# Start clean by removing useeior package and installing latest version
#remove.packages("useeior")#commenting this line for disaggregation development/QA
#devtools::install_github("USEPA/useeior")#commenting this line for disaggregation development/QA

devtools::load_all(".")#to avoid re-building each time
library(useeior)
library(validate)

# See the versioning scheme for explanation of model names (https://github.com/USEPA/USEEIO/blob/master/VersioningScheme.md)
useeior::seeAvailableModels()
model <- useeior::loadIOData('USEEIOv2.0-GHG-D')
model <- loadbuildSatelliteTables(model)
model <- loadandbuildIndicators(model)
model <- useeior::buildEEIOModel(model)
result <- useeior::calculateEEIOModel(model, perspective='DIRECT')

# Write model for API
#useeior::writeModelforAPI(model) #has follwing error:  Error in file.path(basedir, "build", "data") : argument "basedir" is missing, with no default 

# Write model matrices to csv for review
useeior::writeModelMatrices(model,getwd())
