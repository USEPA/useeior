#Start with clean
remove.packages("useeior")
#install
devtools::install_github("USEPA/useeior")
# See https://github.com/USEPA/useeior for more information.

library(useeior)
useeior::seeAvailableModels()
model <- useeior::buildEEIOModel('USEEIOv2.0-GHG')
# Run ?calculateEEIOModel to see documentation for calculateEEIOModel()
result <- useeior::calculateEEIOModel(model, perspective='DIRECT')
useeior::writeModelComponents(model)
#useeior::writeModelMatrices(model)
useeior::writeModelMatricesforAPI(model)
useeior::writeModelDemandstoJSON(model)
useeior::writeModelMetadata(model)
