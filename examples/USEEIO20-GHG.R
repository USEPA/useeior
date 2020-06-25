#Start with clean
remove.packages("useeior")
#install
devtools::install_github("USEPA/useeior")

library(useeior)
useeior::seeAvailableModels()
model <- useeior::buildEEIOModel('USEEIOv2.0-GHG')
result <- useeior::calculateEEIOModel(model, perspective='DIRECT')
useeior::writeModelComponents(model)
useeior::writeModelMatrices(model)
useeior::writeModelMatricesforAPI(model)
useeior::writeModelDemandstoJSON(model)
useeior::writeModelMetadata(model)
