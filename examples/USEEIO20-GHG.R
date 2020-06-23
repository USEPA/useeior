#Start with clean
remove.packages("useeior")
#install
devtools::install_github("USEPA/useeior")

library(useeior)
useeior::seeAvailableModels()
model <- useeior::prepareEEIOModel('USEEIOv2.0-GHG')
model <- useeior::buildEEIOModel(model)
result <- useeior::calculateEEIOModel(model, perspective='DIRECT')
useeior::writeModelComponents(model)
