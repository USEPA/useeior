library(useeior)
model <- useeior::loadIOData('USEEIOv2.0-GHG')
model <- useeior::buildEEIOModel(model)

writeModelforAPI(model)
writeSectorCrosswalkforAPI()

model <- useeior::loadIOData('USEEIOv2.0.16s-GHG')
model <- useeior::buildEEIOModel(model)

writeModelforAPI(model)
