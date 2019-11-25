
library(useeior)
modelname <- "USEEIOv2.0-GHG"
model1 <- buildEEIOModel(modelname)
result <- calculateEEIOModel(model1, "DIRECT")
writeModelComponents(model1)
