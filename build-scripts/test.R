
library(useeior)
modelname <- "USEEIOv2.0-GHG"
model1 <- buildEEIOmodel(modelname)
result <- calculate(model1, "DIRECT")
writeModelComponents(model1)
