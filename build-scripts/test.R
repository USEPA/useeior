
library(useeior)
modelname <- "USEEIOv2.0-GHG"
model1 <- buildEEIOmodel(modelname)
result <- calculate(model1, "DIRECT")
writeModelComponents(model1)
#Test to see if data stored in /data are accessible
PCEBridge <- useeior::PCEBridge2012
