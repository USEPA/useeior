
library(useeior)
modelname <- "USEEIOv2.0-GHG"
model1 <- buildEEIOModel(modelname)
result <- calculateEEIOModel(model1, "DIRECT")
result_domestic <- calculateEEIOModel(model1, "DIRECT", use_domestic = TRUE)
result_external_using_domestic <- calculateEEIOModel(model1, "DIRECT", use_domestic = TRUE, 
                                                     for_imports_using_domestic=TRUE)
writeModelComponents(model1)
