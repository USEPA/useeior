
library(useeior)
modelname <- "USEEIOv2.0-GHG"
model1 <- buildEEIOModel(modelname)
result <- calculateEEIOModel(model1, "DIRECT")
result_domestic <- calculateEEIOModel(model1, "DIRECT", use_domestic = TRUE)
result_external_using_domestic <- calculateEEIOModel(model1, "DIRECT", use_domestic = TRUE, 
                                                     for_imports_using_domestic=TRUE)
price_adjusted_result <- adjustMultiplierPrice(2017, purchaser_price=TRUE, margin_type="final consumer", model1)
writeModelComponents(model1)

# Normalize the M result
m_d_n <- useeior::normalizeResultMatrixByTotalImpacts(result$m_d)
