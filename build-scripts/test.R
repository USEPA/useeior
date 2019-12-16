
library(useeior)
modelname <- "USEEIOv2.0.10i-GHG"
model <- buildEEIOModel(modelname)
model <- deriveMarginSectorImpacts(model, margin_type = "intermediate")
result <- calculateEEIOModel(model, "DIRECT")
result_domestic <- calculateEEIOModel(model, "DIRECT", use_domestic = TRUE)
result_external_using_domestic <- calculateEEIOModel(model, "DIRECT", use_domestic = TRUE, 
                                                     for_imports_using_domestic=TRUE)
price_adjusted_result <- adjustMultiplierPrice("B", 2017, purchaser_price=TRUE, margin_type="final consumer", model)
writeModelComponents(model1)

# Normalize the M result
m_d_n <- useeior::normalizeResultMatrixByTotalImpacts(result$m_d)
