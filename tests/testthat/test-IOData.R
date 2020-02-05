#' Check that IO industry output equals to GDP gross output
test_that("IO industry output equals to GDP gross output", {
  model <- buildEEIOModel("USEEIOv2.0.16-GHG")
  GrossOutput <- merge(model$BEA$MakeIndustryOutput, model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE], by = 0)
  show_failure(expect_equal(GrossOutput[, 2], GrossOutput[, as.character(model$specs$IOYear)]))
  
  model <- buildEEIOModel("USEEIOv2.0.16s-GHG")
  GrossOutput <- merge(model$BEA$MakeIndustryOutput, model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE], by = 0)
  show_failure(expect_equal(GrossOutput[, 2], GrossOutput[, as.character(model$specs$IOYear)]))
  
  model <- buildEEIOModel("USEEIOv2.0.16c-GHG")
  GrossOutput <- merge(model$BEA$MakeIndustryOutput, model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE], by = 0)
  show_failure(expect_equal(GrossOutput[, 2], GrossOutput[, as.character(model$specs$IOYear)]))
  
  model <- buildEEIOModel("USEEIOv2.0.12-GHG")
  GrossOutput <- merge(model$BEA$MakeIndustryOutput, model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE], by = 0)
  show_failure(expect_equal(GrossOutput[, 2], GrossOutput[, as.character(model$specs$IOYear)]))
})

#' Check that IO commodity output equals to GDP gross output
test_that("IO commodity output equals to GDP gross output", {
  model <- buildEEIOModel("USEEIOv2.0.16-GHG")
  CommodityOutput <- merge(model$BEA$UseCommodityOutput, model$CommodityOutput, by = 0)
  show_failure(expect_equal(CommodityOutput[, 2], CommodityOutput[, as.character(model$specs$IOYear)]))
  
  model <- buildEEIOModel("USEEIOv2.0.16s-GHG")
  CommodityOutput <- merge(model$BEA$UseCommodityOutput, model$CommodityOutput, by = 0)
  show_failure(expect_equal(CommodityOutput[, 2], CommodityOutput[, as.character(model$specs$IOYear)]))
  
  model <- buildEEIOModel("USEEIOv2.0.16c-GHG")
  CommodityOutput <- merge(model$BEA$UseCommodityOutput, model$CommodityOutput, by = 0)
  show_failure(expect_equal(CommodityOutput[, 2], CommodityOutput[, as.character(model$specs$IOYear)]))
  
  model <- buildEEIOModel("USEEIOv2.0.12-GHG")
  CommodityOutput <- merge(model$BEA$UseCommodityOutput, model$CommodityOutput, by = 0)
  show_failure(expect_equal(CommodityOutput[, 2], CommodityOutput[, as.character(model$specs$IOYear)]))
})
