
test_that("Stored model configurations are valid", {
  data(ModelConfiguration)
  ref_config_params <- names(ModelConfiguration)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    config_params <- names(config)  
    show_failure(expect_equal(config_params, ref_config_params))
  }

})
