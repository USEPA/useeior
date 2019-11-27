#' Check if stored Model, Satllite, Demand, Indicators configurations are valid.
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

test_that("Stored satellite table configurations are valid", {
  data(SatelliteTableConfiguration)
  ref_config_params <- names(SatelliteTableConfiguration)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    for (sattable in config$SatelliteTable) {
      config_params <- names(sattable)
      show_failure(expect_equal(config_params, ref_config_params))
    }
  }
})

test_that("Stored demand configurations are valid", {
  data(DemandConfiguration)
  ref_config_params <- names(DemandConfiguration)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    config_params <- names(config$Demand)
    show_failure(expect_equal(config_params, ref_config_params))
  }
})

test_that("Stored indicator configurations are valid", {
  data(IndicatorConfiguration)
  ref_config_params <- names(IndicatorConfiguration)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    for (indicator in config$Indicators) {
      config_params <- names(indicator)
      show_failure(expect_equal(config_params, ref_config_params))
    }
  }
})

test_that("Stored data source configurations are valid", {
  data(DataSourceConfiguration)
  ref_config_params <- names(DataSourceConfiguration)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    # Satellite table data source
    for (sattable in config$SatelliteTable) {
      for (datasource in sattable$DataSources) {
        config_params <- names(datasource)
        show_failure(expect_equal(config_params, ref_config_params))
      }
    }
    # Indicator data source
    for (indicator in config$Indicators) {
      for (datasource in indicator$DataSources) {
        config_params <- names(datasource)
        show_failure(expect_equal(config_params, ref_config_params))
      }
    }
  }
})

#' Check if class of variables in Model, Satllite, Demand, Indicators configurations
#' is the same with that model configurations.
test_that("Class of stored variables in model configurations is valid", {
  data(ModelConfiguration)
  ref_params_class <- sapply(ModelConfiguration, class)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    params_class <- sapply(config, class)
    show_failure(expect_equal(params_class, ref_params_class))
  }
})

test_that("Class of stored variables in satellite table configurations is valid", {
  data(SatelliteTableConfiguration)
  ref_params_class <- sapply(SatelliteTableConfiguration, class)
  models <- seeAvailableModels()
  for (c in models) {
    config <- getModelConfiguration(c)
    for (sattable in config$SatelliteTable) {
      params_class <- sapply(sattable, class)
      show_failure(expect_equal(params_class, ref_params_class))
    }
  }
})
