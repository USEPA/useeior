#' Check that satellite functions are working
test_that("Aggregation of satellite table is working", {
  model <- buildEEIOModel("USEEIOv2.0.16s-GHG")
  totals_by_sector <- utils::read.table(system.file("extdata", model$specs$SatelliteTable$GHG$StaticFile, package = "useeior"),
                                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
  totals_by_sector_agg <- aggregateSatelliteTable(totals_by_sector, model$specs$SatelliteTable$GHG$SectorListLevel, model$specs$BaseIOLevel, model)
  
  GHG <- "CO2"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "CH4"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "N2O"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  model <- buildEEIOModel("USEEIOv2.0.16c-GHG")
  totals_by_sector <- utils::read.table(system.file("extdata", model$specs$SatelliteTable$GHG$StaticFile, package = "useeior"),
                                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
  totals_by_sector_agg <- aggregateSatelliteTable(totals_by_sector, model$specs$SatelliteTable$GHG$SectorListLevel, model$specs$BaseIOLevel, model)
  
  GHG <- "CO2"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "CH4"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "N2O"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  model <- buildEEIOModel("USEEIOv2.0.12s-GHG")
  totals_by_sector <- utils::read.table(system.file("extdata", model$specs$SatelliteTable$GHG$StaticFile, package = "useeior"),
                                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
  totals_by_sector_agg <- aggregateSatelliteTable(totals_by_sector, model$specs$SatelliteTable$GHG$SectorListLevel, model$specs$BaseIOLevel, model)
  
  GHG <- "CO2"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "CH4"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "N2O"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  model <- buildEEIOModel("USEEIOv2.0.12is-GHG")
  totals_by_sector <- utils::read.table(system.file("extdata", model$specs$SatelliteTable$GHG$StaticFile, package = "useeior"),
                                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
  totals_by_sector_agg <- aggregateSatelliteTable(totals_by_sector, model$specs$SatelliteTable$GHG$SectorListLevel, model$specs$BaseIOLevel, model)
  
  GHG <- "CO2"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "CH4"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
  GHG <- "N2O"
  show_failure(expect_equal(sum(totals_by_sector[totals_by_sector$FlowName==GHG, "FlowAmount"]),
                            sum(totals_by_sector_agg[totals_by_sector_agg$FlowName==GHG, "FlowAmount"])))
  
})