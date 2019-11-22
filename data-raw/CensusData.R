# Download Census 2012 Numerical List of Manufactured and Mineral Products
getCensusManufacturingMiningNAICSSectors <- function() {
  SectorList <- c(211, 212, 213, 311, 312, 313, 314, 315, 316, 321, 322, 323, 324, 325, 326, 327, 331, 332, 333, 334, 335, 336, 337, 339)
  CensusNAICSList <- list()
  td <- tempdir()
  tf <- tempfile(tmpdir = tempdir(), fileext = ".csv")
  for(sector in SectorList) {
    download.file(paste("https://www.census.gov/manufacturing/numerical_list/", sector, ".xls", sep = ""), tf, mode = "wb")
    CensusNAICSList[[sector]] <- as.data.frame(readxl::read_excel(tf, sheet = 1, col_names = TRUE, skip = 2))[, 1:2]
    colnames(CensusNAICSList[[sector]]) <- c("NAICS_Code", "NAICS_Name")
  }
  CensusNAICS <- do.call(rbind, CensusNAICSList)
  
  return(CensusNAICS)
}
Census_ManufacturingMiningSectors_NAICSCodeName <- getCensusManufacturingMiningNAICSSectors()
usethis::use_data(Census_ManufacturingMiningSectors_NAICSCodeName, overwrite = T)
