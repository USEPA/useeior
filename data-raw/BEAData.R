# Download all IO tables from BEA iTable
getBEAIOTables <- function () {
  # Create the placeholder file
  AllTablesIO <- "inst/extdata/AllTablesIO.zip"
  # Download all BEA IO tables into the placeholder file
  if(!file.exists(AllTablesIO)) {
    download.file("https://apps.bea.gov//industry/iTables%20Static%20Files/AllTablesIO.zip", AllTablesIO, mode = "wb")
  }
  # Get the name of all files in the zip archive
  fname <- unzip(AllTablesIO, list = TRUE)[unzip(AllTablesIO, list = TRUE)$Length > 0, ]$Name
  # Unzip the file to the designated directory
  unzip(AllTablesIO, files = fname, exdir = "inst/extdata/AllTablesIO", overwrite = TRUE)
}

# Get BEA Detail Make (Before Redef) 2012 from static Excel
getBEADetailMakeBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  DetailMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_2007_2012_Detail.xlsx"
  for (i in c(2007, 2012)) {
    DetailMake <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6:411, 3:408]
    DetailMake <- as.data.frame(apply(DetailMake, 2, as.numeric))
    rownames(DetailMake) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6:411, 1]
    colnames(DetailMake) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:408]
    DetailMake[is.na(DetailMake)] <- 0
    DetailMakeList[[as.character(i)]] <- DetailMake
  }
  return(DetailMakeList)
}
Detail_Make_2012_BeforeRedef <- getBEADetailMakeBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Make_2012_BeforeRedef, overwrite = T)
Detail_Make_2007_BeforeRedef <- getBEADetailMakeBeforeRedef2012Schema()[["2007"]]
usethis::use_data(Detail_Make_2007_BeforeRedef, overwrite = T)

# Get BEA Detail Use (PRO, Before Redef) 2012 from static Excel
getBEADetailUseProBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  DetailUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_2007_2012_Detail.xlsx"
  for (i in c(2007, 2012)) {
    DetailUse <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6:416, 3:430]
    DetailUse <- as.data.frame(apply(DetailUse, 2, as.numeric))
    rownames(DetailUse) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6:416, 1]
    colnames(DetailUse) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:430]
    DetailUse[is.na(DetailUse)] <- 0
    DetailUseList[[as.character(i)]] <- DetailUse
  }
  return(DetailUseList)
}
Detail_Use_2012_PRO_BeforeRedef <- getBEADetailUseProBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Use_2012_PRO_BeforeRedef, overwrite = T)
Detail_Use_2007_PRO_BeforeRedef <- getBEADetailUseProBeforeRedef2012Schema()[["2007"]]
usethis::use_data(Detail_Use_2007_PRO_BeforeRedef, overwrite = T)

# Download all GDP tables from BEA iTable
getBEAUnderlyingTables <- function () {
  # Create the placeholder file
  AllTablesUnderlying <- "inst/extdata/AllTablesUnderlying.zip"
  # Download all BEA IO tables into the placeholder file
  if(!file.exists(AllTablesUnderlying)) {
    download.file("https://apps.bea.gov//industry/iTables%20Static%20Files/AllTablesUnderlying.zip", AllTablesUnderlying, mode = "wb")
  }
  # Get the name of all files in the zip archive
  fname <- unzip(AllTablesUnderlying, list = TRUE)[unzip(AllTablesUnderlying, list = TRUE)$Length > 0, ]$Name
  # Unzip the file to the designated directory
  unzip(AllTablesUnderlying, files = fname, exdir = "inst/extdata/AllTablesUnderlying", overwrite = TRUE)
}

# Get BEA Gross Output 2007-2017 from static Excel
# Detail
getBEADetailGrossOutput2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  getBEAUnderlyingTables()
  # Load desired excel file
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual_Detail.xls"
  DetailGrossOutput <- readxl::read_excel(FileName, sheet = "GO")[6:422, c(2, 13:23)]
  colnames(DetailGrossOutput) <- c("Gross_Output_Detail_Industry", as.data.frame(readxl::read_excel(FileName, sheet = "GO"))[5, 13:23])

  return(DetailGrossOutput)
}
Detail_GrossOutput_IO <- adjustBEAGrossOutouttoIOIndustry2012Schema()[["Detail"]]
usethis::use_data(Detail_GrossOutput_IO, overwrite = T)

# Summary
getBEASummaryGrossOutput2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  getBEAUnderlyingTables()
  # Load desired excel file
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SummaryGrossOutput <- readxl::read_excel(FileName, sheet = "GO")[6:197, c(2, 13:23)]
  colnames(SummaryGrossOutput) <- c("Gross_Output_Industry", as.data.frame(readxl::read_excel(FileName, sheet = "GO"))[5, 13:23])

  return(SummaryGrossOutput)
}
Summary_GrossOutput_IO <- adjustBEAGrossOutouttoIOIndustry2012Schema()[["Summary"]]
usethis::use_data(Summary_GrossOutput_IO, overwrite = T)

# Sector
getBEASectorGrossOutput2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  getBEAUnderlyingTables()
  # Load desired excel file
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SectorGrossOutput <- readxl::read_excel(FileName, sheet = "GO")[6:197, c(2, 13:23)]
  colnames(SectorGrossOutput) <- c("Gross_Output_Industry", as.data.frame(readxl::read_excel(FileName, sheet = "GO"))[5, 13:23])

  return(SectorGrossOutput)
}
Sector_GrossOutput_IO <- adjustBEAGrossOutouttoIOIndustry2012Schema()[["Sector"]]
usethis::use_data(Sector_GrossOutput_IO, overwrite = T)

# Get BEA U.Chain-Type Price Indexes (CPI) 2007-2017 from static Excel
# Detail
getBEADetailCPI2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  getBEAUnderlyingTables()
  # Load desired excel file
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual_Detail.xls"
  DetailCPI <- readxl::read_excel(FileName, sheet = "ChainPriceIndexes")[6:422, c(2, 13:23)]
  colnames(DetailCPI) <- c("Gross_Output_Detail_Industry", as.data.frame(readxl::read_excel(FileName, sheet = "ChainPriceIndexes"))[5, 13:23])

  return(DetailCPI)
}
Detail_CPI_IO <- adjustBEACPItoIOIndustry2012Schema()[["Detail"]]
usethis::use_data(Detail_CPI_IO, overwrite = T)

# Summary
getBEASummaryCPI2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  getBEAUnderlyingTables()
  # Load desired excel file
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SummaryCPI <- readxl::read_excel(FileName, sheet = "ChainPriceIndexes")[6:197, c(2, 13:23)]
  colnames(SummaryCPI) <- c("Gross_Output_Industry", as.data.frame(readxl::read_excel(FileName, sheet = "ChainPriceIndexes"))[5, 13:23])

  return(SummaryCPI)
}
Summary_CPI_IO <- adjustBEACPItoIOIndustry2012Schema()[["Summary"]]
usethis::use_data(Summary_CPI_IO, overwrite = T)

# Sector
getBEASectorCPI2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  getBEAUnderlyingTables()
  # Load desired excel file
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SectorCPI <- readxl::read_excel(FileName, sheet = "ChainPriceIndexes")[6:197, c(2, 13:23)]
  colnames(SectorCPI) <- c("Gross_Output_Industry", as.data.frame(readxl::read_excel(FileName, sheet = "ChainPriceIndexes"))[5, 13:23])

  return(SectorCPI)
}
Sector_CPI_IO <- adjustBEACPItoIOIndustry2012Schema()[["Sector"]]
usethis::use_data(Sector_CPI_IO, overwrite = T)

# Gets PCE Bridge data for 2012 from BEA static URL
getBEAPCEBridge2012Schema <- function () {
  # Download BEA PCE bridge table
  if(!file.exists("inst/extdata/PCEBridge_2007_2012_DET.xlsx")) {
    download.file("https://apps.bea.gov/industry/xls/underlying-estimates/PCEBridge_2007_2012_DET.xlsx", "inst/extdata/PCEBridge_2007_2012_DET.xlsx", mode = "wb")
  }
  column_names <- c("NIPACode", "PCECategory", "CommodityCode", "CommodityDescription", "ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
  # 2012 data
  PCEBridge2012 <- as.data.frame(readxl::read_excel("inst/extdata/PCEBridge_2007_2012_DET.xlsx", sheet = "2012"))[6:717, c(1:9)]
  colnames(PCEBridge2012) <- column_names
  # Convert PCE values from character to numeric
  PCEBridge2012[, column_names[5:9]] <- as.data.frame(apply(PCEBridge2012[, column_names[5:9]], 2, as.numeric))
  # 2007 data
  PCEBridge2007 <- as.data.frame(readxl::read_excel("inst/extdata/PCEBridge_2007_2012_DET.xlsx", sheet = "2007"))[6:717, c(1:9)]
  colnames(PCEBridge2007) <- column_names
  # Convert PCE values from character to numeric
  PCEBridge2007[, column_names[5:9]] <- as.data.frame(apply(PCEBridge2007[, column_names[5:9]], 2, as.numeric))

  # PutPCEBridge2012 and PCEBridge2007 in the PCEBridge2012SchemaList
  PCEBridge2012SchemaList <- list(PCEBridge2007, PCEBridge2012)
  names(PCEBridge2012SchemaList) <- c("2007", "2012")

  return(PCEBridge2012SchemaList)
}
PCEBridge2012 <- getBEAPCEBridge2012Schema()[["2012"]]
usethis::use_data(PCEBridge2012, overwrite = T)

# Get PEQ Bridge data for 2012 from BEA static URL
getBEAPEQBridge2012Schema <- function () {
  # Download BEA PEQ bridge table
  if(!file.exists("inst/extdata/PEQBridge_2007_2012_DET.xlsx")) {
    download.file("https://apps.bea.gov/industry/xls/underlying-estimates/PEQBridge_2007_2012_DET.xlsx", "inst/extdata/PEQBridge_2007_2012_DET.xlsx", mode = "wb")
  }
  column_names <- c("NIPACode", "PCECategory", "CommodityCode", "CommodityDescription", "ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
  # 2012 data
  PEQBridge2012 <- as.data.frame(readxl::read_excel("inst/extdata/PEQBridge_2007_2012_DET.xlsx", sheet = "2012"))[6:190, c(1:9)]
  colnames(PEQBridge2012) <-column_names
  # Convert PEQ values from character to numeric
  PEQBridge2012[, column_names[5:9]] <- as.data.frame(apply(PEQBridge2012[, column_names[5:9]], 2, as.numeric))
  # 2007 data
  PEQBridge2007 <- as.data.frame(readxl::read_excel("inst/extdata/PEQBridge_2007_2012_DET.xlsx", sheet = "2007"))[6:190, c(1:9)]
  colnames(PEQBridge2007) <- column_names
  # Convert PEQ values from character to numeric
  PEQBridge2007[, column_names[5:9]] <- as.data.frame(apply(PEQBridge2007[, column_names[5:9]], 2, as.numeric))

  # PutPEQBridge2012 and PEQBridge2007 in the PEQBridge2012SchemaList
  PEQBridge2012SchemaList <- list(PEQBridge2007, PEQBridge2012)
  names(PEQBridge2012SchemaList) <- c("2007", "2012")

  return(PEQBridge2012SchemaList)
}
PEQBridge2012 <- getBEAPEQBridge2012Schema()[["2012"]]
usethis::use_data(PEQBridge2012, overwrite = T)
