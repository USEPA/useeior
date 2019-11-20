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

# Get BEA Detail Make (Before Redef, 2012 schema) 2007 and 2012 tables from static Excel
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

# Get BEA Detail Use (PRO, Before Redef, 2012 schema) 2007 and 2012 tables from static Excel
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

# Get BEA Summary Make (Before Redef, 2012 schema) 2007-2017 tables from static Excel
getBEASummaryMakeBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SummaryMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_1997-2018_Summary.xlsx"
  for (i in 2007:2017) {
    SummaryMake <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:78, 3:76]
    SummaryMake <- as.data.frame(apply(SummaryMake, 2, as.numeric))
    rownames(SummaryMake) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:77, 1],
                               as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[78, 2])
    colnames(SummaryMake) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:75],
                               as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 76])
    SummaryMake[is.na(SummaryMake)] <- 0
    SummaryMakeList[[as.character(i)]] <- SummaryMake
  }
  return(SummaryMakeList)
}
Summary_Make_2012_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Make_2012_BeforeRedef, overwrite = T)
Summary_Make_2007_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2007"]]
usethis::use_data(Summary_Make_2007_BeforeRedef, overwrite = T)

# Get BEA Summary Use (PRO, Before Redef, 2012 schema) 2007-2017 tables from static Excel
getBEASummaryUseProBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SummaryUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_1997-2018_Summary.xlsx"
  for (i in 2007:2017) {
    SummaryUse <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:89, 3:100]
    SummaryUse <- as.data.frame(apply(SummaryUse, 2, as.numeric))
    rownames(SummaryUse) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:79, 1],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[80:82, 2],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[83:85, 1],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[86:89, 2])
    colnames(SummaryUse) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:73],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 74:76],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 77:96],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 97:100])
    SummaryUse[is.na(SummaryUse)] <- 0
    SummaryUseList[[as.character(i)]] <- SummaryUse
  }
  return(SummaryUseList)
}
Summary_Use_2012_PRO_BeforeRedef <- getBEASummaryUseProBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Use_2012_PRO_BeforeRedef, overwrite = T)
Summary_Use_2007_PRO_BeforeRedef <- getBEASummaryUseProBeforeRedef2012Schema()[["2007"]]
usethis::use_data(Summary_Use_2007_PRO_BeforeRedef, overwrite = T)

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

# Get Detail BEA Gross Output (2012 schema) 2007-2017 tables from static Excel
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

# Get Summary BEA Gross Output (2012 schema) 2007-2017 tables from static Excel
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

# Get Sector BEA Gross Output (2012 schema) 2007-2017 tables from static Excel
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

# Get Detail BEA U.Chain-Type Price Indexes (CPI) (2012 schema) 2007-2017 tables from static Excel
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

# Get Summary BEA U.Chain-Type Price Indexes (CPI) (2012 schema) 2007-2017 tables from static Excel
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

# Get Sector BEA U.Chain-Type Price Indexes (CPI) (2012 schema) 2007-2017 tables from static Excel
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

# Get PCE Bridge (2012 schema) 2007 and 2012 tables from BEA static URL
getBEAPCEBridge2012Schema <- function () {
  # Download BEA PCE bridge table
  if(!file.exists("inst/extdata/PCEBridge_2007_2012_DET.xlsx")) {
    download.file("https://apps.bea.gov/industry/xls/underlying-estimates/PCEBridge_2007_2012_DET.xlsx",
                  "inst/extdata/PCEBridge_2007_2012_DET.xlsx", mode = "wb")
  }
  column_names <- c("NIPACode", "PCECategory", "CommodityCode", "CommodityDescription",
                    "ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
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

# Get PEQ Bridge (2012 schema) 2007 and 2012 tables from BEA static URL
getBEAPEQBridge2012Schema <- function () {
  # Download BEA PEQ bridge table
  if(!file.exists("inst/extdata/PEQBridge_2007_2012_DET.xlsx")) {
    download.file("https://apps.bea.gov/industry/xls/underlying-estimates/PEQBridge_2007_2012_DET.xlsx",
                  "inst/extdata/PEQBridge_2007_2012_DET.xlsx", mode = "wb")
  }
  column_names <- c("NIPACode", "PEQCategory", "CommodityCode", "CommodityDescription",
                    "ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
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

# Get Margins (Before Redef, 2012 schema) 2007 and 2012 tables from BEA static URL
getBEAMarginsBeforeRedef2012Schema <- function () {
  # Download BEA PCE bridge table
  if(!file.exists("inst/extdata/Margins_Before_Redefinitions_2007_2012_DET.xlsx")) {
    download.file("https://apps.bea.gov/industry/xls/underlying-estimates/Margins_Before_Redefinitions_2007_2012_DET.xlsx",
                  "inst/extdata/Margins_Before_Redefinitions_2007_2012_DET.xlsx", mode = "wb")
  }
  column_names <- c("NIPACode", "MarginsCategory", "CommodityCode", "CommodityDescription",
                    "ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
  # 2012 data
  Margins2012 <- as.data.frame(readxl::read_excel("inst/extdata/Margins_Before_Redefinitions_2007_2012_DET.xlsx", sheet = "2012"))[5:61848, ]
  colnames(Margins2012) <- column_names
  # Convert Margins values from character to numeric
  Margins2012[, column_names[5:9]] <- as.data.frame(apply(Margins2012[, column_names[5:9]], 2, as.numeric))
  # 2007 data
  Margins2007 <- as.data.frame(readxl::read_excel("inst/extdata/Margins_Before_Redefinitions_2007_2012_DET.xlsx", sheet = "2007"))[5:61844, ]
  colnames(Margins2007) <- column_names
  # Convert Margins values from character to numeric
  Margins2007[, column_names[5:9]] <- as.data.frame(apply(Margins2007[, column_names[5:9]], 2, as.numeric))
  
  # Put Margins2012 and Margins2007 in the Margins2012SchemaList
  Margins2012SchemaList <- list(Margins2007, Margins2012)
  names(Margins2012SchemaList) <- c("2007", "2012")
  
  return(Margins2012SchemaList)
}
MarginsBeforeRedef2012 <- getBEAMarginsBeforeRedef2012Schema()[["2012"]]
usethis::use_data(MarginsBeforeRedef2012, overwrite = T)
