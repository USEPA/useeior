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
Detail_Make_2007_BeforeRedef <- getBEADetailMakeBeforeRedef2012Schema()[["2007"]]
usethis::use_data(Detail_Make_2007_BeforeRedef, overwrite = TRUE)
Detail_Make_2012_BeforeRedef <- getBEADetailMakeBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Make_2012_BeforeRedef, overwrite = TRUE)

# Get BEA Detail Use (PRO, Before Redef, 2012 schema) 2007 and 2012 tables from static Excel
getBEADetailUsePROBeforeRedef2012Schema <- function () {
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
Detail_Use_2007_PRO_BeforeRedef <- getBEADetailUsePROBeforeRedef2012Schema()[["2007"]]
usethis::use_data(Detail_Use_2007_PRO_BeforeRedef, overwrite = TRUE)
Detail_Use_2012_PRO_BeforeRedef <- getBEADetailUsePROBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Use_2012_PRO_BeforeRedef, overwrite = TRUE)

# Get BEA Detail Make (After Redef, 2012 schema) 2007 and 2012 tables from static Excel
getBEADetailMakeAfterRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  DetailMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_After_Redefinitions_2007_2012_Detail.xlsx"
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
Detail_Make_2007_AfterRedef <- getBEADetailMakeAfterRedef2012Schema()[["2007"]]
usethis::use_data(Detail_Make_2007_AfterRedef, overwrite = TRUE)
Detail_Make_2012_AfterRedef <- getBEADetailMakeAfterRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Make_2012_AfterRedef, overwrite = TRUE)

# Get BEA Detail Use (PRO, After Redef, 2012 schema) 2007 and 2012 tables from static Excel
getBEADetailUsePROAfterRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  DetailUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_After_Redefinitions_PRO_2007_2012_Detail.xlsx"
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
Detail_Use_2007_PRO_AfterRedef <- getBEADetailUsePROAfterRedef2012Schema()[["2007"]]
usethis::use_data(Detail_Use_2007_PRO_AfterRedef, overwrite = TRUE)
Detail_Use_2012_PRO_AfterRedef <- getBEADetailUsePROAfterRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Use_2012_PRO_AfterRedef, overwrite = TRUE)

# Get BEA Summary Make (Before Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASummaryMakeBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SummaryMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_1997-2018_Summary.xlsx"
  for (i in 2010:2018) {
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
Summary_Make_2010_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2010"]]
usethis::use_data(Summary_Make_2010_BeforeRedef, overwrite = TRUE)
Summary_Make_2011_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2011"]]
usethis::use_data(Summary_Make_2011_BeforeRedef, overwrite = TRUE)
Summary_Make_2012_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Make_2012_BeforeRedef, overwrite = TRUE)
Summary_Make_2013_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2013"]]
usethis::use_data(Summary_Make_2013_BeforeRedef, overwrite = TRUE)
Summary_Make_2014_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2014"]]
usethis::use_data(Summary_Make_2014_BeforeRedef, overwrite = TRUE)
Summary_Make_2015_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2015"]]
usethis::use_data(Summary_Make_2015_BeforeRedef, overwrite = TRUE)
Summary_Make_2016_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2016"]]
usethis::use_data(Summary_Make_2016_BeforeRedef, overwrite = TRUE)
Summary_Make_2017_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2017"]]
usethis::use_data(Summary_Make_2017_BeforeRedef, overwrite = TRUE)
Summary_Make_2018_BeforeRedef <- getBEASummaryMakeBeforeRedef2012Schema()[["2018"]]
usethis::use_data(Summary_Make_2018_BeforeRedef, overwrite = TRUE)

# Get BEA Summary Use (PRO, Before Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASummaryUsePROBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SummaryUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_1997-2018_Summary.xlsx"
  for (i in 2010:2018) {
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
Summary_Use_2010_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2010"]]
usethis::use_data(Summary_Use_2010_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2011_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2011"]]
usethis::use_data(Summary_Use_2011_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2012_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Use_2012_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2013_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2013"]]
usethis::use_data(Summary_Use_2013_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2014_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2014"]]
usethis::use_data(Summary_Use_2014_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2015_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2015"]]
usethis::use_data(Summary_Use_2015_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2016_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2016"]]
usethis::use_data(Summary_Use_2016_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2017_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2017"]]
usethis::use_data(Summary_Use_2017_PRO_BeforeRedef, overwrite = TRUE)
Summary_Use_2018_PRO_BeforeRedef <- getBEASummaryUsePROBeforeRedef2012Schema()[["2018"]]
usethis::use_data(Summary_Use_2018_PRO_BeforeRedef, overwrite = TRUE)

# Get BEA Summary Make (After Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASummaryMakeAfterRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SummaryMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_After_Redefinitions_1997-2018_Summary.xlsx"
  for (i in 2010:2018) {
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
Summary_Make_2010_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2010"]]
usethis::use_data(Summary_Make_2010_AfterRedef, overwrite = TRUE)
Summary_Make_2011_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2011"]]
usethis::use_data(Summary_Make_2011_AfterRedef, overwrite = TRUE)
Summary_Make_2012_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Make_2012_AfterRedef, overwrite = TRUE)
Summary_Make_2013_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2013"]]
usethis::use_data(Summary_Make_2013_AfterRedef, overwrite = TRUE)
Summary_Make_2014_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2014"]]
usethis::use_data(Summary_Make_2014_AfterRedef, overwrite = TRUE)
Summary_Make_2015_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2015"]]
usethis::use_data(Summary_Make_2015_AfterRedef, overwrite = TRUE)
Summary_Make_2016_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2016"]]
usethis::use_data(Summary_Make_2016_AfterRedef, overwrite = TRUE)
Summary_Make_2017_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2017"]]
usethis::use_data(Summary_Make_2017_AfterRedef, overwrite = TRUE)
Summary_Make_2018_AfterRedef <- getBEASummaryMakeAfterRedef2012Schema()[["2018"]]
usethis::use_data(Summary_Make_2018_AfterRedef, overwrite = TRUE)

# Get BEA Summary Use (PRO, After Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASummaryUsePROAfterRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SummaryUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_After_Redefinitions_PRO_1997-2018_Summary.xlsx"
  for (i in 2010:2018) {
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
Summary_Use_2010_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2010"]]
usethis::use_data(Summary_Use_2010_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2011_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2011"]]
usethis::use_data(Summary_Use_2011_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2012_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Use_2012_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2013_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2013"]]
usethis::use_data(Summary_Use_2013_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2014_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2014"]]
usethis::use_data(Summary_Use_2014_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2015_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2015"]]
usethis::use_data(Summary_Use_2015_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2016_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2016"]]
usethis::use_data(Summary_Use_2016_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2017_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2017"]]
usethis::use_data(Summary_Use_2017_PRO_AfterRedef, overwrite = TRUE)
Summary_Use_2018_PRO_AfterRedef <- getBEASummaryUsePROAfterRedef2012Schema()[["2018"]]
usethis::use_data(Summary_Use_2018_PRO_AfterRedef, overwrite = TRUE)

# Get BEA Sector Make (Before Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASectorMakeBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SectorMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_1997-2018_Sector.xlsx"
  for (i in 2010:2018) {
    SectorMake <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:22, 3:20]
    SectorMake <- as.data.frame(apply(SectorMake, 2, as.numeric))
    rownames(SectorMake) <- c(substring(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:21, 1], 2),
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[22, 2])
    colnames(SectorMake) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:19],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 20])
    SectorMake[is.na(SectorMake)] <- 0
    SectorMakeList[[as.character(i)]] <- SectorMake
  }
  return(SectorMakeList)
}
Sector_Make_2010_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2010"]]
usethis::use_data(Sector_Make_2010_BeforeRedef, overwrite = TRUE)
Sector_Make_2011_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2011"]]
usethis::use_data(Sector_Make_2011_BeforeRedef, overwrite = TRUE)
Sector_Make_2012_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Sector_Make_2012_BeforeRedef, overwrite = TRUE)
Sector_Make_2013_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2013"]]
usethis::use_data(Sector_Make_2013_BeforeRedef, overwrite = TRUE)
Sector_Make_2014_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2014"]]
usethis::use_data(Sector_Make_2014_BeforeRedef, overwrite = TRUE)
Sector_Make_2015_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2015"]]
usethis::use_data(Sector_Make_2015_BeforeRedef, overwrite = TRUE)
Sector_Make_2016_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2016"]]
usethis::use_data(Sector_Make_2016_BeforeRedef, overwrite = TRUE)
Sector_Make_2017_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2017"]]
usethis::use_data(Sector_Make_2017_BeforeRedef, overwrite = TRUE)
Sector_Make_2018_BeforeRedef <- getBEASectorMakeBeforeRedef2012Schema()[["2018"]]
usethis::use_data(Sector_Make_2018_BeforeRedef, overwrite = TRUE)

# Get BEA Sector Use (PRO, Before Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASectorUsePROBeforeRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SectorUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_1997-2018_Sector.xlsx"
  for (i in 2010:2018) {
    SectorUse <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:33, 3:30]
    SectorUse <- as.data.frame(apply(SectorUse, 2, as.numeric))
    rownames(SectorUse) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:23, 1],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[24:26, 2],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[27:29, 1],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[30:33, 2])
    colnames(SectorUse) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:17],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 18:20],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 21:26],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 27:30])
    SectorUse[is.na(SectorUse)] <- 0
    SectorUseList[[as.character(i)]] <- SectorUse
  }
  return(SectorUseList)
}
Sector_Use_2010_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2010"]]
usethis::use_data(Sector_Use_2010_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2011_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2011"]]
usethis::use_data(Sector_Use_2011_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2012_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Sector_Use_2012_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2013_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2013"]]
usethis::use_data(Sector_Use_2013_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2014_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2014"]]
usethis::use_data(Sector_Use_2014_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2015_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2015"]]
usethis::use_data(Sector_Use_2015_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2016_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2016"]]
usethis::use_data(Sector_Use_2016_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2017_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2017"]]
usethis::use_data(Sector_Use_2017_PRO_BeforeRedef, overwrite = TRUE)
Sector_Use_2018_PRO_BeforeRedef <- getBEASectorUsePROBeforeRedef2012Schema()[["2018"]]
usethis::use_data(Sector_Use_2018_PRO_BeforeRedef, overwrite = TRUE)

# Get BEA Sector Make (After Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASectorMakeAfterRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SectorMakeList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOMake_After_Redefinitions_1997-2018_Sector.xlsx"
  for (i in 2010:2018) {
    SectorMake <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:22, 3:20]
    SectorMake <- as.data.frame(apply(SectorMake, 2, as.numeric))
    rownames(SectorMake) <- c(substring(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:21, 1], 2),
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[22, 2])
    colnames(SectorMake) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:19],
                              as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 20])
    SectorMake[is.na(SectorMake)] <- 0
    SectorMakeList[[as.character(i)]] <- SectorMake
  }
  return(SectorMakeList)
}
Sector_Make_2012_AfterRedef <- getBEASectorMakeAfterRedef2012Schema()[["2012"]]
usethis::use_data(Sector_Make_2012_AfterRedef, overwrite = TRUE)

# Get BEA Sector Use (PRO, After Redef, 2012 schema) 2010:2018 tables from static Excel
getBEASectorUsePROAfterRedef2012Schema <- function () {
  # Download all IO tables from BEA iTable
  getBEAIOTables()
  # Load desired excel file
  SectorUseList <- list()
  FileName <- "inst/extdata/AllTablesIO/IOUse_After_Redefinitions_PRO_1997-2018_Sector.xlsx"
  for (i in 2010:2018) {
    SectorUse <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:33, 3:30]
    SectorUse <- as.data.frame(apply(SectorUse, 2, as.numeric))
    rownames(SectorUse) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[7:23, 1],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[24:26, 2],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[27:29, 1],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[30:33, 2])
    colnames(SectorUse) <- c(as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 3:17],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 18:20],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[5, 21:26],
                             as.data.frame(readxl::read_excel(FileName, sheet = as.character(i)))[6, 27:30])
    SectorUse[is.na(SectorUse)] <- 0
    SectorUseList[[as.character(i)]] <- SectorUse
  }
  return(SectorUseList)
}
Sector_Use_2010_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2010"]]
usethis::use_data(Sector_Use_2010_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2011_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2011"]]
usethis::use_data(Sector_Use_2011_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2012_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2012"]]
usethis::use_data(Sector_Use_2012_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2013_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2013"]]
usethis::use_data(Sector_Use_2013_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2014_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2014"]]
usethis::use_data(Sector_Use_2014_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2015_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2015"]]
usethis::use_data(Sector_Use_2015_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2016_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2016"]]
usethis::use_data(Sector_Use_2016_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2017_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2017"]]
usethis::use_data(Sector_Use_2017_PRO_AfterRedef, overwrite = TRUE)
Sector_Use_2018_PRO_AfterRedef <- getBEASectorUsePROAfterRedef2012Schema()[["2018"]]
usethis::use_data(Sector_Use_2018_PRO_AfterRedef, overwrite = TRUE)

# Get BEA Detail Import (Before Redef, 2012 schema) 2007 and 2012 from static Excel
getBEADetailImportBeforeRedef2012Schema <- function () {
  DetailImportList <- list()
  # read excel sheet
  FileName <- "inst/extdata/ImportMatrices_Before_Redefinitions_DET_2007_2012.xlsx"
  if(!file.exists(FileName)) {
    download.file(paste("https://apps.bea.gov/industry/xls/io-annual", FileName, sep = "/"), FileName, mode = "wb")
  }
  for (i in c(2007, 2012)) {
    DetailImport <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i), col_names = FALSE))[7:411, 3:429]
    DetailImport <- as.data.frame(apply(DetailImport, 2, as.numeric))
    rownames(DetailImport) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i), col_names = FALSE))[7:411, 1]
    colnames(DetailImport) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i), col_names = FALSE))[6, 3:429]
    DetailImport[is.na(DetailImport)] <- 0
    DetailImportList[[as.character(i)]] <- DetailImport
  }
  return(DetailImportList)
}
Detail_Import_2012_BeforeRedef <- getBEADetailImportBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Import_2012_BeforeRedef, overwrite = TRUE)

# Get BEA Summary Import (Before Redef, 2012 schema) 2010:2017 from static Excel
getBEASummaryImportBeforeRedef2012Schema <- function () {
  SummaryImportList <- list()
  # read excel sheet
  FileName <- "inst/extdata/ImportMatrices_Before_Redefinitions_SUM_1997-2017.xlsx"
  if(!file.exists(FileName)) {
    download.file(paste("https://apps.bea.gov/industry/xls/io-annual", FileName, sep = "/"), FileName, mode="wb")
  }
  for (i in 2010:2017) {
    SummaryImport <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i), col_names = FALSE))[7:79, 3:95]
    SummaryImport <- as.data.frame(apply(SummaryImport, 2, as.numeric))
    rownames(SummaryImport) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i), col_names = FALSE))[7:79, 1]
    colnames(SummaryImport) <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(i), col_names = FALSE))[6, 3:95]
    SummaryImport[is.na(SummaryImport)] <- 0
    SummaryImportList[[as.character(i)]] <- SummaryImport
  }
  return(SummaryImportList)
}
Summary_Import_2010_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2010"]]
usethis::use_data(Summary_Import_2010_BeforeRedef, overwrite = TRUE)
Summary_Import_2011_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2011"]]
usethis::use_data(Summary_Import_2011_BeforeRedef, overwrite = TRUE)
Summary_Import_2012_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Summary_Import_2012_BeforeRedef, overwrite = TRUE)
Summary_Import_2013_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2013"]]
usethis::use_data(Summary_Import_2013_BeforeRedef, overwrite = TRUE)
Summary_Import_2014_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2014"]]
usethis::use_data(Summary_Import_2014_BeforeRedef, overwrite = TRUE)
Summary_Import_2015_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2015"]]
usethis::use_data(Summary_Import_2015_BeforeRedef, overwrite = TRUE)
Summary_Import_2016_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2016"]]
usethis::use_data(Summary_Import_2016_BeforeRedef, overwrite = TRUE)
Summary_Import_2017_BeforeRedef <- getBEASummaryImportBeforeRedef2012Schema()[["2017"]]
usethis::use_data(Summary_Import_2017_BeforeRedef, overwrite = TRUE)

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
usethis::use_data(Detail_GrossOutput_IO, overwrite = TRUE)

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
usethis::use_data(Summary_GrossOutput_IO, overwrite = TRUE)

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
usethis::use_data(Sector_GrossOutput_IO, overwrite = TRUE)

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
usethis::use_data(Detail_CPI_IO, overwrite = TRUE)

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
usethis::use_data(Summary_CPI_IO, overwrite = TRUE)

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
usethis::use_data(Sector_CPI_IO, overwrite = TRUE)

# Get BEA (Detail/Summary/Sector) Code and Name under 2012 schema
getBEACodeName2012Schema <- function () {
  ### Download all IO tables from BEA iTable
  getBEAIOTables()
  ### Load desired excel file
  ## Detail
  BEADetail <- as.data.frame(readxl::read_excel("inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_2007_2012_Detail.xlsx", sheet = "2012"))
  # Industry
  BEADetailIndustryCodeName <- BEADetail[6:410, 1:2]
  colnames(BEADetailIndustryCodeName) <- c("BEA_2012_Detail_Industry_Code", "BEA_2012_Detail_Industry_Name")
  rownames(BEADetailIndustryCodeName) <- NULL
  # Commodity
  BEADetailCommodityCodeName <- as.data.frame(t(BEADetail[5:4, 3:407]))
  colnames(BEADetailCommodityCodeName) <- c("BEA_2012_Detail_Commodity_Code", "BEA_2012_Detail_Commodity_Name")
  rownames(BEADetailCommodityCodeName) <- NULL
  ## Summary
  BEASummary <- as.data.frame(readxl::read_excel("inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_1997-2018_Summary.xlsx", sheet = "2012"))
  # Industry
  BEASummaryIndustryCodeName <- BEASummary[7:77, 1:2]
  colnames(BEASummaryIndustryCodeName) <- c("BEA_2012_Summary_Industry_Code", "BEA_2012_Summary_Industry_Name")
  rownames(BEASummaryIndustryCodeName) <- NULL
  # Commodity
  BEASummaryCommodityCodeName <- as.data.frame(t(BEASummary[5:6, 3:75]))
  colnames(BEASummaryCommodityCodeName) <- c("BEA_2012_Summary_Commodity_Code", "BEA_2012_Summary_Commodity_Name")
  rownames(BEASummaryCommodityCodeName) <- NULL
  ## Sector
  BEASector <- as.data.frame(readxl::read_excel("inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_1997-2018_Sector.xlsx", sheet = "2012"))
  # Industry
  BEASectorIndustryCodeName <- BEASector[7:21, 1:2]
  colnames(BEASectorIndustryCodeName) <- c("BEA_2012_Sector_Industry_Code", "BEA_2012_Sector_Industry_Name")
  rownames(BEASectorIndustryCodeName) <- NULL
  # Commodity
  BEASectorCommodityCodeName <- as.data.frame(t(BEASector[5:6, 3:19]))
  colnames(BEASectorCommodityCodeName) <- c("BEA_2012_Sector_Commodity_Code", "BEA_2012_Sector_Commodity_Name")
  rownames(BEASectorCommodityCodeName) <- NULL
  ### Put the data.frames in a list
  BEACodeNameList <- list(BEADetailIndustryCodeName, BEADetailCommodityCodeName,
                          BEASummaryIndustryCodeName, BEASummaryCommodityCodeName,
                          BEASectorIndustryCodeName, BEASectorCommodityCodeName)
  names(BEACodeNameList) <- c("DetailIndustry", "DetailCommodity", "SummaryIndustry", "SummaryCommodity", "SectorIndustry", "SectorCommodity")
  return(BEACodeNameList)
}

Detail_IndustryCodeName_2012 <- getBEACodeName2012Schema()[["DetailIndustry"]]
usethis::use_data(Detail_IndustryCodeName_2012, overwrite = TRUE)
Detail_CommodityCodeName_2012 <- getBEACodeName2012Schema()[["DetailCommodity"]]
usethis::use_data(Detail_CommodityCodeName_2012, overwrite = TRUE)
Summary_IndustryCodeName_2012 <- getBEACodeName2012Schema()[["SummaryIndustry"]]
usethis::use_data(Summary_IndustryCodeName_2012, overwrite = TRUE)
Summary_CommodityCodeName_2012 <- getBEACodeName2012Schema()[["SummaryCommodity"]]
usethis::use_data(Summary_CommodityCodeName_2012, overwrite = TRUE)
Sector_IndustryCodeName_2012 <- getBEACodeName2012Schema()[["SectorIndustry"]]
usethis::use_data(Sector_IndustryCodeName_2012, overwrite = TRUE)
Sector_CommodityCodeName_2012 <- getBEACodeName2012Schema()[["SectorCommodity"]]
usethis::use_data(Sector_CommodityCodeName_2012, overwrite = TRUE)

# Get PCE Bridge (2012 schema) 2007 and 2012 tables from BEA static URL
getBEADetailPCEBridge2012Schema <- function () {
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
Detail_PCE_2012 <- getBEADetailPCEBridge2012Schema()[["2012"]]
usethis::use_data(Detail_PCE_2012, overwrite = TRUE)

# Get DetailPEQ Bridge (2012 schema) 2007 and 2012 tables from BEA static URL
getBEADetailPEQBridge2012Schema <- function () {
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
Detail_PEQ_2012 <- getBEADetailPEQBridge2012Schema()[["2012"]]
usethis::use_data(Detail_PEQ_2012, overwrite = TRUE)

# Get Detail Margins (Before Redef, 2012 schema) 2007 and 2012 tables from BEA static URL
getBEADetailMarginsBeforeRedef2012Schema <- function () {
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
Detail_Margins_2012_BeforeRedef <- getBEADetailMarginsBeforeRedef2012Schema()[["2012"]]
usethis::use_data(Detail_Margins_2012_BeforeRedef, overwrite = TRUE)
