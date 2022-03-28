# Download all IO tables from BEA iTable
getBEAIOTables <- function() {
  # Create the placeholder file
  AllTablesIO <- "inst/extdata/AllTablesIO.zip"
  # Download all BEA IO tables into the placeholder file
  url <- "https://apps.bea.gov/industry/iTables%20Static%20Files/AllTablesIO.zip"
  if (!file.exists(AllTablesIO)) {
    utils::download.file(url, AllTablesIO, mode = "wb")
  }
  # Get the name of all files in the zip archive
  files <- unzip(AllTablesIO, list = TRUE)
  fname <- files[files$Length > 0, ]$Name
  if (all(fname == basename(fname))) {
    exdir <- "inst/extdata/AllTablesIO"
  } else {
    exdir <- "inst/extdata/"
  }
  # Unzip the file to the designated directory
  unzip(AllTablesIO, files = fname, exdir = exdir,
        overwrite = TRUE, setTimes = TRUE)
  # Create output
  ls <- list("url" = url,
             "date_accessed" = as.character(as.Date(file.mtime(AllTablesIO))),
             "files" = basename(fname))
  return(ls)
}


# Get BEA Detail Make (Before Redef, 2012 schema) table from static Excel
getBEADetailMakeBeforeRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOMake_Before_Redefinitions") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  DetailMake <- as.data.frame(readxl::read_excel(FileName,
                                                 sheet = as.character(year)))
  # Trim table, assign row and column names
  DetailMake <- DetailMake[!is.na(DetailMake[, 2]), ]
  colnames(DetailMake) <- DetailMake[1, ]
  DetailMake <- DetailMake[-1, ]
  rownames(DetailMake) <- DetailMake$Code
  DetailMake[, c("Code", "Industry Description")] <- NULL
  # Replace NA with zero
  DetailMake[is.na(DetailMake)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailMake,
                 data_name = paste0("Detail_Make_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Make_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Detail Make (Before Redef, 2012 schema)
getBEADetailMakeBeforeRedef2012Schema(2012)

# Get BEA Detail Use (PRO, Before Redef, 2012 schema) table from static Excel
getBEADetailUsePROBeforeRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  DetailUse <- as.data.frame(readxl::read_excel(FileName,
                                                sheet = as.character(year)))
  # Trim table, assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  DetailUse <- DetailUse[-1, ]
  rownames(DetailUse) <- DetailUse$Code
  DetailUse[, c("Code", "Commodity Description")] <- NULL
  # Replace NA with zero
  DetailUse[is.na(DetailUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailUse,
                 data_name = paste0("Detail_Use_", year, "_PRO_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Use_", year, "_PRO_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Detail Use (PRO, Before Redef, 2012 schema)
getBEADetailUsePROBeforeRedef2012Schema(2012)

# Get BEA Detail Use (PUR, Before Redef, 2012 schema) 2012 table from static Excel
getBEADetailUsePURBeforeRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PUR") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  DetailUse <- as.data.frame(readxl::read_excel(FileName,
                                                sheet = as.character(year)))
  # Trim table, assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  DetailUse <- DetailUse[-1, ]
  rownames(DetailUse) <- DetailUse$Code
  DetailUse[, c("Code", "Commodity Description")] <- NULL
  # Replace NA with zero
  DetailUse[is.na(DetailUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailUse,
                 data_name = paste0("Detail_Use_", year, "_PUR_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Use_", year, "_PUR_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Detail Use (PUR, Before Redef, 2012 schema)
getBEADetailUsePURBeforeRedef2012Schema(2012)

# Get BEA Detail Make (After Redef, 2012 schema) table from static Excel
getBEADetailMakeAfterRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOMake_After_Redefinitions") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  DetailMake <- as.data.frame(readxl::read_excel(FileName,
                                                 sheet = as.character(year)))
  # Trim table, assign row and column names
  DetailMake <- DetailMake[!is.na(DetailMake[, 2]), ]
  colnames(DetailMake) <- DetailMake[1, ]
  DetailMake <- DetailMake[-1, ]
  rownames(DetailMake) <- DetailMake$Code
  DetailMake[, c("Code", "Industry Description")] <- NULL
  # Replace NA with zero
  DetailMake[is.na(DetailMake)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailMake,
                 data_name = paste0("Detail_Make_", year, "_AfterRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Make_", year, "_AfterRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Detail Make (After Redef, 2012 schema)
getBEADetailMakeAfterRedef2012Schema(2012)

# Get BEA Detail Use (PRO, After Redef, 2012 schema) table from static Excel
getBEADetailUsePROAfterRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_After_Redefinitions_PRO") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  DetailUse <- as.data.frame(readxl::read_excel(FileName,
                                                sheet = as.character(year)))
  # Trim table, assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  DetailUse <- DetailUse[-1, ]
  rownames(DetailUse) <- DetailUse$Code
  DetailUse[, c("Code", "Commodity Description")] <- NULL
  # Replace NA with zero
  DetailUse[is.na(DetailUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailUse,
                 data_name = paste0("Detail_Use_", year, "_PRO_AfterRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Use_", year, "_PRO_AfterRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Detail Use (PRO, After Redef, 2012 schema)
getBEADetailUsePROAfterRedef2012Schema(2012)

# Get BEA Detail Use (PUR, After Redef, 2012 schema) table from static Excel
getBEADetailUsePURAfterRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_After_Redefinitions_PUR") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  DetailUse <- data.frame(readxl::read_excel(FileName,
                                             sheet = as.character(year)))
  # Trim table, assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  DetailUse <- DetailUse[-1, ]
  rownames(DetailUse) <- DetailUse$Code
  DetailUse[, c("Code", "Commodity Description")] <- NULL
  # Replace NA with zero
  DetailUse[is.na(DetailUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailUse,
                 data_name = paste0("Detail_Use_", year, "_PUR_AfterRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Use_", year, "_PUR_AfterRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Detail Use (PUR, After Redef, 2012 schema)
getBEADetailUsePURAfterRedef2012Schema(2012)

# Get BEA Summary Make (Before Redef, 2012 schema) table from static Excel
getBEASummaryMakeBeforeRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOMake_Before_Redefinitions") &
                  endsWith(files, "Summary.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SummaryMake <- data.frame(readxl::read_excel(FileName,
                                                 sheet = as.character(year)))
    # Trim table, assign column names
    SummaryMake <- SummaryMake[!is.na(SummaryMake[, 2]), ]
    colnames(SummaryMake) <- SummaryMake[1, ]
    colname_check <- is.na(colnames(SummaryMake))
    colnames(SummaryMake)[colname_check] <- SummaryMake[2, colname_check]
    # Fill NA in code column with corresponding name
    SummaryMake[is.na(SummaryMake[, 1]), 1] <- SummaryMake[is.na(SummaryMake[, 1]), 2]
    # Convert all values to numeric, assign row names
    SummaryMake <- as.data.frame(lapply(SummaryMake[-c(1:2), -c(1:2)], as.numeric),
                                 check.names = FALSE,
                                 row.names = SummaryMake[-c(1:2), 1])
    # Replace NA with zero
    SummaryMake[is.na(SummaryMake)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SummaryMake,
                   data_name = paste0("Summary_Make_", year, "_BeforeRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Summary_Make_", year, "_BeforeRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Summary Make (Before Redef, 2012 schema)
getBEASummaryMakeBeforeRedef2012Schema()

# Get BEA Summary Use (PRO, Before Redef, 2012 schema) table from static Excel
getBEASummaryUsePROBeforeRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                  endsWith(files, "Summary.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SummaryUse <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
    # Trim table, assign column names
    SummaryUse <- SummaryUse[!is.na(SummaryUse[, 2]), ]
    colnames(SummaryUse) <- SummaryUse[1, ]
    colname_check <- is.na(colnames(SummaryUse))
    colnames(SummaryUse)[colname_check] <- SummaryUse[2, colname_check]
    # Fill NA in code column with corresponding name
    SummaryUse[is.na(SummaryUse[, 1]), 1] <- SummaryUse[is.na(SummaryUse[, 1]), 2]
    # Convert all values to numeric, assign row names
    SummaryUse <- as.data.frame(lapply(SummaryUse[-c(1:2), -c(1:2)], as.numeric),
                                check.names = FALSE,
                                row.names = SummaryUse[-c(1:2), 1])
    # Replace NA with zero
    SummaryUse[is.na(SummaryUse)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SummaryUse,
                   data_name = paste0("Summary_Use_", year, "_PRO_BeforeRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Summary_Use_", year, "_PRO_BeforeRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Summary Use (PRO, Before Redef, 2012 schema)
getBEASummaryUsePROBeforeRedef2012Schema()

# Get BEA Summary Use (PUR, Before Redef, 2012 schema) table from static Excel
getBEASummaryUsePURBeforeRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOUse_Before_Redefinitions_PUR") &
                  endsWith(files, "Summary.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Load data
  SummaryUse <- as.data.frame(readxl::read_excel(FileName,
                                                 sheet = as.character(year)))
  # Trim table, assign column names
  SummaryUse <- SummaryUse[!is.na(SummaryUse[, 2]), ]
  colnames(SummaryUse) <- SummaryUse[1, ]
  colname_check <- is.na(colnames(SummaryUse))
  colnames(SummaryUse)[colname_check] <- SummaryUse[2, colname_check]
  # Fill NA in code column with corresponding name
  SummaryUse[is.na(SummaryUse[, 1]), 1] <- SummaryUse[is.na(SummaryUse[, 1]), 2]
  # Convert all values to numeric, assign row names
  SummaryUse <- as.data.frame(lapply(SummaryUse[-c(1:2), -c(1:2)], as.numeric),
                              check.names = FALSE,
                              row.names = SummaryUse[-c(1:2), 1])
  # Replace NA with zero
  SummaryUse[is.na(SummaryUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SummaryUse,
                 data_name = paste0("Summary_Use_", year, "_PUR_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Summary_Use_", year, "_PUR_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Summary Use (PUR, Before Redef, 2012 schema)
getBEASummaryUsePURBeforeRedef2012Schema(2012)

# Get BEA Summary Make (After Redef, 2012 schema) table from static Excel
getBEASummaryMakeAfterRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOMake_After_Redefinitions") &
                  endsWith(files, "Summary.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SummaryMake <- data.frame(readxl::read_excel(FileName,
                                                 sheet = as.character(year)))
    # Trim table, assign column names
    SummaryMake <- SummaryMake[!is.na(SummaryMake[, 2]), ]
    colnames(SummaryMake) <- SummaryMake[1, ]
    colname_check <- is.na(colnames(SummaryMake))
    colnames(SummaryMake)[colname_check] <- SummaryMake[2, colname_check]
    # Fill NA in code column with corresponding name
    SummaryMake[is.na(SummaryMake[, 1]), 1] <- SummaryMake[is.na(SummaryMake[, 1]), 2]
    # Convert all values to numeric, assign row names
    SummaryMake <- as.data.frame(lapply(SummaryMake[-c(1:2), -c(1:2)], as.numeric),
                                 check.names = FALSE,
                                 row.names = SummaryMake[-c(1:2), 1])
    # Replace NA with zero
    SummaryMake[is.na(SummaryMake)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SummaryMake,
                   data_name = paste0("Summary_Make_", year, "_AfterRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Summary_Make_", year, "_AfterRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Summary Make (After Redef, 2012 schema)
getBEASummaryMakeAfterRedef2012Schema()

# Get BEA Summary Use (PRO, After Redef, 2012 schema) table from static Excel
getBEASummaryUsePROAfterRedef2012Schema <- function () {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOUse_After_Redefinitions_PRO") &
                  endsWith(files, "Summary.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SummaryUse <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
    # Trim table, assign column names
    SummaryUse <- SummaryUse[!is.na(SummaryUse[, 2]), ]
    colnames(SummaryUse) <- SummaryUse[1, ]
    colname_check <- is.na(colnames(SummaryUse))
    colnames(SummaryUse)[colname_check] <- SummaryUse[2, colname_check]
    # Fill NA in code column with corresponding name
    SummaryUse[is.na(SummaryUse[, 1]), 1] <- SummaryUse[is.na(SummaryUse[, 1]), 2]
    # Convert all values to numeric, assign row names
    SummaryUse <- as.data.frame(lapply(SummaryUse[-c(1:2), -c(1:2)], as.numeric),
                                check.names = FALSE,
                                row.names = SummaryUse[-c(1:2), 1])
    # Replace NA with zero
    SummaryUse[is.na(SummaryUse)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SummaryUse,
                   data_name = paste0("Summary_Use_", year, "_PRO_AfterRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Summary_Use_", year, "_PRO_AfterRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Summary Use (PRO, After Redef, 2012 schema)
getBEASummaryUsePROAfterRedef2012Schema()

# Get BEA Sector Make (Before Redef, 2012 schema) table from static Excel
getBEASectorMakeBeforeRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/AllTablesIO/IOMake_Before_Redefinitions_1997-2019_Sector.xlsx"
  url <- getBEAIOTables()
  # Load data
  SectorMake <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                              skip = 5, n_max = 17),
                           check.names = FALSE)
  SectorMake[is.na(SectorMake[, 1]), 1] <- SectorMake[is.na(SectorMake[, 1]), 2]
  rownames(SectorMake) <- gsub("(^\\s+)|(\\s+$)", "", SectorMake[, 1])
  colnames(SectorMake)[ncol(SectorMake)] <- SectorMake[1, ncol(SectorMake)]
  SectorMake <- data.frame(lapply(SectorMake[-1, -c(1:2)], as.numeric),
                            check.names = FALSE,
                            row.names = rownames(SectorMake[-1, ]))
  SectorMake[is.na(SectorMake)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SectorMake,
                 data_name = paste0("Sector_Make_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Sector_Make_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2010-2018 BEA Sector Make (Before Redef, 2012 schema) table
for (year in 2010:2018) {
  getBEASectorMakeBeforeRedef2012Schema(year)
}

# Get BEA Sector Use (PRO, Before Redef, 2012 schema) table from static Excel
getBEASectorUsePROBeforeRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_1997-2019_Sector.xlsx"
  url <- getBEAIOTables()
  # Load data
  SectorUse <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                             skip = 5, n_max = 28),
                          check.names = FALSE)
  SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
  rownames(SectorUse) <- SectorUse[, 1]
  colname_check <- startsWith(colnames(SectorUse), "...")
  colnames(SectorUse)[colname_check] <- SectorUse[1, colname_check]
  SectorUse <- data.frame(lapply(SectorUse[-1, -c(1:2)], as.numeric),
                           check.names = FALSE,
                           row.names = rownames(SectorUse[-1, ]))
  SectorUse[is.na(SectorUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SectorUse,
                 data_name = paste0("Sector_Use_", year, "_PRO_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Sector_Use_", year, "_PRO_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2010-2018 BEA Sector Use (PRO, Before Redef, 2012 schema) table
for (year in 2010:2018) {
  getBEASectorUsePROBeforeRedef2012Schema(year)
}

# Get BEA Sector Use (PUR, Before Redef, 2012 schema) table from static Excel
getBEASectorUsePURBeforeRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PUR_2007_2012_Sector.xlsx"
  url <- getBEAIOTables()
  # Load data
  SectorUse <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                             skip = 5, n_max = 28),
                          check.names = FALSE)
  SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
  rownames(SectorUse) <- SectorUse[, 1]
  colname_check <- startsWith(colnames(SectorUse), "...")
  colnames(SectorUse)[colname_check] <- SectorUse[1, colname_check]
  SectorUse <- data.frame(lapply(SectorUse[-1, -c(1:2)], as.numeric),
                          check.names = FALSE,
                          row.names = rownames(SectorUse[-1, ]))
  SectorUse[is.na(SectorUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SectorUse,
                 data_name = paste0("Sector_Use_", year, "_PUR_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Sector_Use_", year, "_PUR_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2012 BEA Sector Use (PUR, Before Redef, 2012 schema) table
getBEASectorUsePURBeforeRedef2012Schema(2012)

# Get BEA Sector Make (After Redef, 2012 schema) table from static Excel
getBEASectorMakeAfterRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/AllTablesIO/IOMake_After_Redefinitions_1997-2019_Sector.xlsx"
  url <- getBEAIOTables()
  # Load data
  SectorMake <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                              skip = 5, n_max = 17),
                           check.names = FALSE)
  SectorMake[is.na(SectorMake[, 1]), 1] <- SectorMake[is.na(SectorMake[, 1]), 2]
  rownames(SectorMake) <- gsub("(^\\s+)|(\\s+$)", "", SectorMake[, 1])
  colnames(SectorMake)[ncol(SectorMake)] <- SectorMake[1, ncol(SectorMake)]
  SectorMake <- data.frame(lapply(SectorMake[-1, -c(1:2)], as.numeric),
                           check.names = FALSE,
                           row.names = rownames(SectorMake[-1, ]))
  SectorMake[is.na(SectorMake)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SectorMake,
                 data_name = paste0("Sector_Make_", year, "_AfterRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Sector_Make_", year, "_AfterRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2010-2018 BEA Sector Make (After Redef, 2012 schema) table
for (year in 2010:2018) {
  getBEASectorMakeAfterRedef2012Schema(year)
}
# Get BEA Sector Use (PRO, After Redef, 2012 schema) table from static Excel
getBEASectorUsePROAfterRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/AllTablesIO/IOUse_After_Redefinitions_PRO_1997-2019_Sector.xlsx"
  url <- getBEAIOTables()
  # Load data
  SectorUse <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                             skip = 5, n_max = 28),
                          check.names = FALSE)
  SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
  rownames(SectorUse) <- SectorUse[, 1]
  colname_check <- startsWith(colnames(SectorUse), "...")
  colnames(SectorUse)[colname_check] <- SectorUse[1, colname_check]
  SectorUse <- data.frame(lapply(SectorUse[-1, -c(1:2)], as.numeric),
                          check.names = FALSE,
                          row.names = rownames(SectorUse[-1, ]))
  SectorUse[is.na(SectorUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SectorUse,
                 data_name = paste0("Sector_Use_", year, "_PRO_AfterRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Sector_Use_", year, "_PRO_AfterRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2010-2018 BEA Sector Use (PRO, After Redef, 2012 schema) table
for (year in 2010:2018) {
  getBEASectorUsePROAfterRedef2012Schema(year)
}

# Get BEA Detail Import (Before Redef, 2012 schema) from static Excel
getBEADetailImportBeforeRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/ImportMatrices_Before_Redefinitions_DET_2007_2012.xlsx"
  url <- paste0("https://apps.bea.gov/industry/xls/io-annual",
                gsub("inst/extdata", "", FileName))
  if(!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  # Load data
  DetailImport <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                                skip = 5),
                             check.names = FALSE)
  rownames(DetailImport) <- DetailImport$Code
  DetailImport[, c("Code", "Commodity Description")] <- NULL
  DetailImport[is.na(DetailImport)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailImport,
                 data_name = paste0("Detail_Import_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Import_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2012 BEA Detail Import matrix
getBEADetailImportBeforeRedef2012Schema(2012)

# Get BEA Summary Import (Before Redef, 2012 schema) from static Excel
getBEASummaryImportBeforeRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/ImportMatrices_Before_Redefinitions_SUM_1997-2018.xlsx"
  url <- paste0("https://apps.bea.gov/industry/xls/io-annual",
                gsub("inst/extdata", "", FileName))
  if(!file.exists(FileName)) {
    utils::download.file(url, FileName, mode="wb")
  }
  # Load data
  SummaryImport <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                                skip = 5),
                             check.names = FALSE)
  rownames(SummaryImport) <- SummaryImport$Code
  SummaryImport[, c("Code", "Commodity Description")] <- NULL
  SummaryImport[is.na(SummaryImport)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SummaryImport,
                 data_name = paste0("Summary_Import_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Summary_Import_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2010-2018 BEA Summary Import matrix
for (year in 2010:2018) {
  getBEASummaryImportBeforeRedef2012Schema(year)
}


# Download all GDP tables from BEA iTable
getBEAUnderlyingTables <- function () {
  # Create the placeholder file
  AllTablesUnderlying <- "inst/extdata/AllTablesUnderlying.zip"
  # Download all BEA IO tables into the placeholder file
  url <- "https://edap-ord-data-commons.s3.amazonaws.com/useeior/AllTablesUnderlying.zip"
  if(!file.exists(AllTablesUnderlying)) {
    utils::download.file(url, AllTablesUnderlying, mode = "wb")
  }
  # Get the name of all files in the zip archive
  fname <- unzip(AllTablesUnderlying, list = TRUE)[unzip(AllTablesUnderlying, list = TRUE)$Length > 0, ]$Name
  # Unzip the file to the designated directory
  unzip(AllTablesUnderlying, files = fname, exdir = "inst/extdata/AllTablesUnderlying", overwrite = TRUE)
  return(url)
}

# Get Detail BEA Gross Output (2012 schema) table from static Excel
getBEADetailGrossOutput2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual_Detail.xls"
  DetailGrossOutput <- data.frame(readxl::read_excel(FileName, sheet = "GO",
                                                     skip = 4),
                                  check.names = FALSE)
  DetailGrossOutput <- DetailGrossOutput[, c(2, 8:ncol(DetailGrossOutput))]
  colnames(DetailGrossOutput)[1] <- "Gross_Output_Detail_Industry"
  return(DetailGrossOutput)
}

# Get Summary BEA Gross Output (2012 schema) table from static Excel
getBEASummaryGrossOutput2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SummaryGrossOutput <- data.frame(readxl::read_excel(FileName, sheet = "GO",
                                                      skip = 4, n_max = 192),
                                   check.names = FALSE)
  SummaryGrossOutput <- SummaryGrossOutput[, c(2, 8:ncol(SummaryGrossOutput))]
  colnames(SummaryGrossOutput)[1] <- "Gross_Output_Detail_Industry"
  return(SummaryGrossOutput)
}

# Get Sector BEA Gross Output (2012 schema) table from static Excel
getBEASectorGrossOutput2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SectorGrossOutput <- data.frame(readxl::read_excel(FileName, sheet = "GO",
                                                     skip = 4, n_max = 192),
                                  check.names = FALSE)
  SectorGrossOutput <- SectorGrossOutput[, c(2, 8:ncol(SectorGrossOutput))]
  colnames(SectorGrossOutput)[1] <- "Gross_Output_Detail_Industry"
  return(SectorGrossOutput)
}

# Map gross output ($) from GDP industries to IO industries (2012 schema) at Detail, Summary, and Sector IO levels.
mapBEAGrossOutputtoIOIndustry2012Schema <- function () {
  ### Download all Underlying tables from BEA iTable
  url <- getBEAUnderlyingTables()
  
  ### Detail
  DetailGrossOutput <- getBEADetailGrossOutput2012Schema()
  # Determine year range
  year_range <- colnames(DetailGrossOutput)[2:ncol(DetailGrossOutput)]
  # Map BEA Detail industry code to IO code
  Detail_mapping <- utils::read.table(system.file("inst/extdata",
                                                  "Crosswalk_DetailGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  DetailGrossOutputIO <- merge(Detail_mapping, DetailGrossOutput,
                               by = "Gross_Output_Detail_Industry", all.y = TRUE)
  # Aggregate by BEA Detail industry code
  DetailGrossOutputIO <- stats::aggregate(DetailGrossOutputIO[, year_range],
                                          by = list(DetailGrossOutputIO$BEA_2012_Detail_Code),
                                          sum)
  # Assign rownames as sector code
  rownames(DetailGrossOutputIO) <- DetailGrossOutputIO[, 1]
  DetailGrossOutputIO[, 1] <- NULL
  
  ### Summary
  SummaryGrossOutput <- getBEASummaryGrossOutput2012Schema()
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("inst/extdata",
                                                   "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv",
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SummaryGrossOutputIO <- cbind(Summary_mapping, SummaryGrossOutput)
  # Keep Summary rows
  SummaryGrossOutputIO <- SummaryGrossOutputIO[!SummaryGrossOutputIO$BEA_2012_Summary_Code == "",
                                               c("BEA_2012_Summary_Code", year_range)]
  # Assign rownames as sector code
  rownames(SummaryGrossOutputIO) <- SummaryGrossOutputIO[, 1]
  SummaryGrossOutputIO[, 1] <- NULL
  
  ### Sector
  SectorGrossOutput <- getBEASectorGrossOutput2012Schema()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("inst/extdata",
                                                  "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SectorGrossOutputIO <- cbind(Sector_mapping, SectorGrossOutput)
  # Keep Summary rows
  SectorGrossOutputIO <- SectorGrossOutputIO[!SectorGrossOutputIO$BEA_2012_Sector_Code == "",
                                             c("BEA_2012_Sector_Code", year_range)]
  # Assign rownames as sector code
  rownames(SectorGrossOutputIO) <- SectorGrossOutputIO[, 1]
  SectorGrossOutputIO[, 1] <- NULL
  
  ### Save and Document data
  ls <- list("Detail_GrossOutput_IO" = DetailGrossOutputIO,
             "Summary_GrossOutput_IO" = SummaryGrossOutputIO,
             "Sector_GrossOutput_IO" = SectorGrossOutputIO)
  for (data_name in names(ls)) {
    # Write data to .rda
    writeDatatoRDA(data = ls[[data_name]], data_name = data_name)
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = data_name,
                        year = year_range,
                        source = "US Bureau of Economic Analysis",
                        url = url)
  }
}
# Download, save and document BEA Detail, Summary, and Sector Gross Output tables
mapBEAGrossOutputtoIOIndustry2012Schema()

# Get Detail BEA Chain-Type Price Indexes (CPI) (2012 schema) 2007-2017 tables from static Excel
getBEADetailCPI2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual_Detail.xls"
  DetailCPI <- data.frame(readxl::read_excel(FileName, sheet = "ChainPriceIndexes",
                                             skip = 4),
                          check.names = FALSE)
  DetailCPI <- DetailCPI[, c(2, 8:ncol(DetailCPI))]
  colnames(DetailCPI)[1] <- "Gross_Output_Detail_Industry"
  return(DetailCPI)
}

# Get Summary BEA Chain-Type Price Indexes (CPI) (2012 schema) 2007-2017 tables from static Excel
getBEASummaryCPI2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SummaryCPI <- data.frame(readxl::read_excel(FileName, sheet = "ChainPriceIndexes",
                                              skip = 4, n_max = 192),
                           check.names = FALSE)
  SummaryCPI <- SummaryCPI[, c(2, 8:ncol(SummaryCPI))]
  colnames(SummaryCPI)[1] <- "Gross_Output_Industry"
  return(SummaryCPI)
}

# Get Sector BEA Chain-Type Price Indexes (CPI) (2012 schema) 2007-2017 tables from static Excel
getBEASectorCPI2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/GrossOutputAnnual.xls"
  SectorCPI <- data.frame(readxl::read_excel(FileName, sheet = "ChainPriceIndexes",
                                             skip = 4, n_max = 192),
                          check.names = FALSE)
  SectorCPI <- SectorCPI[, c(2, 8:ncol(SectorCPI))]
  colnames(SectorCPI)[1] <- "Gross_Output_Industry"
  return(SectorCPI)
}

# Map CPI from GDP industries to IO industries (2012 schema) at Detail, Summary, and Sector IO levels.
mapBEACPItoIOIndustry2012Schema <- function () {
  ### Download all Underlying tables from BEA iTable
  url <- getBEAUnderlyingTables()
  
  ### Detail
  DetailCPI <- getBEADetailCPI2012Schema()
  # Determine year range
  year_range <- colnames(DetailCPI)[2:ncol(DetailCPI)]
  # Map BEA Detail industry code to IO code
  Detail_mapping <- utils::read.table(system.file("inst/extdata",
                                                  "Crosswalk_DetailGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  DetailCPIIO <- merge(Detail_mapping, DetailCPI,
                       by = "Gross_Output_Detail_Industry", all.y = TRUE)
  # Adjust (weighted average) CPI based on DetailGrossOutput
  # DetailGrossOutput
  DetailGrossOutput <- getBEADetailGrossOutput2012Schema()
  # Merge CPI with GrossOutput
  DetailCPIIO <- merge(DetailCPIIO, DetailGrossOutput, by = "Gross_Output_Detail_Industry")
  # Calculate weighted average of CPI
  for (code in unique(DetailCPIIO[, "BEA_2012_Detail_Code"])) {
    for (year in year_range) {
      row <- DetailCPIIO$BEA_2012_Detail_Code == code
      DetailCPIIO[row, year] <- stats::weighted.mean(DetailCPIIO[row, paste(year, "x", sep = ".")],
                                                     DetailCPIIO[row, paste(year, "y", sep = ".")])
    }
  }
  # Aggregate CPI by BEA_2012_Detail_Code
  DetailCPIIO <- stats::aggregate(DetailCPIIO[, year_range], by = list(DetailCPIIO$BEA_2012_Detail_Code), mean)
  # Assign rownames as sector code
  rownames(DetailCPIIO) <- DetailCPIIO[, 1]
  DetailCPIIO[, 1] <- NULL
  
  ### Summary
  SummaryCPI <- getBEASummaryCPI2012Schema()
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("inst/extdata",
                                                   "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv",
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SummaryCPIIO <- cbind(Summary_mapping, SummaryCPI)
  # Keep Summary rows
  SummaryCPIIO <- SummaryCPIIO[!SummaryCPIIO$BEA_2012_Summary_Code == "",
                               c("BEA_2012_Summary_Code", year_range)]
  # Assign rownames as sector code
  rownames(SummaryCPIIO) <- SummaryCPIIO[, 1]
  SummaryCPIIO[, 1] <- NULL
  
  ### Sector
  SectorCPI <- getBEASectorCPI2012Schema()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("inst/extdata",
                                                  "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SectorCPIIO <- cbind(Sector_mapping, SectorCPI)
  # Keep Sector rows
  SectorCPIIO <- SectorCPIIO[!SectorCPIIO$BEA_2012_Sector_Code == "",
                             c("BEA_2012_Sector_Code", year_range)]
  # Assign rownames as sector code
  rownames(SectorCPIIO) <- SectorCPIIO[, 1]
  SectorCPIIO[, 1] <- NULL
  
  ### Save and Document data
  ls <- list("Detail_CPI_IO" = DetailCPIIO,
             "Summary_CPI_IO" = SummaryCPIIO,
             "Sector_CPI_IO" = SectorCPIIO)
  for (data_name in names(ls)) {
    # Write data to .rda
    writeDatatoRDA(data = ls[[data_name]], data_name = data_name)
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = data_name,
                        year = year_range,
                        source = "US Bureau of Economic Analysis",
                        url = url)
  }
}
# Download, save and document BEA Detail, Summary, and Sector CPI tables
mapBEACPItoIOIndustry2012Schema()

#' Get Summary BEA Value Added (2012 schema) 2007-2017 tables from static Excel
getBEASummaryValueAdded2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/ValueAddedAnnual.xls"
  SummaryValueAdded <- data.frame(readxl::read_excel(FileName, sheet = "VA",
                                              skip = 4, n_max = 192),
                           check.names = FALSE)
  SummaryValueAdded <- SummaryValueAdded[, c(2, 13:ncol(SummaryValueAdded))]
  colnames(SummaryValueAdded)[1] <- "Industry"
  return(SummaryValueAdded)
}
#' Get Sector BEA Value Added (2012 schema) 2007-2017 tables from static Excel
#' 
#' @return Sector Value Added data from downloaded BEA excel file
getBEASectorValueAdded2012Schema <- function () {
  FileName <- "inst/extdata/AllTablesUnderlying/ValueAddedAnnual.xls"
  SectorValueAdded <- data.frame(readxl::read_excel(FileName, sheet = "VA",
                                                    skip = 4, n_max = 192),
                                 check.names = FALSE)
  SectorValueAdded <- SectorValueAdded[, c(2, 13:ncol(SectorValueAdded))]
  colnames(SectorValueAdded)[1] <- "Industry"
  return(SectorValueAdded)
}

#' Map Value Added ($) from GDP industries to IO industries (2012 schema) at Summary and Sector IO levels.
#' @return A list contains IO-based Value Added at Summary and Sector IO levels.
mapBEAValueAddedtoIOIndustry2012Schema <- function () {
  # Download all Underlying tables from BEA iTable
  url <- getBEAUnderlyingTables()
  
  ### Summary
  SummaryValueAdded <- getBEASummaryValueAdded2012Schema()
  # Determine year range
  year_range <- colnames(SummaryValueAdded)[2:ncol(SummaryValueAdded)]
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("inst/extdata",
                                                   "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv",
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SummaryValueAddedIO <- cbind(Summary_mapping, SummaryValueAdded)
  # Keep Summary rows
  SummaryValueAddedIO <- SummaryValueAddedIO[!SummaryValueAddedIO$BEA_2012_Summary_Code == "",
                                             c("BEA_2012_Summary_Code", year_range)]
  # Assign rownames as sector code
  rownames(SummaryValueAddedIO) <- SummaryValueAddedIO[, 1]
  SummaryValueAddedIO[, 1] <- NULL
  
  # Sector
  SectorValueAdded <- getBEASectorValueAdded2012Schema()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("inst/extdata",
                                                  "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SectorValueAddedIO <- cbind(Sector_mapping, SectorValueAdded)
  # Keep Sector rows
  SectorValueAddedIO <- SectorValueAddedIO[!SectorValueAddedIO$BEA_2012_Sector_Code == "",
                                           c("BEA_2012_Sector_Code", year_range)]
  # Assign rownames as sector code
  rownames(SectorValueAddedIO) <- SectorValueAddedIO[, 1]
  SectorValueAddedIO[, 1] <- NULL
  
  ### Save and Document data
  ls <- list("Summary_ValueAdded_IO" = SummaryValueAddedIO,
             "Sector_ValueAdded_IO" = SectorValueAddedIO)
  for (data_name in names(ls)) {
    # Write data to .rda
    writeDatatoRDA(data = ls[[data_name]], data_name = data_name)
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = data_name,
                        year = year_range,
                        source = "US Bureau of Economic Analysis",
                        url = url)
  }
}
# Download, save and document BEA Summary and Sector Value Added tables
mapBEAValueAddedtoIOIndustry2012Schema()

#'Applies string functions to clean BEA codes in a df
#'@param df, a sector dataframe with codes in col 1
#'@return df, the same df with cleaned codes
cleanSectorCodes <- function(df) {
  codes <- df[,1]
  codes <- removeExtraSpaces(codes)
  df[,1] <- codes
  return(df)
}

#'Applies string functions to clean BEA names in a df
#'@param df, a sector dataframe with sector names in col 2
#'@return df, the same df with cleaned names
cleanSectorNames <- function(df) {
  sec_names <- df[,2]
  sec_names <- removeNumberinSlashes(sec_names)
  sec_names <- convertStrEncodingLatintoASCII(sec_names)
  df[,2] <- sec_names
  return(df)
}

# Get BEA (Detail/Summary/Sector) Code and Name under 2012 schema
getBEACodeName2012Schema <- function () {
  ### Download all IO tables from BEA iTable
  url <- getBEAIOTables()
  ### Load desired excel file
  ## Detail
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_2007_2012_Detail.xlsx"
  BEADetail <- data.frame(readxl::read_excel(FileName, sheet = "2012"))
  # Industry
  BEADetailIndustryCodeName <- data.frame(t(BEADetail[5:4, 3:407]), stringsAsFactors = FALSE)
  colnames(BEADetailIndustryCodeName) <- c("BEA_2012_Detail_Industry_Code", "BEA_2012_Detail_Industry_Name")
  rownames(BEADetailIndustryCodeName) <- NULL
  # Commodity
  BEADetailCommodityCodeName <- BEADetail[6:410, 1:2]
  colnames(BEADetailCommodityCodeName) <- c("BEA_2012_Detail_Commodity_Code", "BEA_2012_Detail_Commodity_Name")
  rownames(BEADetailCommodityCodeName) <- NULL
  # Value Added
  BEADetailValueAddedCodeName <- BEADetail[412:414, 1:2]
  colnames(BEADetailValueAddedCodeName) <- c("BEA_2012_Detail_ValueAdded_Code", "BEA_2012_Detail_ValueAdded_Name")
  rownames(BEADetailValueAddedCodeName) <- NULL
  # Final Demand
  BEADetailFinalDemandCodeName <-  data.frame(t(BEADetail[5:4, 409:428]), stringsAsFactors = FALSE)
  colnames(BEADetailFinalDemandCodeName) <- c("BEA_2012_Detail_FinalDemand_Code", "BEA_2012_Detail_FinalDemand_Name")
  rownames(BEADetailFinalDemandCodeName) <- NULL
  
  ## Summary
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_1997-2019_Summary.xlsx"
  BEASummary <- data.frame(readxl::read_excel(FileName, sheet = "2012"))
  # Industry
  BEASummaryIndustryCodeName <- data.frame(t(BEASummary[5:6, 3:73]), stringsAsFactors = FALSE)
  colnames(BEASummaryIndustryCodeName) <- c("BEA_2012_Summary_Industry_Code", "BEA_2012_Summary_Industry_Name")
  rownames(BEASummaryIndustryCodeName) <- NULL
  # Commodity
  BEASummaryCommodityCodeName <- BEASummary[7:79, 1:2]
  colnames(BEASummaryCommodityCodeName) <- c("BEA_2012_Summary_Commodity_Code", "BEA_2012_Summary_Commodity_Name")
  rownames(BEASummaryCommodityCodeName) <- NULL
  # Value Added
  BEASummaryValueAddedCodeName <- BEASummary[83:85, 1:2]
  colnames(BEASummaryValueAddedCodeName) <- c("BEA_2012_Summary_ValueAdded_Code", "BEA_2012_Summary_ValueAdded_Name")
  rownames(BEASummaryValueAddedCodeName) <- NULL
  # Final Demand
  BEASummaryFinalDemandCodeName <-  data.frame(t(BEASummary[5:6, 77:96]), stringsAsFactors = FALSE)
  colnames(BEASummaryFinalDemandCodeName) <- c("BEA_2012_Summary_FinalDemand_Code", "BEA_2012_Summary_FinalDemand_Name")
  rownames(BEASummaryFinalDemandCodeName) <- NULL
  
  ## Sector
  FileName <- "inst/extdata/AllTablesIO/IOUse_Before_Redefinitions_PRO_1997-2019_Sector.xlsx"
  BEASector <- data.frame(readxl::read_excel(FileName, sheet = "2012"))
  # Industry
  BEASectorIndustryCodeName <- data.frame(t(BEASector[5:6, 3:17]), stringsAsFactors = FALSE)
  colnames(BEASectorIndustryCodeName) <- c("BEA_2012_Sector_Industry_Code", "BEA_2012_Sector_Industry_Name")
  rownames(BEASectorIndustryCodeName) <- NULL
  # Commodity
  BEASectorCommodityCodeName <- BEASector[7:23, 1:2]
  colnames(BEASectorCommodityCodeName) <- c("BEA_2012_Sector_Commodity_Code", "BEA_2012_Sector_Commodity_Name")
  rownames(BEASectorCommodityCodeName) <- NULL
  # Value Added
  BEASectorValueAddedCodeName <- BEASector[27:29, 1:2]
  colnames(BEASectorValueAddedCodeName) <- c("BEA_2012_Sector_ValueAdded_Code", "BEA_2012_Sector_ValueAdded_Name")
  rownames(BEASectorValueAddedCodeName) <- NULL
  # Final Demand
  BEASectorFinalDemandCodeName <-  data.frame(t(BEASector[5:6, 21:26]), stringsAsFactors = FALSE)
  colnames(BEASectorFinalDemandCodeName) <- c("BEA_2012_Sector_FinalDemand_Code", "BEA_2012_Sector_FinalDemand_Name")
  rownames(BEASectorFinalDemandCodeName) <- NULL
  
  ### Put the data.frames in a list
  BEACodeNameList <- list("Detail_IndustryCodeName_2012"     = BEADetailIndustryCodeName,
                          "Detail_CommodityCodeName_2012"    = BEADetailCommodityCodeName,
                          "Detail_ValueAddedCodeName_2012"   = BEADetailValueAddedCodeName,
                          "Detail_FinalDemandCodeName_2012"  = BEADetailFinalDemandCodeName,
                          "Summary_IndustryCodeName_2012"    = BEASummaryIndustryCodeName,
                          "Summary_CommodityCodeName_2012"   = BEASummaryCommodityCodeName,
                          "Summary_ValueAddedCodeName_2012"  = BEASummaryValueAddedCodeName,
                          "Summary_FinalDemandCodeName_2012" = BEASummaryFinalDemandCodeName,
                          "Sector_IndustryCodeName_2012"     = BEASectorIndustryCodeName,
                          "Sector_CommodityCodeName_2012"    = BEASectorCommodityCodeName,
                          "Sector_ValueAddedCodeName_2012"   = BEASectorValueAddedCodeName,
                          "Sector_FinalDemandCodeName_2012"  = BEASectorFinalDemandCodeName)
  BEACodeNameList <- lapply(BEACodeNameList, cleanSectorNames)
  BEACodeNameList <- lapply(BEACodeNameList, cleanSectorCodes)
  ### Save and Document data
  for (data_name in names(BEACodeNameList)) {
    # Write data to .rda
    writeDatatoRDA(data = BEACodeNameList[[data_name]], data_name = data_name)
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = data_name,
                        year = 2012,
                        source = "US Bureau of Economic Analysis",
                        url = url)
  }
}
# Download, save and document BEA Detail, Summary, and Sector Code and Name under 2012 schema
getBEACodeName2012Schema()

# Get Detail Margins (Before Redef, 2012 schema) table from BEA static URL
getBEADetailMarginsBeforeRedef2012Schema <- function (year) {
  # Download data
  FileName <- "inst/extdata/Margins_Before_Redefinitions_2007_2012_DET.xlsx"
  url <- paste0("https://apps.bea.gov/industry/xls/underlying-estimates",
                gsub("inst/extdata", "", FileName))
  if(!file.exists(FileName)) {
    utils::download.file(url, FileName, mode="wb")
  }
  # Load data
  Margins <- data.frame(readxl::read_excel(FileName, sheet = as.character(year),
                                           skip = 4), check.names = FALSE)
  rownames(Margins) <- NULL
  colnames(Margins) <- c("NIPACode", "MarginsCategory",
                         "CommodityCode", "CommodityDescription",
                         "ProducersValue", "Transportation",
                         "Wholesale", "Retail", "PurchasersValue")
  # Write data to .rda
  writeDatatoRDA(data = Margins,
                 data_name = paste0("Detail_Margins_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Margins_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url)
}
# Download, save and document 2012 BEA Detail Margins table
getBEADetailMarginsBeforeRedef2012Schema(2012)
