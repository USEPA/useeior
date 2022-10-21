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
  # Assign row and column names
  DetailMake <- DetailMake[!is.na(DetailMake[, 2]), ]
  colnames(DetailMake) <- DetailMake[1, ]
  rownames(DetailMake) <- DetailMake$Code
  # Trim table, convert all values to numeric, assign row names
  DetailMake <- as.data.frame(lapply(DetailMake[-1, -c(1:2)], as.numeric),
                              check.names = FALSE,
                              row.names = DetailMake[-1, 1])
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
  # Assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  rownames(DetailUse) <- DetailUse$Code
  # Trim table, convert all values to numeric, assign row names
  DetailUse <- as.data.frame(lapply(DetailUse[-1, -c(1:2)], as.numeric),
                             check.names = FALSE,
                             row.names = DetailUse[-1, 1])
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
  # Assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  rownames(DetailUse) <- DetailUse$Code
  # Trim table, convert all values to numeric, assign row names
  DetailUse <- as.data.frame(lapply(DetailUse[-1, -c(1:2)], as.numeric),
                             check.names = FALSE,
                             row.names = DetailUse[-1, 1])
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
  # Assign row and column names
  DetailMake <- DetailMake[!is.na(DetailMake[, 2]), ]
  colnames(DetailMake) <- DetailMake[1, ]
  rownames(DetailMake) <- DetailMake$Code
  # Trim table, convert all values to numeric, assign row names
  DetailMake <- as.data.frame(lapply(DetailMake[-1, -c(1:2)], as.numeric),
                              check.names = FALSE,
                              row.names = DetailMake[-1, 1])
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
  # Assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  rownames(DetailUse) <- DetailUse$Code
  # Trim table, convert all values to numeric, assign row names
  DetailUse <- as.data.frame(lapply(DetailUse[-1, -c(1:2)], as.numeric),
                             check.names = FALSE,
                             row.names = DetailUse[-1, 1])
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
  # Assign row and column names
  DetailUse <- DetailUse[!is.na(DetailUse[, 2]), ]
  colnames(DetailUse) <- DetailUse[1, ]
  rownames(DetailUse) <- DetailUse$Code
  # Trim table, convert all values to numeric, assign row names
  DetailUse <- as.data.frame(lapply(DetailUse[-1, -c(1:2)], as.numeric),
                             check.names = FALSE,
                             row.names = DetailUse[-1, 1])
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
getBEASummaryUsePROAfterRedef2012Schema <- function() {
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
getBEASectorMakeBeforeRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOMake_Before_Redefinitions") &
                  endsWith(files, "Sector.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SectorMake <- data.frame(readxl::read_excel(FileName,
                                                sheet = as.character(year)))
    # Trim table, assign column names
    SectorMake <- SectorMake[!is.na(SectorMake[, 2]), ]
    colnames(SectorMake) <- SectorMake[1, ]
    colname_check <- is.na(colnames(SectorMake))
    colnames(SectorMake)[colname_check] <- SectorMake[2, colname_check]
    # Fill NA in code column with corresponding name
    SectorMake[is.na(SectorMake[, 1]), 1] <- SectorMake[is.na(SectorMake[, 1]), 2]
    # Convert all values to numeric, assign row names
    SectorMake <- as.data.frame(lapply(SectorMake[-c(1:2), -c(1:2)], as.numeric),
                                check.names = FALSE,
                                row.names = SectorMake[-c(1:2), 1])
    # Replace NA with zero
    SectorMake[is.na(SectorMake)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SectorMake,
                   data_name = paste0("Sector_Make_", year, "_BeforeRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Sector_Make_", year, "_BeforeRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Sector Make (Before Redef, 2012 schema)
getBEASectorMakeBeforeRedef2012Schema()

# Get BEA Sector Use (PRO, Before Redef, 2012 schema) table from static Excel
getBEASectorUsePROBeforeRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                  endsWith(files, "Sector.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  for (year in 2010:end_year) {
    SectorUse <- data.frame(readxl::read_excel(FileName,
                                               sheet = as.character(year)))
    # Trim table, assign column names
    SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
    colnames(SectorUse) <- SectorUse[1, ]
    colname_check <- is.na(colnames(SectorUse))
    colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
    # Fill NA in code column with corresponding name
    SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
    # Convert all values to numeric, assign row names
    SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
                               check.names = FALSE,
                               row.names = SectorUse[-c(1:2), 1])
    # Replace NA with zero
    SectorUse[is.na(SectorUse)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SectorUse,
                   data_name = paste0("Sector_Use_", year, "_PRO_BeforeRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Sector_Use_", year, "_PRO_BeforeRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Sector Use (PRO, Before Redef, 2012 schema)
getBEASectorUsePROBeforeRedef2012Schema()

# Get BEA Sector Use (PUR, Before Redef, 2012 schema) table from static Excel
getBEASectorUsePURBeforeRedef2012Schema <- function(year) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOUse_Before_Redefinitions_PUR") &
                  endsWith(files, "Sector.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Load data
  SectorUse <- data.frame(readxl::read_excel(FileName,
                                             sheet = as.character(year)))
  # Trim table, assign column names
  SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
  colnames(SectorUse) <- SectorUse[1, ]
  colname_check <- is.na(colnames(SectorUse))
  colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
  # Fill NA in code column with corresponding name
  SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
  # Convert all values to numeric, assign row names
  SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
                             check.names = FALSE,
                             row.names = SectorUse[-c(1:2), 1])
  # Replace NA with zero
  SectorUse[is.na(SectorUse)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = SectorUse,
                 data_name = paste0("Sector_Use_", year, "_PUR_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Sector_Use_", year, "_PUR_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = date_last_modified,
                      date_accessed = date_accessed)
}
# Download, save and document 2012 BEA Sector Use (PUR, Before Redef, 2012 schema)
getBEASectorUsePURBeforeRedef2012Schema(2012)

# Get BEA Sector Make (After Redef, 2012 schema) table from static Excel
getBEASectorMakeAfterRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOMake_After_Redefinitions") &
                  endsWith(files, "Sector.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SectorMake <- data.frame(readxl::read_excel(FileName,
                                                sheet = as.character(year)))
    # Trim table, assign column names
    SectorMake <- SectorMake[!is.na(SectorMake[, 2]), ]
    colnames(SectorMake) <- SectorMake[1, ]
    colname_check <- is.na(colnames(SectorMake))
    colnames(SectorMake)[colname_check] <- SectorMake[2, colname_check]
    # Fill NA in code column with corresponding name
    SectorMake[is.na(SectorMake[, 1]), 1] <- SectorMake[is.na(SectorMake[, 1]), 2]
    # Convert all values to numeric, assign row names
    SectorMake <- as.data.frame(lapply(SectorMake[-c(1:2), -c(1:2)], as.numeric),
                                check.names = FALSE,
                                row.names = SectorMake[-c(1:2), 1])
    # Replace NA with zero
    SectorMake[is.na(SectorMake)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SectorMake,
                   data_name = paste0("Sector_Make_", year, "_AfterRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Sector_Make_", year, "_AfterRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2020 BEA Sector Make (After Redef, 2012 schema)
getBEASectorMakeAfterRedef2012Schema()

# Get BEA Sector Use (PRO, After Redef, 2012 schema) table from static Excel
getBEASectorUsePROAfterRedef2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "IOUse_After_Redefinitions_PRO") &
                  endsWith(files, "Sector.xlsx")]
  FileName <- file.path("inst/extdata/AllTablesIO", file)
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- file_split[length(file_split) - 1]
  end_year <- sub(".*-", "", year_range)
  for (year in 2010:end_year) {
    SectorUse <- data.frame(readxl::read_excel(FileName,
                                               sheet = as.character(year)))
    # Trim table, assign column names
    SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
    colnames(SectorUse) <- SectorUse[1, ]
    colname_check <- is.na(colnames(SectorUse))
    colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
    # Fill NA in code column with corresponding name
    SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
    # Convert all values to numeric, assign row names
    SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
                               check.names = FALSE,
                               row.names = SectorUse[-c(1:2), 1])
    # Replace NA with zero
    SectorUse[is.na(SectorUse)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SectorUse,
                   data_name = paste0("Sector_Use_", year, "_PRO_AfterRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Sector_Use_", year, "_PRO_AfterRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document 2010-2018 BEA Sector Use (PRO, After Redef, 2012 schema)
getBEASectorUsePROAfterRedef2012Schema()

# Get BEA Detail Import (Before Redef, 2012 schema) from static Excel
getBEADetailImportBeforeRedef2012Schema <- function(year) {
  # Download data
  file <- "ImportMatrices_Before_Redefinitions_DET_2007_2012.xlsx"
  url <- file.path("https://apps.bea.gov/industry/xls/io-annual", file)
  FileName <- file.path("inst/extdata/", file)
  if (!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  # Load data
  DetailImport <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
  # Trim table, assign column names
  DetailImport <- DetailImport[!is.na(DetailImport[, 2]), ]
  colnames(DetailImport) <- DetailImport[1, ]
  colname_check <- is.na(colnames(DetailImport))
  colnames(DetailImport)[colname_check] <- DetailImport[2, colname_check]
  # Fill NA in code column with corresponding name
  DetailImport[is.na(DetailImport[, 1]), 1] <- DetailImport[is.na(DetailImport[, 1]), 2]
  # Convert all values to numeric, assign row names
  DetailImport <- as.data.frame(lapply(DetailImport[-1, -c(1:2)], as.numeric),
                                check.names = FALSE,
                                row.names = DetailImport[-1, 1])
  # Replace NA with zero
  DetailImport[is.na(DetailImport)] <- 0
  # Write data to .rda
  writeDatatoRDA(data = DetailImport,
                 data_name = paste0("Detail_Import_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Import_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = "unknown",
                      date_accessed = as.character(as.Date(file.mtime(FileName))))
}
# Download, save and document 2012 BEA Detail Import matrix
getBEADetailImportBeforeRedef2012Schema(2012)

# Get BEA Summary Import (Before Redef, 2012 schema) from static Excel
getBEASummaryImportBeforeRedef2012Schema <- function() {
  # Download data
  file <- "ImportMatrices_Before_Redefinitions_SUM_1997-2020.xlsx"
  url <- file.path("https://apps.bea.gov/industry/xls/io-annual", file)
  FileName <- file.path("inst/extdata/", file)
  if (!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  # Find latest data year
  file_split <- unlist(stringr::str_split(file, pattern = "_"))
  year_range <- sub(".xlsx", "", file_split[length(file_split)])
  end_year <- sub(".*-", "", year_range)
  # Load data
  for (year in 2010:end_year) {
    SummaryImport <- data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
    # Trim table, assign column names
    SummaryImport <- SummaryImport[!is.na(SummaryImport[, 2]), ]
    colnames(SummaryImport) <- SummaryImport[1, ]
    colname_check <- is.na(colnames(SummaryImport))
    colnames(SummaryImport)[colname_check] <- SummaryImport[2, colname_check]
    # Fill NA in code column with corresponding name
    SummaryImport[is.na(SummaryImport[, 1]), 1] <- SummaryImport[is.na(SummaryImport[, 1]), 2]
    # Convert all values to numeric, assign row names
    SummaryImport <- as.data.frame(lapply(SummaryImport[-c(1:2), -c(1:2)], as.numeric),
                                check.names = FALSE,
                                row.names = SummaryImport[-c(1:2), 1])
    # Replace NA with zero
    SummaryImport[is.na(SummaryImport)] <- 0
    # Write data to .rda
    writeDatatoRDA(data = SummaryImport,
                   data_name = paste0("Summary_Import_", year, "_BeforeRedef"))
    # Write metadata to JSON
    writeMetadatatoJSON(package = "useeior",
                        name = paste0("Summary_Import_", year, "_BeforeRedef"),
                        year = year,
                        source = "US Bureau of Economic Analysis",
                        url = url,
                        date_last_modified = "2022-02-11", # obtained from notes on BEA webpage about "BEA's use tables were updated on Feb.11, 2022."
                        date_accessed = as.character(as.Date(file.mtime(FileName))))
  }
}
# Download, save and document 2010-2020 BEA Summary Import matrix
getBEASummaryImportBeforeRedef2012Schema()


# Download all GDP tables from BEA
getBEAUnderlyingTables <- function() {
  # Create the placeholder file
  UnderlyingTables <- "inst/extdata/UGdpByInd.zip"
  # Download all BEA IO tables into the placeholder file
  url <- "https://apps.bea.gov/industry/Release/ZIP/UGdpByInd.zip"
  if (!file.exists(UnderlyingTables)) {
    utils::download.file(url, UnderlyingTables, mode = "wb")
  }
  # Get the name of all files in the zip archive
  files <- unzip(UnderlyingTables, list = TRUE)
  fname <- files[files$Length > 0, ]$Name
  if (all(fname == basename(fname))) {
    exdir <- "inst/extdata/UGdpByInd"
  } else {
    exdir <- "inst/extdata/"
  }
  # Unzip the file to the designated directory
  unzip(UnderlyingTables, files = fname, exdir = exdir,
        overwrite = TRUE, setTimes = TRUE)
  # Create output
  ls <- list("url" = url,
             "date_accessed" = as.character(as.Date(file.mtime(UnderlyingTables))),
             "files" = basename(fname))
  return(ls)
}

# Get Detail BEA Gross Output (2012 schema) since 2002
getBEADetailGrossOutput2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  sheet <- paste0(content[content$Title ==
                            "U.Gross Output by Industry - Detail Level", "Code"],
                  "-A")
  DetailGrossOutput <- as.data.frame(readxl::read_excel(FileName,
                                                        sheet = sheet))
  # Trim table, assign column names
  DetailGrossOutput <- DetailGrossOutput[!is.na(DetailGrossOutput[, 4]), ]
  colnames(DetailGrossOutput) <- DetailGrossOutput[1, ]
  Gross_Output_Detail_Industry <- DetailGrossOutput[-1, 2]
  # Convert all values to numeric, assign row names
  DetailGrossOutput <- cbind.data.frame(Gross_Output_Detail_Industry,
                             lapply(DetailGrossOutput[-1, -c(1:3)],
                                                  as.numeric))
  # Keep columns since 2002
  col_2002 <- which(colnames(DetailGrossOutput) == "2002")
  DetailGrossOutput <- DetailGrossOutput[, c(1, col_2002:ncol(DetailGrossOutput))]
  return(DetailGrossOutput)
}

# Get Summary BEA Gross Output (2012 schema) since 2002
getBEASummaryGrossOutput2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  sheet <- paste0(content[content$Title == "U.Gross Output by Industry", "Code"],
                  "-A")
  SummaryGrossOutput <- as.data.frame(readxl::read_excel(FileName,
                                                         sheet = sheet))
  # Trim table, assign column names
  SummaryGrossOutput <- SummaryGrossOutput[!is.na(SummaryGrossOutput[, 4]), ]
  colnames(SummaryGrossOutput) <- SummaryGrossOutput[1, ]
  Gross_Output_Industry <- SummaryGrossOutput[-1, 2]
  # Convert all values to numeric, assign row names
  SummaryGrossOutput <- cbind.data.frame(Gross_Output_Industry,
                                         lapply(SummaryGrossOutput[-1, -c(1:3)],
                                                as.numeric))
  # Keep columns since 2002
  col_2002 <- which(colnames(SummaryGrossOutput) == "2002")
  SummaryGrossOutput <- SummaryGrossOutput[, c(1, col_2002:ncol(SummaryGrossOutput))]
  return(SummaryGrossOutput)
}

# Get Sector BEA Gross Output (2012 schema) since 2002
getBEASectorGrossOutput2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  sheet <- paste0(content[content$Title == "U.Gross Output by Industry", "Code"],
                  "-A")
  SectorGrossOutput <- as.data.frame(readxl::read_excel(FileName,
                                                        sheet = sheet))
  # Trim table, assign column names
  SectorGrossOutput <- SectorGrossOutput[!is.na(SectorGrossOutput[, 4]), ]
  colnames(SectorGrossOutput) <- SectorGrossOutput[1, ]
  Gross_Output_Industry <- SectorGrossOutput[-1, 2]
  # Convert all values to numeric, assign row names
  SectorGrossOutput <- cbind.data.frame(Gross_Output_Industry,
                                        lapply(SectorGrossOutput[-1, -c(1:3)],
                                               as.numeric))
  # Keep columns since 2002
  col_2002 <- which(colnames(SectorGrossOutput) == "2002")
  SectorGrossOutput <- SectorGrossOutput[, c(1, col_2002:ncol(SectorGrossOutput))]
  return(SectorGrossOutput)
}

# Map gross output ($) from GDP industries to IO industries (2012 schema) at Detail, Summary, and Sector IO levels.
mapBEAGrossOutputtoIOIndustry2012Schema <- function() {
  # Download data
  url <- getBEAUnderlyingTables()[["url"]]
  date_accessed <- getBEAUnderlyingTables()[["date_accessed"]]
  files <- getBEAUnderlyingTables()[["files"]]
  FileName <- file.path("inst/extdata/UGdpByInd",
                        files[startsWith(files, "GrossOutput")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  
  ### Detail ###
  DetailGrossOutput <- getBEADetailGrossOutput2012Schema()
  # Determine year range
  year_range <- colnames(DetailGrossOutput)[2:ncol(DetailGrossOutput)]
  # Map BEA Detail industry code to IO code
  Detail_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_DetailGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE,
                                      stringsAsFactors = FALSE,
                                      quote = "\"")
  DetailGrossOutputIO <- merge(Detail_mapping, DetailGrossOutput,
                               by = "Gross_Output_Detail_Industry",
                               all.y = TRUE)
  # Aggregate by BEA Detail industry code
  DetailGrossOutputIO <- stats::aggregate(DetailGrossOutputIO[, year_range],
                                          by = list(DetailGrossOutputIO$BEA_2012_Detail_Code),
                                          sum)
  # Assign sector code to row names
  rownames(DetailGrossOutputIO) <- DetailGrossOutputIO[, 1]
  DetailGrossOutputIO[, 1] <- NULL
  
  ### Summary ###
  SummaryGrossOutput <- getBEASummaryGrossOutput2012Schema()
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("extdata",
                                                   "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv",
                                                   package = "useeior"),
                                       sep = ",", header = TRUE,
                                       stringsAsFactors = FALSE)
  SummaryGrossOutputIO <- cbind(Summary_mapping, SummaryGrossOutput)
  # Keep Summary rows
  SummaryGrossOutputIO <- SummaryGrossOutputIO[!SummaryGrossOutputIO$BEA_2012_Summary_Code == "",
                                               c("BEA_2012_Summary_Code", year_range)]
  # Assign sector code to row names
  rownames(SummaryGrossOutputIO) <- SummaryGrossOutputIO[, 1]
  SummaryGrossOutputIO[, 1] <- NULL
  
  ### Sector ###
  SectorGrossOutput <- getBEASectorGrossOutput2012Schema()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SectorGrossOutputIO <- cbind(Sector_mapping, SectorGrossOutput)
  # Keep Summary rows
  SectorGrossOutputIO <- SectorGrossOutputIO[!SectorGrossOutputIO$BEA_2012_Sector_Code == "",
                                             c("BEA_2012_Sector_Code", year_range)]
  # Assign sector code to row names
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
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document BEA Detail, Summary, and Sector Gross Output tables
mapBEAGrossOutputtoIOIndustry2012Schema()

# Get Detail BEA Chain-Type Price Indexes (CPI) (2012 schema) since 2002
getBEADetailCPI2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  dataname <- "U.Chain-Type Price Indexes for Gross Output by Industry"
  sheet <- paste0(content[content$Title == paste(dataname, "- Detail Level"), "Code"],
                  "-A")
  DetailCPI <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  DetailCPI <- DetailCPI[!is.na(DetailCPI[, 4]), ]
  colnames(DetailCPI) <- DetailCPI[1, ]
  Gross_Output_Detail_Industry <- DetailCPI[-1, 2]
  # Convert all values to numeric, assign row names
  DetailCPI <- cbind.data.frame(Gross_Output_Detail_Industry,
                                lapply(DetailCPI[-1, -c(1:3)],
                                       as.numeric))
  # Keep columns since 2002
  col_2002 <- which(colnames(DetailCPI) == "2002")
  DetailCPI <- DetailCPI[, c(1, col_2002:ncol(DetailCPI))]
  return(DetailCPI)
}

# Get Summary BEA Chain-Type Price Indexes (CPI) (2012 schema) since 2002
getBEASummaryCPI2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  dataname <- "U.Chain-Type Price Indexes for Gross Output by Industry"
  sheet <- paste0(content[content$Title == dataname, "Code"], "-A")
  SummaryCPI <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  SummaryCPI <- SummaryCPI[!is.na(SummaryCPI[, 4]), ]
  colnames(SummaryCPI) <- SummaryCPI[1, ]
  Gross_Output_Industry <- SummaryCPI[-1, 2]
  # Convert all values to numeric, assign row names
  SummaryCPI <- cbind.data.frame(Gross_Output_Industry,
                                 lapply(SummaryCPI[-1, -c(1:3)],
                                        as.numeric))
  # Keep columns since 2002
  col_2002 <- which(colnames(SummaryCPI) == "2002")
  SummaryCPI <- SummaryCPI[, c(1, col_2002:ncol(SummaryCPI))]
  return(SummaryCPI)
}

# Get Sector BEA Chain-Type Price Indexes (CPI) (2012 schema) since 2002
getBEASectorCPI2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  dataname <- "U.Chain-Type Price Indexes for Gross Output by Industry"
  sheet <- paste0(content[content$Title == dataname, "Code"], "-A")
  SectorCPI <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  SectorCPI <- SectorCPI[!is.na(SectorCPI[, 4]), ]
  colnames(SectorCPI) <- SectorCPI[1, ]
  Gross_Output_Industry <- SectorCPI[-1, 2]
  # Convert all values to numeric, assign row names
  SectorCPI <- cbind.data.frame(Gross_Output_Industry,
                                 lapply(SectorCPI[-1, -c(1:3)],
                                        as.numeric))
  # Keep columns since 2002
  col_2002 <- which(colnames(SectorCPI) == "2002")
  SectorCPI <- SectorCPI[, c(1, col_2002:ncol(SectorCPI))]
  return(SectorCPI)
}

# Map CPI from GDP industries to IO industries (2012 schema) at Detail, Summary, and Sector IO levels.
mapBEACPItoIOIndustry2012Schema <- function() {
  # Download data
  url <- getBEAUnderlyingTables()[["url"]]
  date_accessed <- getBEAUnderlyingTables()[["date_accessed"]]
  files <- getBEAUnderlyingTables()[["files"]]
  FileName <- file.path("inst/extdata/UGdpByInd",
                        files[startsWith(files, "GrossOutput")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  
  ### Detail ###
  DetailCPI <- getBEADetailCPI2012Schema()
  DetailCPI$Gross_Output_Detail_Industry <- sub("’", "'",
                                                DetailCPI$Gross_Output_Detail_Industry)
  # Determine year range
  year_range <- colnames(DetailCPI)[2:ncol(DetailCPI)]
  # Map BEA Detail industry code to IO code
  Detail_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_DetailGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE,
                                      stringsAsFactors = FALSE,
                                      quote = "\"")
  Detail_mapping$Gross_Output_Detail_Industry <- sub("’", "'",
                                                     Detail_mapping$Gross_Output_Detail_Industry)
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
  DetailCPIIO <- stats::aggregate(DetailCPIIO[, year_range],
                                  by = list(DetailCPIIO$BEA_2012_Detail_Code),
                                  mean)
  # Assign sector code to row names
  rownames(DetailCPIIO) <- DetailCPIIO[, 1]
  DetailCPIIO[, 1] <- NULL
  
  ### Summary ###
  SummaryCPI <- getBEASummaryCPI2012Schema()
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("extdata",
                                                   "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv",
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SummaryCPIIO <- cbind(Summary_mapping, SummaryCPI)
  # Keep Summary rows
  SummaryCPIIO <- SummaryCPIIO[!SummaryCPIIO$BEA_2012_Summary_Code == "",
                               c("BEA_2012_Summary_Code", year_range)]
  # Assign sector code to row names
  rownames(SummaryCPIIO) <- SummaryCPIIO[, 1]
  SummaryCPIIO[, 1] <- NULL
  
  ### Sector ###
  SectorCPI <- getBEASectorCPI2012Schema()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SectorCPIIO <- cbind(Sector_mapping, SectorCPI)
  # Keep Sector rows
  SectorCPIIO <- SectorCPIIO[!SectorCPIIO$BEA_2012_Sector_Code == "",
                             c("BEA_2012_Sector_Code", year_range)]
  # Assign sector code to row names
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
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document BEA Detail, Summary, and Sector CPI tables since 2002
mapBEACPItoIOIndustry2012Schema()

#' Get Summary BEA Value Added (2012 schema) since 2007
getBEASummaryValueAdded2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "ValueAdded")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  dataname <- "U.Value Added by Industry"
  sheet <- paste0(content[content$Title == dataname, "Code"], "-A")
  SummaryValueAdded <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  SummaryValueAdded <- SummaryValueAdded[!is.na(SummaryValueAdded[, 4]), ]
  colnames(SummaryValueAdded) <- SummaryValueAdded[1, ]
  Industry <- SummaryValueAdded[-1, 2]
  # Convert all values to numeric, assign row names
  SummaryValueAdded <- cbind.data.frame(Industry,
                                        lapply(SummaryValueAdded[-1, -c(1:3)],
                                               as.numeric))
  # Keep columns since 2007
  col_2007 <- which(colnames(SummaryValueAdded) == "2007")
  SummaryValueAdded <- SummaryValueAdded[, c(1, col_2007:ncol(SummaryValueAdded))]
  return(SummaryValueAdded)
}

#' Get Sector BEA Value Added (2012 schema) since 2007
getBEASectorValueAdded2012Schema <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "ValueAdded")]
  FileName <- file.path("inst/extdata/UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  dataname <- "U.Value Added by Industry"
  sheet <- paste0(content[content$Title == dataname, "Code"], "-A")
  SectorValueAdded <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  SectorValueAdded <- SectorValueAdded[!is.na(SectorValueAdded[, 4]), ]
  colnames(SectorValueAdded) <- SectorValueAdded[1, ]
  Industry <- SectorValueAdded[-1, 2]
  # Convert all values to numeric, assign row names
  SectorValueAdded <- cbind.data.frame(Industry,
                                       lapply(SectorValueAdded[-1, -c(1:3)],
                                              as.numeric))
  # Keep columns since 2007
  col_2007 <- which(colnames(SectorValueAdded) == "2007")
  SectorValueAdded <- SectorValueAdded[, c(1, col_2007:ncol(SectorValueAdded))]
  return(SectorValueAdded)
}

#' Map Value Added ($) from GDP industries to IO industries (2012 schema)
#' since 2007 at Summary and Sector IO levels
mapBEAValueAddedtoIOIndustry2012Schema <- function() {
  # Download data
  url <- getBEAUnderlyingTables()[["url"]]
  date_accessed <- getBEAUnderlyingTables()[["date_accessed"]]
  files <- getBEAUnderlyingTables()[["files"]]
  FileName <- file.path("inst/extdata/UGdpByInd",
                        files[startsWith(files, "ValueAdded")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  
  ### Summary ###
  SummaryValueAdded <- getBEASummaryValueAdded2012Schema()
  # Determine year range
  year_range <- colnames(SummaryValueAdded)[2:ncol(SummaryValueAdded)]
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("extdata",
                                                   "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv",
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SummaryValueAddedIO <- cbind(Summary_mapping, SummaryValueAdded)
  # Keep Summary rows
  SummaryValueAddedIO <- SummaryValueAddedIO[!SummaryValueAddedIO$BEA_2012_Summary_Code == "",
                                             c("BEA_2012_Summary_Code", year_range)]
  # Assign sector code to row names
  rownames(SummaryValueAddedIO) <- SummaryValueAddedIO[, 1]
  SummaryValueAddedIO[, 1] <- NULL
  
  ### Sector ###
  SectorValueAdded <- getBEASectorValueAdded2012Schema()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  SectorValueAddedIO <- cbind(Sector_mapping, SectorValueAdded)
  # Keep Sector rows
  SectorValueAddedIO <- SectorValueAddedIO[!SectorValueAddedIO$BEA_2012_Sector_Code == "",
                                           c("BEA_2012_Sector_Code", year_range)]
  # Assign sector code to row names
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
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document BEA Summary and Sector Value Added tables since 2002
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
getBEACodeName2012Schema <- function() {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  ### Detail ###
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                                endsWith(files, "Detail.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  BEADetail <- as.data.frame(readxl::read_excel(FileName, sheet = "2012"))
  ## Commodity & Value Added
  DetailCommVA <- BEADetail[!is.na(BEADetail[, 2]), c(1:2)][-1, ]
  commodity_range <- c(1:(which(DetailCommVA[, 1] == "T005") - 1))
  va_range <- c((length(commodity_range) + 2):(which(DetailCommVA[, 1] == "T006") - 1))
  # Commodity
  BEADetailCommodityCodeName <- DetailCommVA[commodity_range, ]
  colnames(BEADetailCommodityCodeName) <- c("BEA_2012_Detail_Commodity_Code",
                                            "BEA_2012_Detail_Commodity_Name")
  rownames(BEADetailCommodityCodeName) <- NULL
  # Value Added
  BEADetailValueAddedCodeName <- DetailCommVA[va_range, ]
  colnames(BEADetailValueAddedCodeName) <- c("BEA_2012_Detail_ValueAdded_Code",
                                             "BEA_2012_Detail_ValueAdded_Name")
  rownames(BEADetailValueAddedCodeName) <- NULL
  ## Industry & Final Demand
  DetailIndFD <- as.data.frame(t(BEADetail[!is.na(BEADetail[, 3]), ][2:1, -c(1:2)]))
  industry_range <- c(1:(which(DetailIndFD[, 1] == "T001") - 1))
  fd_range <- c((length(industry_range) + 2):(which(DetailIndFD[, 1] == "T004") - 1))
  # Industry
  BEADetailIndustryCodeName <- DetailIndFD[industry_range, ]
  colnames(BEADetailIndustryCodeName) <- c("BEA_2012_Detail_Industry_Code",
                                           "BEA_2012_Detail_Industry_Name")
  rownames(BEADetailIndustryCodeName) <- NULL
  # Final Demand
  BEADetailFinalDemandCodeName <-  DetailIndFD[fd_range, ]
  colnames(BEADetailFinalDemandCodeName) <- c("BEA_2012_Detail_FinalDemand_Code",
                                              "BEA_2012_Detail_FinalDemand_Name")
  rownames(BEADetailFinalDemandCodeName) <- NULL
  
  ### Summary ###
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                                endsWith(files, "Summary.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  BEASummary <- as.data.frame(readxl::read_excel(FileName, sheet = "2012"))
  ## Commodity & Value Added
  SummaryCommVA <- BEASummary[!is.na(BEASummary[, 2]), c(1:2)][-c(1:2), ]
  commodity_range <- c(1:(which(SummaryCommVA[, 2] == "Total Intermediate") - 1))
  va_range <- c((length(commodity_range) + 2):(which(SummaryCommVA[, 2] == "Total Value Added") - 1))
  # Commodity
  BEASummaryCommodityCodeName <- SummaryCommVA[commodity_range, ]
  colnames(BEASummaryCommodityCodeName) <- c("BEA_2012_Summary_Commodity_Code",
                                            "BEA_2012_Summary_Commodity_Name")
  rownames(BEASummaryCommodityCodeName) <- NULL
  # Value Added
  BEASummaryValueAddedCodeName <- SummaryCommVA[va_range, ]
  colnames(BEASummaryValueAddedCodeName) <- c("BEA_2012_Summary_ValueAdded_Code",
                                             "BEA_2012_Summary_ValueAdded_Name")
  rownames(BEASummaryValueAddedCodeName) <- NULL
  ## Industry & Final Demand
  SummaryIndFD <- as.data.frame(t(BEASummary[!is.na(BEASummary[, 3]), ][1:2, -c(1:2)]))
  industry_range <- c(1:(which(SummaryIndFD[, 2] == "Total Intermediate") - 1))
  fd_range <- c((length(industry_range) + 2):(which(SummaryIndFD[, 2] == "Total Final Uses (GDP)") - 1))
  # Industry
  BEASummaryIndustryCodeName <- SummaryIndFD[industry_range, ]
  colnames(BEASummaryIndustryCodeName) <- c("BEA_2012_Summary_Industry_Code",
                                            "BEA_2012_Summary_Industry_Name")
  rownames(BEASummaryIndustryCodeName) <- NULL
  # Final Demand
  BEASummaryFinalDemandCodeName <-  SummaryIndFD[fd_range, ]
  colnames(BEASummaryFinalDemandCodeName) <- c("BEA_2012_Summary_FinalDemand_Code",
                                               "BEA_2012_Summary_FinalDemand_Name")
  rownames(BEASummaryFinalDemandCodeName) <- NULL
  
  ### Sector ###
  # Load data
  FileName <- file.path("inst/extdata/AllTablesIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                                endsWith(files, "Sector.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  BEASector <- as.data.frame(readxl::read_excel(FileName, sheet = "2012"))
  ## Commodity & Value Added
  SectorCommVA <- BEASector[!is.na(BEASector[, 2]), c(1:2)][-c(1:2), ]
  commodity_range <- c(1:(which(SectorCommVA[, 2] == "Total Intermediate") - 1))
  va_range <- c((length(commodity_range) + 2):(which(SectorCommVA[, 2] == "Total Value Added") - 1))
  # Commodity
  BEASectorCommodityCodeName <- SectorCommVA[commodity_range, ]
  colnames(BEASectorCommodityCodeName) <- c("BEA_2012_Sector_Commodity_Code",
                                            "BEA_2012_Sector_Commodity_Name")
  rownames(BEASectorCommodityCodeName) <- NULL
  # Value Added
  BEASectorValueAddedCodeName <- SectorCommVA[va_range, ]
  colnames(BEASectorValueAddedCodeName) <- c("BEA_2012_Sector_ValueAdded_Code",
                                             "BEA_2012_Sector_ValueAdded_Name")
  rownames(BEASectorValueAddedCodeName) <- NULL
  ## Industry & Final Demand
  SectorIndFD <- as.data.frame(t(BEASector[!is.na(BEASector[, 3]), ][1:2, -c(1:2)]))
  industry_range <- c(1:(which(SectorIndFD[, 2] == "Total Intermediate") - 1))
  fd_range <- c((length(industry_range) + 2):(which(SectorIndFD[, 2] == "Total Final Uses (GDP)") - 1))
  # Industry
  BEASectorIndustryCodeName <- SectorIndFD[industry_range, ]
  colnames(BEASectorIndustryCodeName) <- c("BEA_2012_Sector_Industry_Code",
                                           "BEA_2012_Sector_Industry_Name")
  rownames(BEASectorIndustryCodeName) <- NULL
  # Final Demand
  BEASectorFinalDemandCodeName <-  SectorIndFD[fd_range, ]
  colnames(BEASectorFinalDemandCodeName) <- c("BEA_2012_Sector_FinalDemand_Code",
                                              "BEA_2012_Sector_FinalDemand_Name")
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
                        url = url,
                        date_last_modified = date_last_modified,
                        date_accessed = date_accessed)
  }
}
# Download, save and document BEA Detail, Summary, and Sector Code and Name (2012 schema)
getBEACodeName2012Schema()

# Get Detail Margins (Before Redefinition, 2012 schema) table from BEA static URL
getBEADetailMarginsBeforeRedef2012Schema <- function(year) {
  # Download data
  file <- "Margins_Before_Redefinitions_2007_2012_DET.xlsx"
  url <- file.path("https://apps.bea.gov/industry/xls/underlying-estimates", file)
  FileName <- file.path("inst/extdata", file)
  if (!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  # Load data
  Margins <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(year)))
  # Trim table, assign column names
  Margins <- Margins[!is.na(Margins[, 4]), ][-1, ]
  rownames(Margins) <- NULL
  colnames(Margins) <- c("NIPACode", "MarginsCategory",
                         "CommodityCode", "CommodityDescription",
                         "ProducersValue", "Transportation",
                         "Wholesale", "Retail", "PurchasersValue")
  # Convert all values to numeric, assign row names
  Margins[, 5:ncol(Margins)] <- as.data.frame(lapply(Margins[, 5:ncol(Margins)],
                                                     as.numeric),
                                              check.names = FALSE)
  # Write data to .rda
  writeDatatoRDA(data = Margins,
                 data_name = paste0("Detail_Margins_", year, "_BeforeRedef"))
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = paste0("Detail_Margins_", year, "_BeforeRedef"),
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = url,
                      date_last_modified = "2022-03-04", # page last modified
                      date_accessed = as.character(as.Date(file.mtime(FileName))))
}
# Download, save and document 2012 BEA Detail Margins table
getBEADetailMarginsBeforeRedef2012Schema(2012)
