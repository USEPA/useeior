url_ls <- c(IO = "https://apps.bea.gov/industry/iTables%20Static%20Files/AllTablesIO.zip",
            imports = "https://apps.bea.gov/industry/xls/io-annual",
            GDP = "https://apps.bea.gov/industry/Release/ZIP/UGdpByInd.zip",
            SUP = "https://apps.bea.gov/industry/iTables%20Static%20Files/AllTablesSUP.zip",
            margins = "https://apps.bea.gov/industry/xls/underlying-estimates"
            )

dir <- file.path(rappdirs::user_data_dir(), "USEEIO-input")
dir.create(dir, showWarnings = FALSE)

# Summary table year range
start_year <- 2012
end_year <- 2023

# Download and unzip all IO tables under Make-Use framework from BEA iTable
getBEAIOTables <- function() {
  # Create the placeholder file
  AllTablesIO <- paste0(dir, "/", "AllTablesIO.zip")
  # Download all BEA IO tables into the placeholder file
  url <- url_ls["IO"]
  if (!file.exists(AllTablesIO)) {
    utils::download.file(url, AllTablesIO, mode = "wb")
  }
  # Get the name of all files in the zip archive
  files <- unzip(AllTablesIO, list = TRUE)
  fname <- files[files$Length > 0, ]$Name
  if (all(fname == basename(fname))) {
    exdir <- paste0(dir, "/", "AllTableIO")
  } else {
    exdir <- dir
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


# Download and unzip all Supply and Use tables from BEA AllTablesSUP.zip
getBEASupplyUseTables <- function() {
  # Create the placeholder file
  AllTablesSUP <- paste0(dir, "/", "AllTablesIOSUP.zip")
  # Download all BEA IO tables into the placeholder file
  url <- url_ls["SUP"]
  if (!file.exists(AllTablesSUP)) {
    utils::download.file(url, AllTablesSUP, mode = "wb")
  }
  # Get the name of all files in the zip archive
  files <- unzip(AllTablesSUP, list = TRUE)
  fname <- files[files$Length > 0, ]$Name
  if (all(fname == basename(fname))) {
    exdir <- paste0(dir, "/", "AllTablesSUP")
  } else {
    exdir <- dir
  }
  # Unzip the file to the designated directory
  unzip(AllTablesSUP, files = fname, exdir = exdir,
        overwrite = TRUE, setTimes = TRUE)
  # Create output
  ls <- list("url" = url,
             "date_accessed" = as.character(as.Date(file.mtime(AllTablesSUP))),
             "files" = basename(fname))
  return(ls)
}

# Download and unzip all GDP tables from BEA
getBEAUnderlyingTables <- function() {
  # Create the placeholder file
  UnderlyingTables <- paste0(dir, "/", "UGdpByInd.zip")
  # Download all BEA IO tables into the placeholder file
  url <- url_ls["GDP"]
  if (!file.exists(UnderlyingTables)) {
    utils::download.file(url, UnderlyingTables, mode = "wb")
  }
  # Get the name of all files in the zip archive
  files <- unzip(UnderlyingTables, list = TRUE)
  fname <- files[files$Length > 0, ]$Name
  if (all(fname == basename(fname))) {
    exdir <- paste0(dir, "/", "UGdpByInd")
  } else {
    exdir <- dir
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

#' Extract table from BEA data 
#' @param year, str IOschema year
#' @param filename, str e.g. "IOUse_Before_Redefinitions_PRO"
#' @param ioschema, str e.g. "Detail"
unpackFile <- function(year, filename, ioschema) {
  # Download data
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  # Load data
  FileName <- file.path(rappdirs::user_data_dir(), "USEEIO-input", "AllTableIO",
                        files[startsWith(files, filename) &
                                endsWith(files, paste0(ioschema, ".xlsx"))])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  df <- as.data.frame(readxl::read_excel(FileName,
                                         sheet = as.character(year)))
  # return multiple items to access later
  ls <- list("df" = df,
             "url" = url,
             "date_accessed" = date_accessed,
             "date_last_modified" = date_last_modified)
  return(ls)
}

#' Process dataframe to remove spaces and assign col/row names
#' @param df, dataframe of matrix (i.e., make or use table)
processDetailMatrix <- function(df) {
  # Assign row and column names
  df <- df[!is.na(df[, 2]), ]
  colnames(df) <- df[1, ]
  rownames(df) <- df$Code
  # Trim table, convert all values to numeric, assign row names
  df <- as.data.frame(lapply(df[-1, -c(1:2)], as.numeric),
                      check.names = FALSE,
                      row.names = df[-1, 1])
  # Replace NA with zero
  df[is.na(df)] <- 0
  return(df)
}

#' Process dataframe to remove spaces and assign col/row names
#' @param df, dataframe of matrix (i.e., make or use table)
processSummaryMatrix <- function(df) {
  # Trim table, assign column names
  df <- df[!is.na(df[, 2]), ]
  colnames(df) <- df[1, ]
  colname_check <- is.na(colnames(df))
  colnames(df)[colname_check] <- df[2, colname_check]
  # Fill NA in code column with corresponding name
  df[is.na(df[, 1]), 1] <- df[is.na(df[, 1]), 2]
  # Convert all values to numeric, assign row names
  df <- as.data.frame(lapply(df[-c(1:2), -c(1:2)], as.numeric),
                              check.names = FALSE,
                              row.names = df[-c(1:2), 1])
  # Replace NA with zero
  df[is.na(df)] <- 0
  return(df)
}

#' Write RDA and json metadata files
#' @param df, dataframe of matrix (i.e., make or use table)
#' @param year, str IO data year
#' @param name, str
#' @param ls, list of metadata items
#' @param scehma_year str of schema year (e.g., 2012 or 2017)
writeFile <- function(df, year, name, ls, schema_year) {
  if (!is.null(schema_year)){
    # append schema to filename e.g., _17sch
    name <- paste0(name, "_", substring(schema_year, 3, 4), "sch")
  }
  # Write data to .rda
  writeDatatoRDA(data = df,
                 data_name = name)
  # Write metadata to JSON
  writeMetadatatoJSON(package = "useeior",
                      name = name,
                      year = year,
                      source = "US Bureau of Economic Analysis",
                      url = ls[["url"]],
                      date_last_modified = ls[["date_last_modified"]],
                      date_accessed = ls[["date_accessed"]])  
}

# Get BEA Detail Make (Before Redef) table from static Excel
getBEADetailMakeBeforeRedef <- function(year, schema_year = NULL) {
  if(is.null(schema_year)) {
    schema_year <- year
  }
  ls <- unpackFile(year, filename="IOMake_Before_Redefinitions", ioschema="Detail")
  DetailMake <- data.frame(ls["df"])
  DetailMake <- processDetailMatrix(DetailMake)
  writeFile(df = DetailMake, year = year,
            name = paste0("Detail_Make_", year, "_BeforeRedef"), ls = ls,
            schema_year = schema_year)
}

# Get BEA Detail Use (PRO, Before Redef) table from static Excel
getBEADetailUsePROBeforeRedef <- function(year, schema_year = NULL) {
  if(is.null(schema_year)) {
    schema_year <- year
  }
  ls <- unpackFile(year, filename="IOUse_Before_Redefinitions_PRO", ioschema="Detail")
  DetailUse <- data.frame(ls["df"])
  DetailUse <- processDetailMatrix(DetailUse)
  writeFile(df = DetailUse, year = year,
            name = paste0("Detail_Use_", year, "_PRO_BeforeRedef"), ls = ls,
            schema_year = schema_year)
}

# Get BEA Detail Use (PUR, Before Redef) table from static Excel
getBEADetailUsePURBeforeRedef <- function(year) {
  ls <- unpackFile(year, filename="IOUse_Before_Redefinitions_PUR", ioschema="Detail")
  DetailUse <- data.frame(ls["df"])
  DetailUse <- processDetailMatrix(DetailUse)
  writeFile(df = DetailUse, year = year,
            name = paste0("Detail_Use_", year, "_PUR_BeforeRedef"), ls = ls,
            schema_year = year)
}

# Get BEA Detail Make (After Redef) table from static Excel
getBEADetailMakeAfterRedef <- function(year) {
  ls <- unpackFile(year, filename="IOMake_After_Redefinitions", ioschema="Detail ")
  ### ^^ Typo in filename from BEA requires extra space in ioschema ^^^
  DetailMake <- data.frame(ls["df"])
  DetailMake <- processDetailMatrix(DetailMake)
  writeFile(df = DetailMake, year = year,
            name = paste0("Detail_Make_", year, "_AfterRedef"), ls = ls,
            schema_year = year)
}

# Get BEA Detail Use (PRO, After Redef) table from static Excel
getBEADetailUsePROAfterRedef <- function(year) {
  ls <- unpackFile(year, filename="IOUse_After_Redefinitions_PRO", ioschema="Detail")
  DetailUse <- data.frame(ls["df"])
  DetailUse <- processDetailMatrix(DetailUse)
  writeFile(df = DetailUse, year = year,
            name = paste0("Detail_Use_", year, "_PRO_AfterRedef"), ls = ls,
            schema_year = year)
}

# Get BEA Detail Use (PUR, After Redef) table from static Excel
getBEADetailUsePURAfterRedef <- function(year) {
  ls <- unpackFile(year, filename="IOUse_After_Redefinitions_PUR", ioschema="Detail")
  DetailUse <- data.frame(ls["df"])
  DetailUse <- processDetailMatrix(DetailUse)
  writeFile(df = DetailUse, year = year,
            name = paste0("Detail_Use_", year, "_PUR_AfterRedef"), ls = ls,
            schema_year = year)
}

# Get BEA Summary Make (Before Redef) table from static Excel
getBEASummaryMakeBeforeRedef <- function(year) {
  for (y in start_year:end_year) {
    ls <- unpackFile(y, filename="IOMake_Before_Redefinitions", ioschema="Summary")
    SummaryMake <- data.frame(ls["df"])
    SummaryMake <- processSummaryMatrix(SummaryMake)
    writeFile(df = SummaryMake, year = y,
              name = paste0("Summary_Make_", y, "_BeforeRedef"), ls = ls,
              schema_year = year)
  }
}

# Get BEA Summary Use (PRO, Before Redef) table from static Excel
getBEASummaryUsePROBeforeRedef <- function(year) {
  for (y in start_year:end_year) {
    ls <- unpackFile(y, filename="IOUse_Before_Redefinitions_PRO", ioschema="Summary")
    SummaryUse <- data.frame(ls["df"])
    SummaryUse <- processSummaryMatrix(SummaryUse)
    writeFile(df = SummaryUse, year = y,
              name = paste0("Summary_Use_", y, "_PRO_BeforeRedef"), ls = ls,
              schema_year = year)
  }
}

# Get BEA Summary Use (PUR, Before Redef) table from static Excel
getBEASummaryUsePURBeforeRedef <- function(year) {
  ls <- unpackFile(year, filename="IOUse_Before_Redefinitions_PUR", ioschema="Summary")
  SummaryUse <- data.frame(ls["df"])
  SummaryUse <- processSummaryMatrix(SummaryUse)
  writeFile(df = SummaryUse, year = year,
            name = paste0("Summary_Use_", year, "_PUR_BeforeRedef"), ls = ls,
            schema_year = year)
}

# Get BEA Summary Make (After Redef) table from static Excel
getBEASummaryMakeAfterRedef <- function(year) {
  for (y in start_year:end_year) {
    ls <- unpackFile(y, filename="IOMake_After_Redefinitions", ioschema="Summary")
    SummaryMake <- data.frame(ls["df"])
    SummaryMake <- processSummaryMatrix(SummaryMake)
    writeFile(df = SummaryMake, year = y,
              name = paste0("Summary_Make_", y, "_AfterRedef"), ls = ls,
              schema_year = year)
  }
}

# Get BEA Summary Use (PRO, After Redef) table from static Excel
getBEASummaryUsePROAfterRedef <- function(year) {
  for (y in start_year:end_year) {
    ls <- unpackFile(y, filename="IOUse_After_Redefinitions_PRO", ioschema="Summary")
    SummaryUse <- data.frame(ls["df"])
    SummaryUse <- processSummaryMatrix(SummaryUse)
    writeFile(df = SummaryUse, year = y,
              name = paste0("Summary_Use_", y, "_PRO_AfterRedef"), ls = ls,
              schema_year = year)
  }
}

# # Get BEA Sector Make (Before Redef, 2012 schema) table from static Excel
# getBEASectorMakeBeforeRedef2012Schema <- function() {
#   # Download data
#   url <- getBEAIOTables()[["url"]]
#   date_accessed <- getBEAIOTables()[["date_accessed"]]
#   files <- getBEAIOTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "IOMake_Before_Redefinitions") &
#                   endsWith(files, "Sector.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesIO", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Find latest data year
#   file_split <- unlist(stringr::str_split(file, pattern = "_"))
#   year_range <- file_split[length(file_split) - 1]
#   end_year <- sub(".*-", "", year_range)
#   # Load data
#   for (year in 2010:end_year) {
#     SectorMake <- data.frame(readxl::read_excel(FileName,
#                                                 sheet = as.character(year)))
#     # Trim table, assign column names
#     SectorMake <- SectorMake[!is.na(SectorMake[, 2]), ]
#     colnames(SectorMake) <- SectorMake[1, ]
#     colname_check <- is.na(colnames(SectorMake))
#     colnames(SectorMake)[colname_check] <- SectorMake[2, colname_check]
#     # Fill NA in code column with corresponding name
#     SectorMake[is.na(SectorMake[, 1]), 1] <- SectorMake[is.na(SectorMake[, 1]), 2]
#     # Convert all values to numeric, assign row names
#     SectorMake <- as.data.frame(lapply(SectorMake[-c(1:2), -c(1:2)], as.numeric),
#                                 check.names = FALSE,
#                                 row.names = SectorMake[-c(1:2), 1])
#     # Replace NA with zero
#     SectorMake[is.na(SectorMake)] <- 0
#     # Write data to .rda
#     writeDatatoRDA(data = SectorMake,
#                    data_name = paste0("Sector_Make_", year, "_BeforeRedef"))
#     # Write metadata to JSON
#     writeMetadatatoJSON(package = "useeior",
#                         name = paste0("Sector_Make_", year, "_BeforeRedef"),
#                         year = year,
#                         source = "US Bureau of Economic Analysis",
#                         url = url,
#                         date_last_modified = date_last_modified,
#                         date_accessed = date_accessed)
#   }
# }
# 
# # Get BEA Sector Use (PRO, Before Redef, 2012 schema) table from static Excel
# getBEASectorUsePROBeforeRedef2012Schema <- function() {
#   # Download data
#   url <- getBEAIOTables()[["url"]]
#   date_accessed <- getBEAIOTables()[["date_accessed"]]
#   files <- getBEAIOTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
#                   endsWith(files, "Sector.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesIO", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Find latest data year
#   file_split <- unlist(stringr::str_split(file, pattern = "_"))
#   year_range <- file_split[length(file_split) - 1]
#   end_year <- sub(".*-", "", year_range)
#   for (year in 2010:end_year) {
#     SectorUse <- data.frame(readxl::read_excel(FileName,
#                                                sheet = as.character(year)))
#     # Trim table, assign column names
#     SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
#     colnames(SectorUse) <- SectorUse[1, ]
#     colname_check <- is.na(colnames(SectorUse))
#     colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
#     # Fill NA in code column with corresponding name
#     SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
#     # Convert all values to numeric, assign row names
#     SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
#                                check.names = FALSE,
#                                row.names = SectorUse[-c(1:2), 1])
#     # Replace NA with zero
#     SectorUse[is.na(SectorUse)] <- 0
#     # Write data to .rda
#     writeDatatoRDA(data = SectorUse,
#                    data_name = paste0("Sector_Use_", year, "_PRO_BeforeRedef"))
#     # Write metadata to JSON
#     writeMetadatatoJSON(package = "useeior",
#                         name = paste0("Sector_Use_", year, "_PRO_BeforeRedef"),
#                         year = year,
#                         source = "US Bureau of Economic Analysis",
#                         url = url,
#                         date_last_modified = date_last_modified,
#                         date_accessed = date_accessed)
#   }
# }
# 
# # Get BEA Sector Use (PUR, Before Redef, 2012 schema) table from static Excel
# getBEASectorUsePURBeforeRedef2012Schema <- function(year) {
#   # Download data
#   url <- getBEAIOTables()[["url"]]
#   date_accessed <- getBEAIOTables()[["date_accessed"]]
#   files <- getBEAIOTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "IOUse_Before_Redefinitions_PUR") &
#                   endsWith(files, "Sector.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesIO", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Load data
#   SectorUse <- data.frame(readxl::read_excel(FileName,
#                                              sheet = as.character(year)))
#   # Trim table, assign column names
#   SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
#   colnames(SectorUse) <- SectorUse[1, ]
#   colname_check <- is.na(colnames(SectorUse))
#   colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
#   # Fill NA in code column with corresponding name
#   SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
#   # Convert all values to numeric, assign row names
#   SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
#                              check.names = FALSE,
#                              row.names = SectorUse[-c(1:2), 1])
#   # Replace NA with zero
#   SectorUse[is.na(SectorUse)] <- 0
#   # Write data to .rda
#   writeDatatoRDA(data = SectorUse,
#                  data_name = paste0("Sector_Use_", year, "_PUR_BeforeRedef"))
#   # Write metadata to JSON
#   writeMetadatatoJSON(package = "useeior",
#                       name = paste0("Sector_Use_", year, "_PUR_BeforeRedef"),
#                       year = year,
#                       source = "US Bureau of Economic Analysis",
#                       url = url,
#                       date_last_modified = date_last_modified,
#                       date_accessed = date_accessed)
# }
# 
# # Get BEA Sector Make (After Redef, 2012 schema) table from static Excel
# getBEASectorMakeAfterRedef2012Schema <- function() {
#   # Download data
#   url <- getBEAIOTables()[["url"]]
#   date_accessed <- getBEAIOTables()[["date_accessed"]]
#   files <- getBEAIOTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "IOMake_After_Redefinitions") &
#                   endsWith(files, "Sector.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesIO", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Find latest data year
#   file_split <- unlist(stringr::str_split(file, pattern = "_"))
#   year_range <- file_split[length(file_split) - 1]
#   end_year <- sub(".*-", "", year_range)
#   # Load data
#   for (year in 2010:end_year) {
#     SectorMake <- data.frame(readxl::read_excel(FileName,
#                                                 sheet = as.character(year)))
#     # Trim table, assign column names
#     SectorMake <- SectorMake[!is.na(SectorMake[, 2]), ]
#     colnames(SectorMake) <- SectorMake[1, ]
#     colname_check <- is.na(colnames(SectorMake))
#     colnames(SectorMake)[colname_check] <- SectorMake[2, colname_check]
#     # Fill NA in code column with corresponding name
#     SectorMake[is.na(SectorMake[, 1]), 1] <- SectorMake[is.na(SectorMake[, 1]), 2]
#     # Convert all values to numeric, assign row names
#     SectorMake <- as.data.frame(lapply(SectorMake[-c(1:2), -c(1:2)], as.numeric),
#                                 check.names = FALSE,
#                                 row.names = SectorMake[-c(1:2), 1])
#     # Replace NA with zero
#     SectorMake[is.na(SectorMake)] <- 0
#     # Write data to .rda
#     writeDatatoRDA(data = SectorMake,
#                    data_name = paste0("Sector_Make_", year, "_AfterRedef"))
#     # Write metadata to JSON
#     writeMetadatatoJSON(package = "useeior",
#                         name = paste0("Sector_Make_", year, "_AfterRedef"),
#                         year = year,
#                         source = "US Bureau of Economic Analysis",
#                         url = url,
#                         date_last_modified = date_last_modified,
#                         date_accessed = date_accessed)
#   }
# }
# 
# # Get BEA Sector Use (PRO, After Redef, 2012 schema) table from static Excel
# getBEASectorUsePROAfterRedef2012Schema <- function() {
#   # Download data
#   url <- getBEAIOTables()[["url"]]
#   date_accessed <- getBEAIOTables()[["date_accessed"]]
#   files <- getBEAIOTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "IOUse_After_Redefinitions_PRO") &
#                   endsWith(files, "Sector.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesIO", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Find latest data year
#   file_split <- unlist(stringr::str_split(file, pattern = "_"))
#   year_range <- file_split[length(file_split) - 1]
#   end_year <- sub(".*-", "", year_range)
#   for (year in 2010:end_year) {
#     SectorUse <- data.frame(readxl::read_excel(FileName,
#                                                sheet = as.character(year)))
#     # Trim table, assign column names
#     SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
#     colnames(SectorUse) <- SectorUse[1, ]
#     colname_check <- is.na(colnames(SectorUse))
#     colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
#     # Fill NA in code column with corresponding name
#     SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
#     # Convert all values to numeric, assign row names
#     SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
#                                check.names = FALSE,
#                                row.names = SectorUse[-c(1:2), 1])
#     # Replace NA with zero
#     SectorUse[is.na(SectorUse)] <- 0
#     # Write data to .rda
#     writeDatatoRDA(data = SectorUse,
#                    data_name = paste0("Sector_Use_", year, "_PRO_AfterRedef"))
#     # Write metadata to JSON
#     writeMetadatatoJSON(package = "useeior",
#                         name = paste0("Sector_Use_", year, "_PRO_AfterRedef"),
#                         year = year,
#                         source = "US Bureau of Economic Analysis",
#                         url = url,
#                         date_last_modified = date_last_modified,
#                         date_accessed = date_accessed)
#   }
# }

# Get BEA Detail Import (Before Redef schema) from static Excel
getBEADetailImportBeforeRedef <- function(year, schema_year = NULL) {
  if(is.null(schema_year)) {
    schema_year <- year
  }
  # Download data
  file <- paste0("ImportMatrices_Before_Redefinitions_DET_", schema_year, ".xlsx")
  url <- file.path(url_ls["imports"], file)
  FileName <- file.path(dir, "/", file)
  if (!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  # Load data
  DetailImport <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
  DetailImport <- processDetailMatrix(DetailImport)
  ls <- list("url" = url,
             "date_accessed" = as.character(as.Date(file.mtime(FileName))),
             "date_last_modified" = "unknown")
  writeFile(df = DetailImport, year = year,
            name = paste0("Detail_Import_", year, "_BeforeRedef"), ls = ls,
            schema_year = schema_year)
}

# Get BEA Summary Import (Before Redef) from static Excel
getBEASummaryImportBeforeRedef <- function(year) {
  # Download data
  file <- "ImportMatrices_Before_Redefinitions_SUM_1997-2023.xlsx"
  url <- file.path(url_ls["imports"], file)
  FileName <- file.path(dir, file)
  if (!file.exists(FileName)) {
    utils::download.file(url, FileName, mode = "wb")
  }
  # Load data
  for (y in start_year:end_year) {
    SummaryImport <- data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(y)))
    SummaryImport <- processSummaryMatrix(SummaryImport)
    ls <- list("url" = url,
               "date_accessed" = as.character(as.Date(file.mtime(FileName))),
               "date_last_modified" = "2024-11-01") # page last modified
    writeFile(df = SummaryImport, year = y,
              name = paste0("Summary_Import_", y, "_BeforeRedef"), ls = ls,
              schema_year = year)
  }
}


#' Get Detail BEA Gross Output
#' @param level, str "Detail", "Summary", or "Sector"
getBEAGrossOutput <- function(level) {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path(dir, "UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  if(level=="Detail"){
    tag = "U.Gross Output by Industry - Detail Level"
  } else {
    tag = "U.Gross Output by Industry"
  }
  sheet <- paste0(content[content$Title == tag, "Code"], "-A")
  GrossOutput <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  GrossOutput <- GrossOutput[!is.na(GrossOutput[, 4]), ]
  colnames(GrossOutput) <- GrossOutput[1, ]
  sector <- GrossOutput[-1, 2]
  # Convert all values to numeric, assign row names
  GrossOutput <- cbind.data.frame(sector, 
                                  lapply(GrossOutput[-1, -c(1:3)], as.numeric))
  return(GrossOutput)
}


# Map gross output ($) from GDP industries to IO industries at Detail, Summary, and Sector IO levels.
mapBEAGrossOutputtoIOIndustry <- function(schema_year) {
  ls <- getBEAUnderlyingTables()
  FileName <- file.path(dir, "UGdpByInd",
                        ls[["files"]][startsWith(ls[["files"]], "GrossOutput")])
  ls["date_last_modified"] <- as.character(as.Date(file.mtime(FileName)))
  ### Detail ###
  DetailGrossOutput <- getBEAGrossOutput(level="Detail")
  # Determine year range
  year_range <- colnames(DetailGrossOutput)[2:ncol(DetailGrossOutput)]
  # Map BEA Detail industry code to IO code
  Detail_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_DetailGDPIndustrytoIO.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE,
                                      stringsAsFactors = FALSE,
                                      quote = "\"")
  Detail_mapping <- Detail_mapping[,c("Gross_Output_Detail_Industry",
                                      paste0("BEA_", schema_year, "_Detail_Code"))]
  colnames(Detail_mapping) <- c("sector", "BEA_Detail_Code")
  DetailGrossOutputIO <- merge(Detail_mapping, DetailGrossOutput,
                               by = "sector",
                               all.y = TRUE)
  # Aggregate by BEA Detail industry code
  DetailGrossOutputIO <- stats::aggregate(DetailGrossOutputIO[, year_range],
                                          by = list(DetailGrossOutputIO$BEA_Detail_Code),
                                          sum)
  # Assign sector code to row names
  rownames(DetailGrossOutputIO) <- DetailGrossOutputIO[, 1]
  DetailGrossOutputIO[, 1] <- NULL

  ### Summary ###
  SummaryGrossOutput <- getBEAGrossOutput(level="Summary")
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("extdata",
                                                   paste0("Crosswalk_SummaryGDPIndustrytoIO", 2012, "Schema.csv"),
                                                   package = "useeior"),
                                       sep = ",", header = TRUE,
                                       stringsAsFactors = FALSE)
  colnames(Summary_mapping) <- c("sector", "BEA_Summary_Code")
  ## TODO can update crosswalk file for 2017, 2017 summary schema is same as 2012 ^^
  SummaryGrossOutputIO <- cbind(Summary_mapping, SummaryGrossOutput)
  # Keep Summary rows
  SummaryGrossOutputIO <- SummaryGrossOutputIO[!SummaryGrossOutputIO$BEA_Summary_Code == "",
                                               c("BEA_Summary_Code", year_range)]
  # Assign sector code to row names
  rownames(SummaryGrossOutputIO) <- SummaryGrossOutputIO[, 1]
  SummaryGrossOutputIO[, 1] <- NULL

  ### Sector ###
  SectorGrossOutput <- getBEAGrossOutput(level="Sector")
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("extdata",
                                                  paste0("Crosswalk_SectorGDPIndustrytoIO", 2012, "Schema.csv"),
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  colnames(Sector_mapping) <- c("sector", "BEA_Sector_Code")
  ## TODO can update crosswalk file for 2017, 2017 sector schema is same as 2012 ^^
  SectorGrossOutputIO <- cbind(Sector_mapping, SectorGrossOutput)
  # Keep Summary rows
  SectorGrossOutputIO <- SectorGrossOutputIO[!SectorGrossOutputIO$BEA_Sector_Code == "",
                                             c("BEA_Sector_Code", year_range)]
  # Assign sector code to row names
  rownames(SectorGrossOutputIO) <- SectorGrossOutputIO[, 1]
  SectorGrossOutputIO[, 1] <- NULL

  ### Save and Document data
  dfs <- list("Detail_GrossOutput_IO" = DetailGrossOutputIO,
             "Summary_GrossOutput_IO" = SummaryGrossOutputIO,
             "Sector_GrossOutput_IO" = SectorGrossOutputIO)
  for (data_name in names(dfs)) {
    writeFile(df = dfs[[data_name]], year = year_range,
              name = data_name, ls = ls,
              schema_year = schema_year)
  }
}

#' Get Detail BEA Chain-Type Price Indexes (CPI)
#' @param level, str "Detail", "Summary", or "Sector"
getBEACPI <- function(level) {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "GrossOutput")]
  FileName <- file.path(dir, "UGdpByInd", file)
  # Load data
  content <- na.omit(as.data.frame(readxl::read_excel(FileName,
                                                      sheet = "Contents",
                                                      na = "")))
  if(level=="Detail"){
    tag = "U.Chain-Type Price Indexes for Gross Output by Industry - Detail Level"
  } else {
    tag = "U.Chain-Type Price Indexes for Gross Output by Industry"
  }
  sheet <- paste0(content[content$Title == tag, "Code"], "-A")
  CPI <- as.data.frame(readxl::read_excel(FileName, sheet = sheet))
  # Trim table, assign column names
  CPI <- CPI[!is.na(CPI[, 4]), ]
  colnames(CPI) <- CPI[1, ]
  sector <- CPI[-1, 2]
  # Convert all values to numeric, assign row names
  CPI <- cbind.data.frame(sector,
                          lapply(CPI[-1, -c(1:3)], as.numeric))
  return(CPI)
}


# Map CPI from GDP industries to IO industries at Detail, Summary, and Sector IO levels.
mapBEACPItoIOIndustry <- function(schema_year) {
  ls <- getBEAUnderlyingTables()
  FileName <- file.path(dir, "UGdpByInd",
                        ls[["files"]][startsWith(ls[["files"]], "GrossOutput")])
  ls["date_last_modified"] <- as.character(as.Date(file.mtime(FileName)))

  ### Detail ###
  DetailCPI <- getBEACPI(level="Detail")
  DetailCPI$sector <- sub("’", "'", DetailCPI$sector)
  # Determine year range
  year_range <- colnames(DetailCPI)[2:ncol(DetailCPI)]
  # Map BEA Detail industry code to IO code
  Detail_mapping <- utils::read.table(system.file("extdata",
                                                  "Crosswalk_DetailGDPIndustrytoIO.csv",
                                                  package = "useeior"),
                                      sep = ",", header = TRUE,
                                      stringsAsFactors = FALSE,
                                      quote = "\"")
  Detail_mapping$Gross_Output_Detail_Industry <- sub("’", "'",
                                                     Detail_mapping$Gross_Output_Detail_Industry)
  Detail_mapping <- Detail_mapping[,c("Gross_Output_Detail_Industry",
                                      paste0("BEA_", schema_year, "_Detail_Code"))]
  colnames(Detail_mapping) <- c("sector", "BEA_Detail_Code")
  DetailCPIIO <- merge(Detail_mapping, DetailCPI,
                       by = "sector", all.y = TRUE)
  if(sum(is.na(DetailCPIIO$BEA_Detail_Code)) > 0){
    print('ERROR: missing mappings')
    DetailCPIIO$BEA_Detail_Code[is.na(DetailCPIIO$BEA_Detail_Code)] <- "missing"
  }
  # Adjust (weighted average) CPI based on DetailGrossOutput
  # DetailGrossOutput
  DetailGrossOutput <- getBEAGrossOutput(level="Detail")
  # Merge CPI with GrossOutput
  DetailCPIIO <- merge(DetailCPIIO, DetailGrossOutput, by = "sector")
  # Calculate weighted average of CPI
  for (code in unique(DetailCPIIO[, "BEA_Detail_Code"])) {
    for (y in year_range) {
      row <- DetailCPIIO$BEA_Detail_Code == code
      DetailCPIIO[row, y] <- stats::weighted.mean(DetailCPIIO[row, paste(y, "x", sep = ".")],
                                                  DetailCPIIO[row, paste(y, "y", sep = ".")])
    }
  }
  # Aggregate CPI by BEA_Detail_Code
  DetailCPIIO <- stats::aggregate(DetailCPIIO[, year_range],
                                  by = list(DetailCPIIO$BEA_Detail_Code),
                                  mean)
  # Assign sector code to row names
  rownames(DetailCPIIO) <- DetailCPIIO[, 1]
  DetailCPIIO[, 1] <- NULL

  ### Summary ###
  SummaryCPI <- getBEACPI(level="Summary")
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("extdata",
                                                   paste0("Crosswalk_SummaryGDPIndustrytoIO", 2012,"Schema.csv"),
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  colnames(Summary_mapping) <- c("sector", "BEA_Summary_Code")
  ## TODO can update crosswalk file for 2017, 2017 summary schema is same as 2012 ^^
  SummaryCPIIO <- cbind(Summary_mapping, SummaryCPI)
  # Keep Summary rows
  SummaryCPIIO <- SummaryCPIIO[!SummaryCPIIO$BEA_Summary_Code == "",
                               c("BEA_Summary_Code", year_range)]
  # Assign sector code to row names
  rownames(SummaryCPIIO) <- SummaryCPIIO[, 1]
  SummaryCPIIO[, 1] <- NULL

  ### Sector ###
  SectorCPI <- getBEACPI(level="Sector")
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("extdata",
                                                  paste0("Crosswalk_SectorGDPIndustrytoIO", 2012,"Schema.csv"),
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  colnames(Sector_mapping) <- c("sector", "BEA_Sector_Code")
  ## TODO can update crosswalk file for 2017, 2017 sector schema is same as 2012 ^^
  SectorCPIIO <- cbind(Sector_mapping, SectorCPI)
  # Keep Sector rows
  SectorCPIIO <- SectorCPIIO[!SectorCPIIO$BEA_Sector_Code == "",
                             c("BEA_Sector_Code", year_range)]
  # Assign sector code to row names
  rownames(SectorCPIIO) <- SectorCPIIO[, 1]
  SectorCPIIO[, 1] <- NULL

  ### Save and Document data
  dfs <- list("Detail_CPI_IO" = DetailCPIIO,
             "Summary_CPI_IO" = SummaryCPIIO,
             "Sector_CPI_IO" = SectorCPIIO)
  for (data_name in names(dfs)) {
    writeFile(df = dfs[[data_name]], year = year_range,
              name = data_name, ls = ls,
              schema_year = schema_year)
  }
}

#' Get Summary BEA Value Added
getBEASummaryValueAdded <- function() {
  # Download data
  files <- getBEAUnderlyingTables()[["files"]]
  # Prepare file name
  file <- files[startsWith(files, "ValueAdded")]
  FileName <- file.path(dir, "UGdpByInd", file)
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
  return(SummaryValueAdded)
}


#' Map Value Added ($) from GDP industries to IO industries
#' at Summary and Sector IO levels
mapBEAValueAddedtoIOIndustry <- function(schema_year) {
  # Download data
  ls <- getBEAUnderlyingTables()
  FileName <- file.path(dir, "UGdpByInd",
                        ls[["files"]][startsWith(ls[["files"]], "ValueAdded")])
  ls["date_last_modified"] <- as.character(as.Date(file.mtime(FileName)))

  ### Summary ###
  SummaryValueAdded <- getBEASummaryValueAdded()
  # Determine year range
  year_range <- colnames(SummaryValueAdded)[2:ncol(SummaryValueAdded)]
  # Map BEA Summary industry code to IO code
  Summary_mapping <- utils::read.table(system.file("extdata",
                                                   paste0("Crosswalk_SummaryGDPIndustrytoIO", 2012,"Schema.csv"),
                                                   package = "useeior"),
                                       sep = ",", header = TRUE, stringsAsFactors = FALSE)
  colnames(Summary_mapping) <- c("Gross_Output_Industry","BEA_Summary_Code")
  ## TODO can update crosswalk file for 2017, 2017 summary schema is same as 2012 ^^
  SummaryValueAddedIO <- cbind(Summary_mapping, SummaryValueAdded)
  # Keep Summary rows
  SummaryValueAddedIO <- SummaryValueAddedIO[!SummaryValueAddedIO$BEA_Summary_Code == "",
                                             c("BEA_Summary_Code", year_range)]
  # Assign sector code to row names
  rownames(SummaryValueAddedIO) <- SummaryValueAddedIO[, 1]
  SummaryValueAddedIO[, 1] <- NULL

  ### Sector ###
  SectorValueAdded <- getBEASummaryValueAdded()
  # Map BEA Sector industry code to IO code
  Sector_mapping <- utils::read.table(system.file("extdata",
                                                  paste0("Crosswalk_SectorGDPIndustrytoIO", 2012,"Schema.csv"),
                                                  package = "useeior"),
                                      sep = ",", header = TRUE, stringsAsFactors = FALSE)
  colnames(Sector_mapping) <- c("Gross_Output_Industry","BEA_Sector_Code")
  ## TODO can update crosswalk file for 2017, 2017 sector schema is same as 2012 ^^
  SectorValueAddedIO <- cbind(Sector_mapping, SectorValueAdded)
  # Keep Sector rows
  SectorValueAddedIO <- SectorValueAddedIO[!SectorValueAddedIO$BEA_Sector_Code == "",
                                           c("BEA_Sector_Code", year_range)]
  # Assign sector code to row names
  rownames(SectorValueAddedIO) <- SectorValueAddedIO[, 1]
  SectorValueAddedIO[, 1] <- NULL

  ### Save and Document data
  dfs <- list("Summary_ValueAdded_IO" = SummaryValueAddedIO,
             "Sector_ValueAdded_IO" = SectorValueAddedIO)
  for (data_name in names(dfs)) {
    writeFile(df = dfs[[data_name]], year = year_range,
              name = data_name, ls = ls,
              schema_year = schema_year)
  }
}

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

# Get BEA (Detail/Summary/Sector) Code and Name
getBEACodeName <- function(schema_year) {
  # Download data
  # Get the data from AllTablesIO
  url <- getBEAIOTables()[["url"]]
  date_accessed <- getBEAIOTables()[["date_accessed"]]
  files <- getBEAIOTables()[["files"]]
  FileName <- file.path(dir, "AllTableIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                        endsWith(files, "Detail.xlsx")])

  # Get the data from AllTablesSUP
  # url <- getBEASupplyUseTables()[["url"]]
  # date_accessed <- getBEASupplyUseTables()[["date_accessed"]]
  # files <- getBEASupplyUseTables()[["files"]]
  # FileName <- file.path(dir, "AllTablesSUP",
  #                       files[startsWith(files, paste0("Use_SUT_Framework_",schema_year,"_DET.xlsx"))])  

  date_last_modified <- as.character(as.Date(file.mtime(FileName)))

  ### Detail ###
  # Load data
  BEADetail <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(schema_year)))
  ## Commodity & Value Added
  DetailCommVA <- BEADetail[!is.na(BEADetail[, 2]), c(1:2)][-1, ]
  commodity_range <- c(1:(which(DetailCommVA[, 1] == "T005") - 1))
  # value added range in MUT Use table
  va_range <- c((length(commodity_range) + 2):(which(DetailCommVA[, 1] == "T006") - 1))
  # Value added range from SUT
  # va_range <- c((length(commodity_range) + 2):(which(DetailCommVA[, 1] == "VABAS") - 1))
  # va_range <- append(va_range,c((max(va_range) + 3):(which(DetailCommVA[, 1] == "VAPRO") - 1)))
  
  # Commodity
  BEADetailCommodityCodeName <- DetailCommVA[commodity_range, ]
  colnames(BEADetailCommodityCodeName) <- c(paste0("BEA_",schema_year,"_Detail_Commodity_Code"),
                                            paste0("BEA_",schema_year,"_Detail_Commodity_Name"))
  rownames(BEADetailCommodityCodeName) <- NULL
  # Value Added
  BEADetailValueAddedCodeName <- DetailCommVA[va_range, ]
  colnames(BEADetailValueAddedCodeName) <- c(paste0("BEA_",schema_year,"_Detail_ValueAdded_Code"),
                                             paste0("BEA_",schema_year,"_Detail_ValueAdded_Name"))
  rownames(BEADetailValueAddedCodeName) <- NULL
  ## Industry & Final Demand
  DetailIndFD <- as.data.frame(t(BEADetail[!is.na(BEADetail[, 3]), ][2:1, -c(1:2)]))
  industry_range <- c(1:(which(DetailIndFD[, 1] == "T001") - 1))
  
  # FD range from MUT Use
  fd_range <- c((length(industry_range) + 2):(which(DetailIndFD[, 2] == "Total Final Uses (GDP)") - 1))

  # FD range from SUT Use
  # fd_range <- c((length(industry_range) + 2):(which(DetailIndFD[, 1] == "T019") - 1))
  
  # Industry
  BEADetailIndustryCodeName <- DetailIndFD[industry_range, ]
  colnames(BEADetailIndustryCodeName) <- c(paste0("BEA_",schema_year,"_Detail_Industry_Code"),
                                           paste0("BEA_",schema_year,"_Detail_Industry_Name"))
  rownames(BEADetailIndustryCodeName) <- NULL
  # Final Demand
  BEADetailFinalDemandCodeName <-  DetailIndFD[fd_range, ]
  colnames(BEADetailFinalDemandCodeName) <- c(paste0("BEA_",schema_year,"_Detail_FinalDemand_Code"),
                                              paste0("BEA_",schema_year,"_Detail_FinalDemand_Name"))
  rownames(BEADetailFinalDemandCodeName) <- NULL

  ### Summary ###
  # Load data
  FileName <- file.path(dir, "AllTableIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                              endsWith(files, "Summary.xlsx")])
  # FileName <- file.path(dir, "AllTablesSUP",
  #                       files[startsWith(files, "Use_Tables") &
  #                               endsWith(files, "Summary.xlsx")])
  
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  BEASummary <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(schema_year)))
  ## Commodity & Value Added
  SummaryCommVA <- BEASummary[!is.na(BEASummary[, 2]), c(1:2)][-c(1:2), ]
  commodity_range <- c(1:(which(SummaryCommVA[, 2] == "Total Intermediate") - 1))
  va_range <- c((length(commodity_range) + 2):(which(SummaryCommVA[, 2] == "Total Value Added") - 1))
  # Commodity
  BEASummaryCommodityCodeName <- SummaryCommVA[commodity_range, ]
  colnames(BEASummaryCommodityCodeName) <- c(paste0("BEA_",schema_year, "_Summary_Commodity_Code"),
                                             paste0("BEA_",schema_year, "_Summary_Commodity_Name"))
  rownames(BEASummaryCommodityCodeName) <- NULL
  # Value Added
  BEASummaryValueAddedCodeName <- SummaryCommVA[va_range, ]
  colnames(BEASummaryValueAddedCodeName) <- c(paste0("BEA_",schema_year, "_Summary_ValueAdded_Code"),
                                              paste0("BEA_",schema_year, "_Summary_ValueAdded_Name"))
  rownames(BEASummaryValueAddedCodeName) <- NULL
  ## Industry & Final Demand
  SummaryIndFD <- as.data.frame(t(BEASummary[!is.na(BEASummary[, 3]), ][1:2, -c(1:2)]))
  industry_range <- c(1:(which(SummaryIndFD[, 2] == "Total Intermediate") - 1))
  fd_range <- c((length(industry_range) + 2):(which(SummaryIndFD[, 2] == "Total Final Uses (GDP)") - 1))
  # Industry
  BEASummaryIndustryCodeName <- SummaryIndFD[industry_range, ]
  colnames(BEASummaryIndustryCodeName) <- c(paste0("BEA_",schema_year, "_Summary_Industry_Code"),
                                            paste0("BEA_",schema_year, "_Summary_Industry_Name"))
  rownames(BEASummaryIndustryCodeName) <- NULL
  # Final Demand
  BEASummaryFinalDemandCodeName <-  SummaryIndFD[fd_range, ]
  colnames(BEASummaryFinalDemandCodeName) <- c(paste0("BEA_",schema_year, "_Summary_FinalDemand_Code"),
                                               paste0("BEA_",schema_year, "_Summary_FinalDemand_Name"))
  rownames(BEASummaryFinalDemandCodeName) <- NULL

  ### Sector ###
  # Load data
  FileName <- file.path(dir, "AllTableIO",
                        files[startsWith(files, "IOUse_Before_Redefinitions_PRO") &
                              endsWith(files, "Sector.xlsx")])
  date_last_modified <- as.character(as.Date(file.mtime(FileName)))
  BEASector <- as.data.frame(readxl::read_excel(FileName, sheet = as.character(schema_year)))
  ## Commodity & Value Added
  SectorCommVA <- BEASector[!is.na(BEASector[, 2]), c(1:2)][-c(1:2), ]
  commodity_range <- c(1:(which(SectorCommVA[, 2] == "Total Intermediate") - 1))
  va_range <- c((length(commodity_range) + 2):(which(SectorCommVA[, 2] == "Total Value Added") - 1))
  # Commodity
  BEASectorCommodityCodeName <- SectorCommVA[commodity_range, ]
  colnames(BEASectorCommodityCodeName) <- c(paste0("BEA_",schema_year, "_Sector_Commodity_Code"),
                                            paste0("BEA_",schema_year, "_Sector_Commodity_Name"))
  rownames(BEASectorCommodityCodeName) <- NULL
  # Value Added
  BEASectorValueAddedCodeName <- SectorCommVA[va_range, ]
  colnames(BEASectorValueAddedCodeName) <- c(paste0("BEA_",schema_year, "_Sector_ValueAdded_Code"),
                                             paste0("BEA_",schema_year, "_Sector_ValueAdded_Name"))
  rownames(BEASectorValueAddedCodeName) <- NULL
  ## Industry & Final Demand
  SectorIndFD <- as.data.frame(t(BEASector[!is.na(BEASector[, 3]), ][1:2, -c(1:2)]))
  industry_range <- c(1:(which(SectorIndFD[, 2] == "Total Intermediate") - 1))
  fd_range <- c((length(industry_range) + 2):(which(SectorIndFD[, 2] == "Total Final Uses (GDP)") - 1))
  # Industry
  BEASectorIndustryCodeName <- SectorIndFD[industry_range, ]
  colnames(BEASectorIndustryCodeName) <- c(paste0("BEA_",schema_year, "_Sector_Industry_Code"),
                                           paste0("BEA_",schema_year, "_Sector_Industry_Name"))
  rownames(BEASectorIndustryCodeName) <- NULL
  # Final Demand
  BEASectorFinalDemandCodeName <-  SectorIndFD[fd_range, ]
  colnames(BEASectorFinalDemandCodeName) <- c(paste0("BEA_",schema_year, "_Sector_FinalDemand_Code"),
                                              paste0("BEA_",schema_year, "_Sector_FinalDemand_Name"))
  rownames(BEASectorFinalDemandCodeName) <- NULL

  ### Put the data.frames in a list
  BEACodeNameList <- list("Detail_IndustryCodeName"     = BEADetailIndustryCodeName,
                          "Detail_CommodityCodeName"    = BEADetailCommodityCodeName,
                          "Detail_ValueAddedCodeName"   = BEADetailValueAddedCodeName,
                          "Detail_FinalDemandCodeName"  = BEADetailFinalDemandCodeName,
                          "Summary_IndustryCodeName"    = BEASummaryIndustryCodeName,
                          "Summary_CommodityCodeName"   = BEASummaryCommodityCodeName,
                          "Summary_ValueAddedCodeName"  = BEASummaryValueAddedCodeName,
                          "Summary_FinalDemandCodeName" = BEASummaryFinalDemandCodeName,
                          "Sector_IndustryCodeName"     = BEASectorIndustryCodeName,
                          "Sector_CommodityCodeName"    = BEASectorCommodityCodeName,
                          "Sector_ValueAddedCodeName"   = BEASectorValueAddedCodeName,
                          "Sector_FinalDemandCodeName"  = BEASectorFinalDemandCodeName)
  BEACodeNameList <- lapply(BEACodeNameList, cleanSectorNames)
  BEACodeNameList <- lapply(BEACodeNameList, cleanSectorCodes)
  ### Save and Document data
  ls <- list("url" = url,
             "date_last_modified" = date_last_modified,
             "date_accessed" = date_accessed)
  for (data_name in names(BEACodeNameList)) {
    writeFile(df = BEACodeNameList[[data_name]], year = schema_year,
              name = paste0(data_name, "_", schema_year), ls = ls,
              schema_year = NULL)
  }
}

# Get Detail Margins (Before Redefinition) table from BEA static URL
getBEADetailMarginsBeforeRedef <- function(year, schema_year = NULL) {
  if(is.null(schema_year)) {
    schema_year <- year
  }
  # Download data
  file <- paste0("Margins_Before_Redefinitions_", schema_year, "_DET.xlsx")
  url <- file.path(url_ls["margins"], file)
  FileName <- file.path(dir, file)
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
  ls <- list("url" = url,
             "date_accessed" = as.character(as.Date(file.mtime(FileName))),
             "date_last_modified" = "2024-05-23") # page last modified 
  writeFile(df = Margins, year = year,
            name = paste0("Detail_Margins_", year, "_BeforeRedef"), ls = ls,
            schema_year = schema_year)
}


# Get BEA Detail Supply table from static Excel
getBEADetailSupply <- function(year, schema_year = NULL) {
  if(is.null(schema_year)) {
    schema_year <- year
  }
  ls <- getBEASupplyUseTables()
  FileName <- file.path(dir, "AllTablesSUP",
                        ls[["files"]][startsWith(ls[["files"]], "Supply") &
                                      endsWith(ls[["files"]], "DET.xlsx")])
  ls["date_last_modified"] <- as.character(as.Date(file.mtime(FileName)))
  DetailSupply <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
  DetailSupply <- processDetailMatrix(DetailSupply)
  writeFile(df = DetailSupply, year = year,
            name = paste0("Detail_Supply_", year), ls = ls,
            schema_year = schema_year)
}


# Get BEA Detail Use (under the Supply-Use framework schema) table from static Excel
getBEADetailUseSUT <- function(year, schema_year = NULL) {
  if(is.null(schema_year)) {
    schema_year <- year
  }
  ls <- getBEASupplyUseTables()
  FileName <- file.path(dir, "AllTablesSUP",
                        ls[["files"]][startsWith(ls[["files"]], "Use") &
                                        endsWith(ls[["files"]], "DET.xlsx")])
  ls["date_last_modified"] = as.character(as.Date(file.mtime(FileName)))
  DetailUse <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(year)))
  DetailUse <- processDetailMatrix(DetailUse)
  writeFile(df = DetailUse, year = year,
            name = paste0("Detail_Use_SUT_", year), ls = ls,
            schema_year = schema_year)
}


# Get BEA Summary Supply table from static Excel
getBEASummarySupply <- function(year) {
  ls <- getBEASupplyUseTables()
  # Prepare file name
  file <- ls[["files"]][startsWith(ls[["files"]], "Supply") & 
                          endsWith(ls[["files"]], "Summary.xlsx")]
  FileName <- file.path(dir, "AllTablesSUP", file)
  # Load data
  for (y in start_year:end_year) {
    SummarySupply <- as.data.frame(readxl::read_excel(FileName,
                                                      sheet = as.character(y)))
    SummarySupply <- processSummaryMatrix(SummarySupply)
    writeFile(df = SummarySupply, year = y,
              name = paste0("Summary_Supply_", y), ls = ls,
              schema_year = year)
  }
}

# Get BEA Summary Use under the Supply-Use framework from static Excel
getBEASummaryUseSUT <- function(year) {
  ls <- getBEASupplyUseTables()
  file <- ls[["files"]][startsWith(ls[["files"]], "Use") &
                          endsWith(ls[["files"]], "Summary.xlsx")]
  FileName <- file.path(dir, "AllTablesSUP", file)
  # Load data
  for (y in start_year:end_year) {
    SummaryUse <- as.data.frame(readxl::read_excel(FileName,
                                                   sheet = as.character(y)))
    SummaryUse <- processSummaryMatrix(SummaryUse)
    writeFile(df = SummaryUse, year = y,
              name = paste0("Summary_Use_SUT_", y), ls = ls,
              schema_year = year)
  }
}


# # Get BEA Sector Supply table from static Excel
# getBEASectorSupply <- function() {
#   # Download data
#   url <- getBEASupplyUseTables()[["url"]]
#   date_accessed <- getBEASupplyUseTables()[["date_accessed"]]
#   files <- getBEASupplyUseTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "Supply") & endsWith(files, "SEC.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesSUP", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Find latest data year
#   file_split <- unlist(stringr::str_split(file, pattern = "_"))
#   year_range <- file_split[length(file_split) - 1]
#   end_year <- sub(".*-", "", year_range)
#   # Load data
#   for (year in 2010:end_year) {
#     SectorSupply <- as.data.frame(readxl::read_excel(FileName,
#                                                      sheet = as.character(year)))
#     # Trim table, assign column names
#     SectorSupply <- SectorSupply[!is.na(SectorSupply[, 2]), ]
#     colnames(SectorSupply) <- SectorSupply[1, ]
#     colname_check <- is.na(colnames(SectorSupply))
#     colnames(SectorSupply)[colname_check] <- SectorSupply[2, colname_check]
#     # Assign T017 to Total industry supply if not provided
#     if (is.na(SectorSupply[SectorSupply$`Commodities/Industries` == "Total industry supply", 1])) {
#       SectorSupply[SectorSupply$`Commodities/Industries` == "Total industry supply", 1] <- "T017"
#     }
#     # Fill NA in code column with corresponding name
#     SectorSupply[is.na(SectorSupply[, 1]), 1] <- SectorSupply[is.na(SectorSupply[, 1]), 2]
#     # Convert all values to numeric, assign row names
#     SectorSupply <- as.data.frame(lapply(SectorSupply[-c(1:2), -c(1:2)], as.numeric),
#                                   check.names = FALSE,
#                                   row.names = SectorSupply[-c(1:2), 1])
#     # Replace NA with zero
#     SectorSupply[is.na(SectorSupply)] <- 0
#     # Write data to .rda
#     writeDatatoRDA(data = SectorSupply,
#                    data_name = paste0("Sector_Supply_", year))
#     # Write metadata to JSON
#     writeMetadatatoJSON(package = "useeior",
#                         name = paste0("Sector_Supply_", year),
#                         year = year,
#                         source = "US Bureau of Economic Analysis",
#                         url = url,
#                         date_last_modified = date_last_modified,
#                         date_accessed = date_accessed)
#   }
# }
# 
# 
# # Get BEA Sector Use (under the Supply-Use framework, 2012 schema) table from static Excel
# getBEASectorUseSUT2012Schema <- function() {
#   # Download data
#   url <- getBEASupplyUseTables()[["url"]]
#   date_accessed <- getBEASupplyUseTables()[["date_accessed"]]
#   files <- getBEASupplyUseTables()[["files"]]
#   # Prepare file name
#   file <- files[startsWith(files, "Use") & endsWith(files, "SECT.xlsx")]
#   FileName <- file.path("inst/extdata/AllTablesSUP", file)
#   date_last_modified <- as.character(as.Date(file.mtime(FileName)))
#   # Find latest data year
#   file_split <- unlist(stringr::str_split(file, pattern = "_"))
#   year_range <- file_split[length(file_split) - 1]
#   end_year <- sub(".*-", "", year_range)
#   # Load data
#   for (year in 2010:end_year) {
#     SectorUse <- as.data.frame(readxl::read_excel(FileName,
#                                                   sheet = as.character(year)))
#     # Trim table, assign column names
#     SectorUse <- SectorUse[!is.na(SectorUse[, 2]), ]
#     colnames(SectorUse) <- SectorUse[1, ]
#     colname_check <- is.na(colnames(SectorUse))
#     colnames(SectorUse)[colname_check] <- SectorUse[2, colname_check]
#     # Fill NA in code column with corresponding name
#     SectorUse[is.na(SectorUse[, 1]), 1] <- SectorUse[is.na(SectorUse[, 1]), 2]
#     # Convert all values to numeric, assign row names
#     SectorUse <- as.data.frame(lapply(SectorUse[-c(1:2), -c(1:2)], as.numeric),
#                                check.names = FALSE,
#                                row.names = SectorUse[-c(1:2), 1])
#     # Replace NA with zero
#     SectorUse[is.na(SectorUse)] <- 0
#     # Write data to .rda
#     writeDatatoRDA(data = SectorUse,
#                    data_name = paste0("Sector_Use_SUT_", year))
#     # Write metadata to JSON
#     writeMetadatatoJSON(package = "useeior",
#                         name = paste0("Sector_Use_SUT_", year),
#                         year = year,
#                         source = "US Bureau of Economic Analysis",
#                         url = url,
#                         date_last_modified = date_last_modified,
#                         date_accessed = date_accessed)
#   }
# }

