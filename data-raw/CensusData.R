# Download 2-to-6 digit 2012 NAICS code table from Census.gov
get2to6Digit2012NAICS <- function () {
  # Create the placeholder file
  file <- "inst/extdata/2-digit_2012_Codes.xls"
  # Download 2-to-6 digit 2012 NAICS code table
  if(!file.exists(file)) {
    utils::download.file("https://www.census.gov/naics/2012NAICS/2-digit_2012_Codes.xls", "2-digit_2012_Codes.xls", mode = "wb")
  }
}

get2012to2007NAICSConcordance <- function () {
  # Create the placeholder file
  file <- "2012_to_2007_NAICS.xls"
  # Download 2-to-6 digit 2012 NAICS code table
  if(!file.exists(file)) {
    utils::download.file("https://www.census.gov/naics/concordances/2012_to_2007_NAICS.xls", "2012_to_2007_NAICS.xls", mode = "wb")
  }
}
