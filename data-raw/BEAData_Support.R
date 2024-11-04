source("data-raw/BEAData.R")
source("R/UtilityFunctions.R")

schema_year <- 2017
# 2012 - 2023 tables


## Annual Summary Make and Use
# Download, save and document BEA Summary Make (Before Redef)
getBEASummaryMakeBeforeRedef(schema_year)

# Download, save and document BEA Summary Use (PRO, Before Redef)
getBEASummaryUsePROBeforeRedef(schema_year)

# Download, save and document BEA Summary Make (After Redef)
getBEASummaryMakeAfterRedef(schema_year)

# Download, save and document BEA Summary Use (PRO, After Redef)
getBEASummaryUsePROAfterRedef(schema_year)

## Supporting Data
# Download, save and document BEA Summary Import matrix
getBEASummaryImportBeforeRedef(schema_year)

# Download, save and document BEA Detail, Summary, and Sector Gross Output tables
mapBEAGrossOutputtoIOIndustry(schema_year)

# Download, save and document BEA Detail, Summary, and Sector CPI tables
mapBEACPItoIOIndustry(schema_year)

# Download, save and document BEA Summary and Sector Value Added tables
mapBEAValueAddedtoIOIndustry(schema_year)

## Supply and Use Tables
# Download, save and document BEA Summary Supply
getBEASummarySupply(schema_year)

# Download, save and document BEA Summary Use
getBEASummaryUseSUT(schema_year)
