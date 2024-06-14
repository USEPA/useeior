source("data-raw/BEAData.R")
source("R/UtilityFunctions.R")

schema_year <- 2017


# Download, save and document BEA Detail Make (Before Redef)
getBEADetailMakeBeforeRedef(year=2012, schema_year)
getBEADetailMakeBeforeRedef(year=2017, schema_year)

# Download, save and document BEA Detail Use (PRO, Before Redef)
getBEADetailUsePROBeforeRedef(year=2012, schema_year)
getBEADetailUsePROBeforeRedef(year=2017, schema_year)

# Download, save and document BEA Detail Use (PUR, Before Redef)
getBEADetailUsePURBeforeRedef(schema_year)

# Download, save and document BEA Detail Make (After Redef)
getBEADetailMakeAfterRedef(schema_year)

# Download, save and document BEA Detail Use (PRO, After Redef)
getBEADetailUsePROAfterRedef(schema_year)

# Download, save and document BEA Detail Use (PUR, After Redef)
getBEADetailUsePURAfterRedef(schema_year)

# Download, save and document BEA Summary Use (PUR, Before Redef)
getBEASummaryUsePURBeforeRedef(schema_year)

# Download, save and document BEA Detail Import matrix
getBEADetailImportBeforeRedef(year=2012, schema_year)
getBEADetailImportBeforeRedef(year=2017, schema_year)

# Download, save and document BEA Detail, Summary, and Sector Code and Name
getBEACodeName(schema_year)

# Download, save and document BEA Detail Margins table
getBEADetailMarginsBeforeRedef(year=2012, schema_year)
getBEADetailMarginsBeforeRedef(year=2017, schema_year)

## Supply and Use Tables
# Download, save and document BEA Detail Supply
getBEADetailSupply(year=2012, schema_year)
getBEADetailSupply(year=2017, schema_year)

# Download, save and document BEA Detail Use
getBEADetailUseSUT(year=2012, schema_year)
getBEADetailUseSUT(year=2017, schema_year)

