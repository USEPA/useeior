source("data-raw/BEAData.R")

schema_year <- 2012

# Download, save and document 2012 BEA Detail Make (Before Redef, 2012 schema)
getBEADetailMakeBeforeRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Detail Use (PRO, Before Redef, 2012 schema)
getBEADetailUsePROBeforeRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Detail Use (PUR, Before Redef, 2012 schema)
getBEADetailUsePURBeforeRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Detail Make (After Redef, 2012 schema)
getBEADetailMakeAfterRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Detail Use (PRO, After Redef, 2012 schema)
getBEADetailUsePROAfterRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Detail Use (PUR, After Redef, 2012 schema)
getBEADetailUsePURAfterRedef2012Schema(schema_year)


# Download, save and document 2012 BEA Summary Use (PUR, Before Redef, 2012 schema)
getBEASummaryUsePURBeforeRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Sector Use (PUR, Before Redef, 2012 schema)
getBEASectorUsePURBeforeRedef2012Schema(schema_year)

# Download, save and document 2012 BEA Detail Import matrix
getBEADetailImportBeforeRedef2012Schema(schema_year)

# Download, save and document BEA Detail, Summary, and Sector Code and Name (2012 schema)
getBEACodeName2012Schema()

# Download, save and document 2012 BEA Detail Margins table
getBEADetailMarginsBeforeRedef2012Schema(schema_year)