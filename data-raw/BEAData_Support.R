source("data-raw/BEAData.R")


## Make - Use NOT YET AVAILABLE as of October 12, 2023

## Annual Summary Make and Use
# Download, save and document 2010-2020 BEA Summary Make (Before Redef, 2012 schema)
#getBEASummaryMakeBeforeRedef2012Schema()

# Download, save and document 2010-2020 BEA Summary Use (PRO, Before Redef, 2012 schema)
#getBEASummaryUsePROBeforeRedef2012Schema()

# Download, save and document 2010-2020 BEA Summary Make (After Redef, 2012 schema)
#getBEASummaryMakeAfterRedef2012Schema()

# Download, save and document 2010-2020 BEA Summary Use (PRO, After Redef, 2012 schema)
#getBEASummaryUsePROAfterRedef2012Schema()


## Annual Sector Make and Use
# Download, save and document 2010-2020 BEA Sector Make (Before Redef, 2012 schema)
#getBEASectorMakeBeforeRedef2012Schema()

# Download, save and document 2010-2020 BEA Sector Use (PRO, Before Redef, 2012 schema)
#getBEASectorUsePROBeforeRedef2012Schema()

# Download, save and document 2010-2020 BEA Sector Make (After Redef, 2012 schema)
#getBEASectorMakeAfterRedef2012Schema()

# Download, save and document 2010-2018 BEA Sector Use (PRO, After Redef, 2012 schema)
#getBEASectorUsePROAfterRedef2012Schema()


## Supporting Data
# Download, save and document 2010-2020 BEA Summary Import matrix
#getBEASummaryImportBeforeRedef2012Schema()

# Download, save and document BEA Detail, Summary, and Sector Gross Output tables
mapBEAGrossOutputtoIOIndustry2012Schema()

# Download, save and document BEA Detail, Summary, and Sector CPI tables since 2002
mapBEACPItoIOIndustry2012Schema()

# Download, save and document BEA Summary and Sector Value Added tables since 2002
mapBEAValueAddedtoIOIndustry2012Schema()

## Supply and Use Tables
# Download, save and document 2010-2020 BEA Summary Supply (2012 schema)
getBEASummarySupply2012Schema()

# Download, save and document 2010-2020 BEA Summary Use (2012 schema)
getBEASummaryUseSUT()

# Download, save and document 2010-2020 BEA Sector Supply (2012 schema)
getBEASectorSupply()

# Download, save and document 2010-2020 BEA Sector Supply (2012 schema)
getBEASectorUseSUT2012Schema()
