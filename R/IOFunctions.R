#' Adjust Industry output based on CPI.
#'
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#'
#' @return A dataframe contains adjusted Industry output with row names being BEA sector code.
getAdjustedOutput <- function (outputyear, referenceyear, location_acronym, IsRoUS, model) {
  # Load Industry Gross Output
  if (model$specs$PrimaryRegionAcronym == "US") {
    Output <- cbind.data.frame(rownames(model$GDP$BEAGrossOutputIO), model$GDP$BEAGrossOutputIO[, as.character(outputyear)])
  } else {
    if(model$specs$ModelSource=="WinDC") {
      if(IsRoUS == TRUE) {
        Output <- model$IndustryOutput[model$IndustryOutput$Location=="RoUS", c("SectorCode", as.character(outputyear)), drop = FALSE]
        rownames(Output) <- Output$SectorCode
      } else {
        Output <- model$IndustryOutput[model$IndustryOutput$Location==location_acronym, c("SectorCode", as.character(outputyear)), drop = FALSE]
      }
    } else {
      # This is if model is using calculated state demand
      Output <- getStateIndustryOutput(model$specs$PrimaryRegionAcronym, outputyear)
      row.names(Output) <- Output[,"BEACode"]
      if(IsRoUS == TRUE) {
        Output$RoUS <- Output$US - Output$SoI
        Output <- Output[, c("BEACode", "RoUS")]
      } else {
        Output <- Output[, c("BEACode", "SoI")]
      }
    }
  }
  colnames(Output) <- c("SectorCode", "Output")
  # Adjust Industry output based on CPI
  model$GDP$BEACPIIO$ReferenceYeartoOutputYearRatio <- model$GDP$BEACPIIO[, as.character(referenceyear)]/model$GDP$BEACPIIO[, as.character(outputyear)]
  AdjustedOutput <- merge(Output, model$GDP$BEACPIIO[, "ReferenceYeartoOutputYearRatio", drop = FALSE], by.x = "SectorCode", by.y = 0)
  AdjustedOutput[, paste(outputyear, "IndustryOutput", sep = "")] <- AdjustedOutput$Output * AdjustedOutput$ReferenceYeartoOutputYearRatio
  # Assign rownames and keep wanted column
  rownames(AdjustedOutput) <- AdjustedOutput$SectorCode
  AdjustedOutput <- AdjustedOutput[, paste(outputyear, "IndustryOutput", sep = ""), drop = FALSE]
  return(AdjustedOutput)
}

#' Derive IO coefficients
#' @param IO_transactions_df IO transactions of the model in dataframe format.
#' @param IO_output_df Output of the model in dataframe format.
#' @return A matrix.
normalizeIOTransactions <- function (IO_transactions_df, IO_output_df) {
  Z <- as.matrix(IO_transactions_df)
  x <- unname(unlist(IO_output_df))
  x_hat <- diag(x, length(x), length(x))
  A <- Z %*% solve(x_hat)
  dimnames(A) <- dimnames(Z)
  return(A)
}

#' Generate Direct Requirements matrix from Use table.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param domestic A logical parameter indicating whether to calculate DR or Domestic DR.
#' @return Direct Requirements matrix of the model.
generateDirectRequirementsfromUse <- function (model, domestic) {
  # Generate direct requirments matrix (commodity x industry) from Use, see Miller and Blair section 5.1.1
  if (domestic==TRUE) {
    B <- normalizeIOTransactions(model$DomesticUseTransactions, model$BEA$MakeIndustryOutput) # B = U %*% solve(x_hat)
  } else {
    B <- normalizeIOTransactions(model$UseTransactions, model$BEA$MakeIndustryOutput) # B = U %*% solve(x_hat)
  }
  return(B)
}

#' Generate Market Shares matrix from Make table.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return Market Shares matrix of the model.
generateMarketSharesfromMake <- function(model) {
  # Generate market shares matrix (industry x commodity) from Make, see Miller and Blair section 5.3.1
  D <- normalizeIOTransactions(model$MakeTransactions, model$BEA$UseCommodityOutput) # D = V %*% solve(q_hat)
  # Put in code here for adjusting marketshares to remove scrap
  return(D)
}

#' Generate Commodity Mix matrix.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return Commodity Mix matrix of the model.
generateCommodityMixMatrix <- function (model) {
  # Generate commodity mix matrix (commodity x industry), see Miller and Blair section 5.3.2
  C <- normalizeIOTransactions(t(model$MakeTransactions), model$BEA$MakeIndustryOutput) # C = V' %*% solve(x_hat)
  # Validation: check if column sums equal to 1
  industryoutputfractions <- colSums(C)
  for (s in industryoutputfractions) {
    if (abs(1-s)>0.01) {
      print("Error in commoditymix")
    }
  }
  return(C)
}

#' Generate Commodity output by transforming Industry output using Commodity Mix matrix.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains adjusted Commodity output.
generateCommodityOutputforYear <- function(location_acronym, IsRoUS, model) {
  # Generate a commodity x industry commodity mix matrix, see Miller and Blair section 5.3.2
  CommodityMix <- generateCommodityMixMatrix(model)
  # Generate adjusted industry output by location
  IndustryOutputVector <- as.matrix(model$BEA$MakeIndustryOutput)
  # Use CommodityMix to transform IndustryOutput to CommodityOutput
  CommodityOutput <- as.data.frame(CommodityMix %*% IndustryOutputVector)
  colnames(CommodityOutput) <- as.character(model$specs$IOYear)
  return(CommodityOutput)
}

#' Generate Commodity CPI by transforming Industry CPI using Commodity Mix matrix.
#' @param year Year of Industry CPI.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains adjusted Commodity CPI.
generateCommodityCPIforYear <- function(year, model) {
  # Generate a commodity x industry commodity mix matrix, see Miller and Blair section 5.3.2
  CommodityMix <- generateCommodityMixMatrix(model)
  # Generate adjusted industry CPI by location
  IndustryCPIVector <- as.matrix(model$GDP$BEACPIIO[, as.character(year)])
  # Use CommodityMix to transform IndustryCPI to CommodityCPI
  CommodityCPI <- as.data.frame(CommodityMix %*% IndustryCPIVector)
  colnames(CommodityCPI) <- as.character(year)

  return(CommodityCPI)
}

#' Generate non-scrap ratios
#' @return A datframe with rows being model industries and a column for "non_scrap_ratios" for that industry.
generateNonScrapRatios <- function() {
  # Merge scrap from model Make transactions and Industry output
  V_scrap <- model$MakeTransactions[, ModelScrapCode, drop = FALSE]
  V_scrap_total <- merge(V_scrap, model$BEA$MakeIndustryOutput, by = 0)
  IndustryTotalCode <- colnames(model$BEA$MakeIndustryOutput)
  V_scrap_total[,"nonscrap_ratio"] <- (V_scrap_total[,IndustryTotalCode]-V_scrap_total[, ModelScrapCode])/V_scrap_total[, IndustryTotalCode]
  row.names(V_scrap_total) <- V_scrap_total[,"Row.names"]
  non_scrap_ratios <- V_scrap_total[,"nonscrap_ratio", drop=FALSE]
  return(non_scrap_ratios)
}

#' Transform Direct Requirements matrix with Market Shares matrix, works for both commodity-by-commodity and industry-by-industry model types.
#' @param B Marginal impact per unit of the environmental flows.
#' @param D Market Shares matrix.
#' @return Direct Requirements matrix.
transformDirectRequirementswithMarketShares <- function (B, D) {
  # Only generate result if the column names of the direct requirements table match the row names of the market shares matrix
  if (all(colnames(B) == rownames(D)) && all(colnames(D) == rownames(B))) {

  } else {
    logging::logerror("Error: column names of the direct requirements do not match the rows of the market shares matrix")
  }
  if (model$specs$CommoditybyIndustryType == "Commodity") {
    # commodity model DR_coeffs = dr %*% ms (CxI x IxC) = CxC
    A <- B %*% D
    dimnames(A) <- c(dimnames(B)[1], dimnames(D)[2])
  } else if (model$specs$CommoditybyIndustryType == "Industry") {
    # industry model DR_coeffs = ms %*% dr (IxC x CxI) = IxI
    A <- D %*% B
    dimnames(A) <- c(dimnames(D)[1], dimnames(B)[2])
  } else {
    logging::logerror("commoditybyIndustryType not specified for model or incorrectly specified")
  }
  return(A)
}

#' Transform Final Demand df with Market Shares matrix.
#' @param Fdf Final Demand dataframe.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return Final Demand matrix.
transformFinalDemandwithMarketShares <- function (Fdf, model) {
  D <- generateMarketSharesfromMake(model)
  # See Miller and Blair section 5.3.7 (pg 197)
  Fmatrix <- D %*% as.matrix(Fdf)
  return(Fmatrix)
}

#' Calculate Leontief inverse from direct requirements matrix.
#' @param A Direct Requirements matrix.
#' @return Leontief inverse.
calculateLeontiefInverse <- function(A) {
  I <- diag(nrow(A))
  L <- solve(I-A)
  return(L)
}

#' Generate output-based allocation factors for a dataframe of BEA codes and a grouping variable.
#' @param codeswithgroups A dataframe contains two columns: "Code" and "Group".
#' @return A dataframe contains codes, groups, and allocation factors.
generateOutputBasedAllocationFactorsByGroup <- function(codeswithgroups) {
  # Get output in desired year and that same currency year
  output <- getAdjustedOutput(year, year, location_acronym, model)
  # Merge with output
  codeswithgroupswithoutput <- merge(codeswithgroups, output, by.x = "Code", by.y = 0)
  # Aggregate based on group to get sums of output by group
  outputbygroup <- stats::aggregate(codeswithgroupswithoutput$output, by = list(codeswithgroupswithoutput$Group), sum)
  colnames(outputbygroup) <- c("Group", "Groupoutput")
  # Merge in output totals by group
  codeswithgroupswithoutputandgroupoutput <- merge(codeswithgroupswithoutput,outputbygroup, by = "Group")
  codeswithgroupswithoutputandgroupoutput$allocationfactor <- codeswithgroupswithoutputandgroupoutput$output/codeswithgroupswithoutputandgroupoutput$Groupoutput
  codeswithgroupsandallocation <- codeswithgroupswithoutputandgroupoutput[,c("Code","Group","allocationfactor")]
  return(codeswithgroupsandallocation)
}

#No need for use with refactor
# getUseDetailwithCommoditiesOnly = function() {
#   UseDetail07 = read.table(paste(BEApath,"2007Schema/Detail_Use_2007_PRO_BeforeRedef.csv",sep=""),header=T,sep = ",",row.names=1,check.names=F)
#   UseDetail07[is.na(UseDetail07)]=0
#   total_commodties = 389
#   return(UseDetail07[1:389,])
# }

#Note planned for use with refactor
#returns a a df with column T007 from the detail use table
# getImportDetailwithUseCommodities = function() {
#   ImportDetail07=read.table(paste(BEApath,"2007Schema/Detail_ImportMatrix_2007_BeforeRedef.csv",sep=""),sep = ",",header=T,row.names=1,check.names=F)
#   ImportDetail07[is.na(ImportDetail07)]=0
#   # Drop 331314, S00101, S00201, and S00202 to align with the UseDetail07
#   ImportDetail07 = ImportDetail07[!rownames(ImportDetail07) %in% c("331314","S00101","S00201","S00202"),]
#
#   # Check if rows in UseDetail and ImportDetail line up
#   if (!all.equal(rownames(UseDetail07),rownames(ImportDetail07))) {
#     return ("Error: commodities in import do  not match Use commodity order")
#   }  else {# should return all true
#     return (ImportDetail07)
#   }
# }

#' Determine proportion of Imports in Industr use.
#' @return A dataframe contains "imports_used_by_industry", "total_used_by_industry", and "import_ratio_of_industry_use".
getIndustryUseofImportedCommodities <- function() {
  ImportDetail <- getImportDetailwithUseCommodities()
  TotalIndustryUseofImportedCommoditiesfromImportDetail <- ImportDetail[, "T001"]
  UseDetail <- getUseDetailwithCommoditiesOnly()
  TotalIndustryUseofCommoditiesfromUseDetail <- UseDetail[, "T001"]
  proportion_imports_in_industryuse <- cbind(TotalIndustryUseofImportedCommoditiesfromImportDetail,TotalIndustryUseofCommoditiesfromUseDetail)
  proportion_imports_in_industryuse[, "import_ratio"] <- proportion_imports_in_industryuse[, 1]/proportion_imports_in_industryuse[, 2]
  colnames(proportion_imports_in_industryuse) <- c("imports_used_by_industry", "total_used_by_industry", "import_ratio_of_industry_use")
  return (proportion_imports_in_industryuse)
}

#' Adjust multi-year USEEIO gross output by model-specified currency year.
#' @return A dataframe contains adjusted multi-year USEEIO gross output.
# adjustUSEEIOGrossOutputbyCPIYear <- function () {
#   GrossOutput <- model$GDP$BEAGrossOutputIO
#   for (year in colnames(GrossOutput)) {
#     GrossOutput[, year] <- GrossOutput[, year]*(model$GDP$BEACPIIO[, as.character(model$specs$ReferenceCurrencyYear)]/model$GDP$BEACPIIO[, year])
#   }
#   return(GrossOutput)
# }

#' Adjust gross output from GDP industries to IO industries (2012 schema) at Detail, Summary, and Sector IO levels.
#' @return A list contains IO-based gross output at Detail, Summary, and Sector IO levels.
adjustBEAGrossOutouttoIOIndustry2012Schema <- function () {
  # Detail
  DetailGrossOutput <- getBEADetailGrossOutput2012Schema()
  # Determine year range
  year_range <- colnames(DetailGrossOutput)[2:ncol(DetailGrossOutput)]
  # Attach BEA Detail industry code
  DetailGDPIndustrytoIO <- utils::read.table(system.file("extdata", "Crosswalk_DetailGDPIndustrytoIO2012Schema.csv", package = "useeior"),
                                             sep = ",", header = TRUE)
  DetailGrossOutputIO <- merge(DetailGDPIndustrytoIO, DetailGrossOutput, by = "Gross_Output_Detail_Industry", all.y = TRUE)
  # Convert values to numeric format
  DetailGrossOutputIO[, year_range] <- as.data.frame(apply(DetailGrossOutputIO[, year_range], 2, as.numeric))
  # Aggregate by BEA Detail industry code
  DetailGrossOutputIO <- stats::aggregate(DetailGrossOutputIO[, year_range], by = list(DetailGrossOutputIO$BEA_2012_Detail_Code), sum)
  # Assign rownames as sector code
  rownames(DetailGrossOutputIO) <- DetailGrossOutputIO[, 1]
  DetailGrossOutputIO[, 1] <- NULL

  # Summary
  SummaryGrossOutput <- getBEASummaryGrossOutput2012Schema()
  # Attach IO industry
  SummaryGDPIndustrytoIO <- utils::read.table(system.file("extdata", "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv", package = "useeior"),
                                              sep = ",", header = TRUE)
  SummaryGrossOutputIO <- cbind(SummaryGDPIndustrytoIO, SummaryGrossOutput)
  # Keep Summary rows
  SummaryGrossOutputIO <- SummaryGrossOutputIO[!SummaryGrossOutputIO$BEA_2012_Summary_Code == "", c("BEA_2012_Summary_Code", year_range)]
  # Assign rownames as sector code
  rownames(SummaryGrossOutputIO) <- SummaryGrossOutputIO[, 1]
  SummaryGrossOutputIO[, 1] <- NULL
  # Convert values to numeric format
  SummaryGrossOutputIO[] <- as.data.frame(apply(SummaryGrossOutputIO, 2, as.numeric))

  # Sector
  SectorGrossOutput <- getBEASectorGrossOutput2012Schema()
  # Attach IO industry
  SectorGDPIndustrytoIO <- utils::read.table(system.file("extdata", "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv", package = "useeior"),
                                             sep = ",", header = TRUE)
  SectorGrossOutputIO <- cbind(SectorGDPIndustrytoIO, SectorGrossOutput)
  # Keep Summary rows
  SectorGrossOutputIO <- SectorGrossOutputIO[!SectorGrossOutputIO$BEA_2012_Sector_Code == "", c("BEA_2012_Sector_Code", year_range)]
  # Assign rownames as sector code
  rownames(SectorGrossOutputIO) <- SectorGrossOutputIO[, 1]
  SectorGrossOutputIO[, 1] <- NULL
  # Convert values to numeric format
  SectorGrossOutputIO[] <- as.data.frame(apply(SectorGrossOutputIO, 2, as.numeric))

  # Put GrossOutputIO tables in the GrossOutputIOList
  GrossOutputIOList <- list(DetailGrossOutputIO, SummaryGrossOutputIO, SectorGrossOutputIO)
  names(GrossOutputIOList) <- c("Detail", "Summary", "Sector")
  return(GrossOutputIOList)
}



#' Adjust CPI from GDP industries to IO industries (2012 schema) at Detail, Summary, and Sector IO levels.
#' @return A list contains IO-based CPI at Detail, Summary, and Sector IO levels.
adjustBEACPItoIOIndustry2012Schema <- function () {
  # Detail
  DetailCPI <- getBEADetailCPI2012Schema()
  # Determine year range
  year_range <- colnames(DetailCPI)[2:ncol(DetailCPI)]
  # Attach BEA Detail industry code
  DetailGDPIndustrytoIO <- utils::read.table(system.file("extdata", "Crosswalk_DetailGDPIndustrytoIO2012Schema.csv", package = "useeior"),
                                             sep = ",", header = TRUE)
  DetailCPIIO <- merge(DetailGDPIndustrytoIO, DetailCPI, by = "Gross_Output_Detail_Industry", all.y = TRUE)
  # Convert values to numeric format
  DetailCPIIO[, year_range] <- as.data.frame(apply(DetailCPIIO[, year_range], 2, as.numeric))
  # Adjust (weighted average) CPI based on DetailGrossOutput
  # DetailGrossOutput
  DetailGrossOutput <- getBEADetailGrossOutput2012Schema()
  DetailGrossOutput[, year_range] <- as.data.frame(apply(DetailGrossOutput[, year_range], 2, as.numeric))
  # Merge CPI with GrossOutput
  DetailCPIIO <- merge(DetailCPIIO, DetailGrossOutput, by = "Gross_Output_Detail_Industry")
  # Calculate weighted average of CPI
  for (code in unique(DetailCPIIO[, "BEA_2012_Detail_Code"])) {
    for (year in year_range) {
      DetailCPIIO[DetailCPIIO$BEA_2012_Detail_Code == code, year] <- stats::weighted.mean(DetailCPIIO[DetailCPIIO$BEA_2012_Detail_Code == code, paste(year, "x", sep = ".")],
                                                                                         DetailCPIIO[DetailCPIIO$BEA_2012_Detail_Code == code, paste(year, "y", sep = ".")])
    }
  }
  # Aggregate CPI by BEA_2012_Detail_Code
  DetailCPIIO <- stats::aggregate(DetailCPIIO[, year_range], by = list(DetailCPIIO$BEA_2012_Detail_Code), mean)
  # Assign rownames as sector code
  rownames(DetailCPIIO) <- DetailCPIIO[, 1]
  DetailCPIIO[, 1] <- NULL

  # Summary
  SummaryCPI <- getBEASummaryCPI2012Schema()
  # Attach BEA Detail industry code
  SummaryGDPIndustrytoIO <- utils::read.table(system.file("extdata", "Crosswalk_SummaryGDPIndustrytoIO2012Schema.csv", package = "useeior"),
                                              sep = ",", header = TRUE)
  SummaryCPIIO <- cbind(SummaryGDPIndustrytoIO, SummaryCPI)
  # Keep Summary rows
  SummaryCPIIO <- SummaryCPIIO[!SummaryCPIIO$BEA_2012_Summary_Code == "", c("BEA_2012_Summary_Code", year_range)]
  # Assign rownames as sector code
  rownames(SummaryCPIIO) <- SummaryCPIIO[, 1]
  SummaryCPIIO[, 1] <- NULL
  # Convert values to numeric format
  SummaryCPIIO[] <- as.data.frame(apply(SummaryCPIIO, 2, as.numeric))

  # Sector
  SectorCPI <- getBEASectorCPI2012Schema()
  # Attach BEA Detail industry code
  SectorGDPIndustrytoIO <- utils::read.table(system.file("extdata", "Crosswalk_SectorGDPIndustrytoIO2012Schema.csv", package = "useeior"),
                                             sep = ",", header = TRUE)
  SectorCPIIO <- cbind(SectorGDPIndustrytoIO, SectorCPI)
  # Keep Sector rows
  SectorCPIIO <- SectorCPIIO[!SectorCPIIO$BEA_2012_Sector_Code == "", c("BEA_2012_Sector_Code", year_range)]
  # Assign rownames as sector code
  rownames(SectorCPIIO) <- SectorCPIIO[, 1]
  SectorCPIIO[, 1] <- NULL
  # Convert values to numeric format
  SectorCPIIO[] <- as.data.frame(apply(SectorCPIIO, 2, as.numeric))

  # Put CPIIO tables in the CPIIOList
  CPIIOList <- list(DetailCPIIO, SummaryCPIIO, SectorCPIIO)
  names(CPIIOList) <- c("Detail", "Summary", "Sector")
  return(CPIIOList)
}

#' Generate Margins table using either Industry Margins (BEA Margins) or Final Consumer Margins (BEA PCE and PEQ Bridge data).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param marginsource A character indicating the source of Margins, either "Industry" or "FinalConsumer".
#' @return A dataframe containing CommodityCode, and margins for ProducersValue, Transportation, Wholesale, Retail and PurchasersValue.
getMarginsTable <- function (model, marginsource) {
  # Load Margins or PCE and PEQ Bridge data
  if (model$specs$BaseIOSchema==2012) {
    if (marginsource=="intermediate") {
      MarginsTable <- useeior::Detail_Margins_2012_BeforeRedef[, 3:9]
    } else {
      # Use PCE and PEQ Bridge tables
      PCE <- useeior::Detail_PCE_2012[, 3:9]
      PEQ <- useeior::Detail_PEQ_2012[, 3:9]
      MarginsTable <- rbind(PCE, PEQ)
    }
  }
  # Map to Summary and Sector level
  crosswalk <- unique(useeior::MasterCrosswalk2012[,c("BEA_2012_Sector_Code", "BEA_2012_Summary_Code", "BEA_2012_Detail_Code")])
  MarginsTable <- merge(MarginsTable, crosswalk, by.x = "CommodityCode", by.y = "BEA_2012_Detail_Code")
  # Aggregate by CommodityCode (dynamic to model BaseIOLevel) and CommodityDescription
  if (!model$specs$BaseIOLevel=="Detail") {
    MarginsTable$CommodityCode <- MarginsTable[, paste("BEA_2012", model$specs$BaseIOLevel, "Code", sep = "_")]
  }
  value_columns <- c("ProducersValue", "Transportation", "Wholesale", "Retail", "PurchasersValue")
  MarginsTable <- stats::aggregate(MarginsTable[, value_columns], by = list(MarginsTable$CommodityCode), sum)
  colnames(MarginsTable)[1] <- "CommodityCode"
  # Keep the Commodities specified in model
  MarginsTable <- merge(MarginsTable, as.data.frame(model$Commodities), by.x = "CommodityCode", by.y = "model$Commodities", all.y = TRUE)
  MarginsTable[is.na(MarginsTable)] <- 0
  MarginsTable <- MarginsTable[match(model$Commodities, MarginsTable$CommodityCode), ]
  # Transform MarginsTable from Commodity to Industry format
  if (model$specs$CommoditybyIndustryType=="Industry") {
    # Generate a commodity x industry commodity mix matrix, see Miller and Blair section 5.3.2
    CommodityMix <- generateCommodityMixMatrix(model)
    MarginsTable_Industry <- as.data.frame(model$Industries)
    colnames(MarginsTable_Industry) <- "IndustryCode"
    # Transform ProducerValue from Commodity to Industry format
    # ! Not transforming Transportation, Wholesale and Retail to Industry format now
    MarginsTable_Industry[, "ProducersValue"] <- as.vector(MarginsTable[, "ProducersValue"]%*%CommodityMix)
    # Merge Industry Margins Table with Commodity Margins Table
    MarginsTable <- merge(MarginsTable_Industry, MarginsTable[, -which(names(MarginsTable)=="ProducersValue")],
                          by.x = "IndustryCode", by.y = "CommodityCode", all.x = TRUE)
    # Replace NA with zero
    MarginsTable[is.na(MarginsTable)] <- 0
  }
  MarginsTable$PurchasersValue <- rowSums(MarginsTable[, c("ProducersValue", "Transportation", "Wholesale", "Retail")])
  # Rename code column from CommodityCode/IndustryCode to SectorCode
  colnames(MarginsTable)[1] <- "SectorCode"
  return(MarginsTable)
}

