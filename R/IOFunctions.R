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
        Output <- model$IndustryOutput[model$IndustryOutput$Location==location_acronym, 
                                       c("SectorCode", as.character(outputyear)), drop = FALSE]
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
#' @export
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

# Function not used in model
# #' Generate non-scrap ratios
# #' @return A dataframe with rows being model industries and a column for "non_scrap_ratios" for that industry.
# generateNonScrapRatios <- function() {
#   # Merge scrap from model Make transactions and Industry output
#   V_scrap <- model$MakeTransactions[, ModelScrapCode, drop = FALSE]
#   V_scrap_total <- merge(V_scrap, model$BEA$MakeIndustryOutput, by = 0)
#   IndustryTotalCode <- colnames(model$BEA$MakeIndustryOutput)
#   V_scrap_total[,"nonscrap_ratio"] <- (V_scrap_total[,IndustryTotalCode]-V_scrap_total[, ModelScrapCode])/V_scrap_total[, IndustryTotalCode]
#   row.names(V_scrap_total) <- V_scrap_total[,"Row.names"]
#   non_scrap_ratios <- V_scrap_total[,"nonscrap_ratio", drop=FALSE]
#   return(non_scrap_ratios)
# }

#' Transform Direct Requirements matrix with Market Shares matrix, works for both commodity-by-commodity and industry-by-industry model types.
#' @param B Marginal impact per unit of the environmental flows.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param D Market Shares matrix.
#' @return Direct Requirements matrix.
transformDirectRequirementswithMarketShares <- function (B, D, model) {
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
    logging::logerror("CommoditybyIndustryType not specified for model or incorrectly specified")
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
    MarginsTable_Industry <- as.data.frame(model$BEA$Industries)
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

#' Generate domestic Use table by adjusting Use table based on Import matrix.
#' @param Use An original Use table.
#' @param specs Model specifications.
#' @return A Domestic Use table.
generatDomesticUse <- function(Use, specs) {
  # Load Import matrix
  if (specs$BaseIOLevel!="Sector") {
    Import <- get(paste(specs$BaseIOLevel, "Import", specs$IOYear, "BeforeRedef", sep = "_"))*1E6
  } else {
    # Load Summary level Import matrix
    Import <- get(paste("Summary_Import", specs$IOYear, "BeforeRedef", sep = "_"))*1E6
    # Aggregate Import from Summary to Sector
    Import <- as.data.frame(aggregateMatrix(as.matrix(Import), "Summary", "Sector", specs))
  }
  # Sort rows and columns in Import to match those in Use
  Import <- Import[rownames(Use), colnames(Use)]
  # Define Export and Import codes
  ExportCode <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Export")
  ImportCode <- getVectorOfCodes(specs$BaseIOSchema, specs$BaseIOLevel, "Import")
  # Calculate ImportCost.
  # The imports column in the Import matrix is in foreign port value.
  # But in the Use table it is in domestic port value.
  # domestic port value = foreign port value + value of all transportation and insurance services to import + customs duties
  # See documentation of the Import matrix (https://apps.bea.gov/industry/xls/io-annual/ImportMatrices_Before_Redefinitions_SUM_1997-2019.xlsx)
  # So, ImportCost <- Use$Imports - Import$Imports
  ImportCost <- Use[, ImportCode] - Import[, ImportCode]
  # Estimate DomesticUse
  DomesticUse <- Use
  # Calculate row_sum of Use, except for Export and Import, for allocating ImportCost
  row_sum <- rowSums(Use) - (Use[, ExportCode] + Use[, ImportCode])
  # Calculate allocation ratios
  ratio <- sweep(Use, 1, FUN = "/", row_sum)
  ratio[is.na(ratio)] <- 0
  # Subtract Import from Use, then allocate ImportCost to each Industry (column), except for Export and Import
  DomesticUse <- Use - Import + sweep(ratio, 1, FUN = "*", ImportCost)
  # Adjust Export and Import columns
  DomesticUse[, ExportCode] <- Use[, ExportCode]
  DomesticUse[, ImportCode] <- 0
  return(DomesticUse)
}
