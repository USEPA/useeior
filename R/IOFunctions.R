#' Adjust Industry output based on CPI.
#'
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param output_type Type of the output, e.g. "Commodity" or "Industry"
#'
#' @return A dataframe contains adjusted Industry output with row names being BEA sector code.
adjustOutputbyCPI <- function (outputyear, referenceyear, location_acronym, IsRoUS, model, output_type) {
  # Load Industry Gross Output
  if (model$specs$PrimaryRegionAcronym == "US") {
    Output <- cbind.data.frame(rownames(model$MultiYearIndustryOutput),
                               model$MultiYearIndustryOutput[, as.character(outputyear)])
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
  # Adjust output based on CPI
  AdjustedOutput <- merge(Output, model[[paste0("MultiYear", output_type, "CPI")]][, as.character(c(referenceyear, outputyear))],
                          by.x = "SectorCode", by.y = 0)
  AdjustedOutput$DollarRatio <- AdjustedOutput[, as.character(referenceyear)]/AdjustedOutput[, as.character(outputyear)]
  AdjustedOutput[, paste(outputyear, "IndustryOutput", sep = "")] <- AdjustedOutput$Output * AdjustedOutput$DollarRatio
  # Assign rownames and keep wanted column
  rownames(AdjustedOutput) <- AdjustedOutput$SectorCode
  AdjustedOutput <- AdjustedOutput[rownames(model[[paste0("MultiYear", output_type, "CPI")]]),
                                   paste(outputyear, "IndustryOutput", sep = ""), drop = FALSE]
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
  # Generate direct requirements matrix (commodity x industry) from Use, see Miller and Blair section 5.1.1
  if (domestic==TRUE) {
    B <- normalizeIOTransactions(model$DomesticUseTransactions, model$IndustryOutput) # B = U %*% solve(x_hat)
  } else {
    B <- normalizeIOTransactions(model$UseTransactions, model$IndustryOutput) # B = U %*% solve(x_hat)
  }
  return(B)
}

#' Generate Market Shares matrix from Make table.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return Market Shares matrix of the model.
generateMarketSharesfromMake <- function(model) {
  # Generate market shares matrix (industry x commodity) from Make, see Miller and Blair section 5.3.1
  D <- normalizeIOTransactions(model$MakeTransactions, model$CommodityOutput) # D = V %*% solve(q_hat)
  # Put in code here for adjusting marketshares to remove scrap
  return(D)
}

#' Generate Commodity Mix matrix.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return Commodity Mix matrix of the model.
generateCommodityMixMatrix <- function (model) {
  # Generate commodity mix matrix (commodity x industry), see Miller and Blair section 5.3.2
  C <- normalizeIOTransactions(t(model$MakeTransactions), model$IndustryOutput) # C = V' %*% solve(x_hat)
  # Validation: check if column sums equal to 1
  industryoutputfractions <- colSums(C)
  for (s in industryoutputfractions) {
    if (abs(1-s)>0.01) {
      stop("Error in commoditymix")
    }
  }
  return(C)
}

#' Generate Commodity output by transforming Industry output using Commodity Mix matrix.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains adjusted Commodity output.
transformIndustryOutputtoCommodityOutputforYear <- function(year, model) {
  # Generate adjusted industry output by location
  IndustryOutput <- model$MultiYearIndustryOutput[, as.character(year)]
  # Use CommodityMix to transform IndustryOutput to CommodityOutput
  CommodityMix <- generateCommodityMixMatrix(model)
  CommodityOutput <- as.numeric(CommodityMix %*% IndustryOutput)
  return(CommodityOutput)
}

#' Generate Commodity CPI by transforming Industry CPI using Commodity Mix matrix.
#' @param year Year of Industry CPI.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe contains adjusted Commodity CPI.
transformIndustryCPItoCommodityCPIforYear <- function(year, model) {
  # Generate adjusted industry CPI by location
  IndustryCPI <- model$MultiYearIndustryCPI[, as.character(year)]
  # Use MarketShares (of model IO year) to transform IndustryCPI to CommodityCPI
  MarketShares <- generateMarketSharesfromMake(model)
  # The transformation is essentially a I x 1 matrix %*% a C x I matrix which yields a C x 1 matrix
  CommodityCPI <- as.numeric(IndustryCPI %*% MarketShares)
  # Non-industry sectors would have CommodityCPI of 0
  # To avoid interruption in later calculations, they are forced to 100
  CommodityCPI[CommodityCPI==0] <- 100
  # Validation: check if IO year CommodityCPI is 100
  if (year==2012) {
    for (s in CommodityCPI) {
      if (abs(100-s)>0.3) {
        stop("Error in CommodityCPI")
      }
    }
  }
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
