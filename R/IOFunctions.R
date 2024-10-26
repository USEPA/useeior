# Functions implementing core input-output analysis algorithms

#' Adjust Industry output based on CPI.
#' @param outputyear Year of Industry output.
#' @param referenceyear Year of the currency reference.
#' @param location_acronym Abbreviated location name of the model, e.g. "US" or "GA".
#' @param IsRoUS A logical parameter indicating whether to adjust Industry output for Rest of US (RoUS).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param output_type Type of the output, e.g. "Commodity" or "Industry"
#' @return A dataframe contains adjusted Industry output with row names being BEA sector code.
adjustOutputbyCPI <- function (outputyear, referenceyear, location_acronym, IsRoUS, model, output_type) {
  # Load Industry Gross Output
  selected_rows <- grepl(location_acronym, rownames(model$MultiYearIndustryOutput))
  Output <- cbind.data.frame(rownames(model$MultiYearIndustryOutput[selected_rows, ]),
                             model$MultiYearIndustryOutput[selected_rows, as.character(outputyear)])
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
normalizeIOTransactions <- function (IO_transactions_df, IO_output_df) {
  # Replace 0 in IO_transactions_df and IO_output_df with 1E-3 to avoid errors in solve(x_hat)
  for (s in names(IO_output_df[IO_output_df == 0])){
    IO_transactions_df[s, s] <- 1E-3    
  }
  IO_output_df[IO_output_df == 0] <- 1E-3
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
  if (model$specs$IODataSource == "stateior" && !is.null(model$specs$DisaggregationSpecs)){
    # increase tolerance for disaggregated state models
    tolerance <- 0.02
  } else {
    tolerance <- 0.01
  }
  for (s in industryoutputfractions) {
    if (abs(1-s)>tolerance) {
      stop("Error in commoditymix")
    }
  }
  return(C)
}

#' Generate Commodity output by transforming Industry output using Commodity Mix matrix.
#' @param year Year of Industry output
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
  if (model$specs$IODataSource == "stateior" && !is.null(model$specs$DisaggregationSpecs)){
    # increase tolerance for disaggregated state models
    tolerance <- 3.0
  } else {
    tolerance <- 0.4
  }
  if (year==model$specs$BaseIOSchema) {
    for (s in CommodityCPI) {
      if (abs(100-s)>tolerance) {
        stop("Error in CommodityCPI")
      }
    }
  }
  return(CommodityCPI)
}

#' Transform Direct Requirements matrix with Market Shares matrix, works for both commodity-by-commodity and industry-by-industry model types.
#' @param B Marginal impact per unit of the environmental flows.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param D Market Shares matrix.
#' @return Direct Requirements matrix.
transformDirectRequirementswithMarketShares <- function (B, D, model) {
  # Only generate result if the column names of the direct requirements table match the row names of the market shares matrix
  if (all(colnames(B) == rownames(D)) && all(colnames(D) == rownames(B))) {

  } else {
    stop("Column names of the direct requirements do not match the row names of the market shares matrix.")
  }
  if (model$specs$CommodityorIndustryType == "Commodity") {
    # commodity model DR_coeffs = dr %*% ms (CxI x IxC) = CxC
    A <- B %*% D
    dimnames(A) <- c(dimnames(B)[1], dimnames(D)[2])
  } else if (model$specs$CommodityorIndustryType == "Industry") {
    # industry model DR_coeffs = ms %*% dr (IxC x CxI) = IxI
    A <- D %*% B
    dimnames(A) <- c(dimnames(D)[1], dimnames(B)[2])
  } else {
    stop("CommodityorIndustryType not specified or incorrectly specified for model.")
  }
  return(A)
}

#' Transform Final Demand (commodity x sector) with Market Shares matrix
#' to Final Demand (industry x sector)
#' @param Fdf Final Demand data.frame.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return Final Demand (industry x sector) data.frame
transformFinalDemandwithMarketShares <- function (Fdf, model) {
  D <- generateMarketSharesfromMake(model)
  # See Miller and Blair section 5.3.7 (pg 197)
  Fmatrix <- D %*% as.matrix(Fdf)
  return(as.data.frame(Fmatrix))
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
#' @param Use, dataframe of a Use table
#' @param Import, dataframe of a Import table
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @return A Domestic Use table with rows as commodity codes and columns as industry and final demand codes
generateDomesticUse <- function(Use, Import, model) {
  # Adjust Import matrix to BAS price if model is in BAS price
  # Note: according to the documentation in BEA Import matrix, import values in
  # the Import matrix are in producer (PRO) values. For PRO models, imports in the
  # Import matrix are valued at their domestic port value, while imports in Use
  # (Make-Use framework) are valued at their foreign port value, meaning
  # domestic port value = foreign port value + 
  #                       the value of all transportation and insurance services to import +
  #                       customs duties.
  # To get an Import matrix in BAS price, customs duties (i.e. import duties or tax on imports)
  # needs to be subtracted from the original Import matrix
  if (model$specs$BasePriceType == "BAS") {
    # Find "MDTY - import duties" in Supply table
    Supply <- get(paste(na.omit(c(model$specs$BaseIOLevel, "Supply", model$specs$IOYear, schema)),
                        collapse = "_")) * 1E6
    ImportDuty <- Supply[rownames(Import), "MDTY"]
    # Subtract import duties from  Import matrix
    # Expanding it to a matrix based on the Import matrix, except for the import column
    # Then subtract the matrix from the Import matrix to convert it from PRO to BAS
    import_col <- model$FinalDemandMeta[model$FinalDemandMeta$Group == "Import",
                                        "Code"]
    non_import_cols <- setdiff(colnames(Import), import_col)
    ratio_m <- Import/rowSums(Import[, non_import_cols])
    ratio_m[is.na(ratio_m)] <- 1/(ncol(Import) - 1)
    Import_BAS <- Import[, non_import_cols] - diag(ImportDuty) %*% as.matrix(ratio_m)
    # Recalculate import column in the Import matrix by adding import duties
    Import_BAS[, import_col] <- Import[, import_col] + ImportDuty
    # Assign Import_BAS to Import
    Import <- Import_BAS
  }
  # Subtract Import from Use
  DomesticUse <- Use - Import
  # Adjust Import column in DomesticUse to 0.
  # Note: the original values in Import column are essentially the International Trade Adjustment
  # that are reserved and added as an additional column (F050/F05000) in DomesticUse.
  DomesticUse[, getVectorOfCodes(model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Import")] <- 0
  return(DomesticUse)
}

#' Generate international trade adjustment vector from Use and Import matrix.
#' @param Use, dataframe of a Use table
#' @param Import, dataframe of a Import table
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @return An international trade adjustment vector with names as commodity codes
generateInternationalTradeAdjustmentVector <- function(Use, Import, model) {
  # Define Import code
  ImportCode <- getVectorOfCodes(model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Import")
  ImportCode <- ImportCode[startsWith(ImportCode, "F")]
  # Calculate InternationalTradeAdjustment
  # In the Import matrix, the imports column is in domestic (US) port value.
  # But in the Use table, it is in foreign port value.
  # domestic port value = foreign port value + value of all transportation and insurance services to import + customs duties
  # See documentation of the Import matrix (https://apps.bea.gov/industry/xls/io-annual/ImportMatrices_Before_Redefinitions_DET_2007_2012.xlsx)
  # So, InternationalTradeAdjustment <- Use$Imports - Import$Imports
  # InternationalTradeAdjustment is essentially 'value of all transportation and insurance services to import' and 'customs duties'
  InternationalTradeAdjustment <- Use[, ImportCode] - Import[rownames(Use), ImportCode]
  names(InternationalTradeAdjustment) <- rownames(Use)
  return(InternationalTradeAdjustment)
}


#' Convert Use table in the Supply-Use framework from purchasers' price (PUR)
#' to basic price (BAS)
#' @param UseSUT_PUR, a Use table (from the Supply-Use framework) in purchasers' price (PUR)
#' @param specs, model specifications.
#' @param io_codes, a list of BEA IO codes.
#' @return A Use table in basic price (BAS)
convertUsefromPURtoBAS <- function(UseSUT_PUR, specs, io_codes) {
  # Load UsePRO and UsePUR under Make-Use framework
  Redef <- ifelse(specs$BasewithRedefinitions, "AfterRedef", "BeforeRedef")
  schema <- getSchemaCode(specs)
  UsePUR <- get(paste(na.omit(c(specs$BaseIOLevel, "Use", specs$IOYear, "PUR", Redef, specs)), collapse = "_"))
  UsePRO <- get(paste(na.omit(c(specs$BaseIOLevel, "Use", specs$IOYear, "PRO", Redef, specs)), collapse = "_"))
  # Load Supply table
  Supply <- get(paste(na.omit(c(specs$BaseIOLevel, "Supply", specs$IOYear, specs)), collapse = "_"))
  
  # Convert from PUR to PRO by removing margins obtained from Supply table
  rows <- io_codes$Commodities
  cols <- c(io_codes$Industries,
            intersect(colnames(UseSUT_PUR), io_codes$FinalDemandCodes))
  # Calculate margins in matrix form using Use tables under the Make-Use framework
  # Note: there are no retail (comm) sectors in UsePUR, so these sectors in the
  # margins matrix are filled with NA.
  margins <- UsePUR[rows, cols] - UsePRO[rows, cols]
  # Update rownames of the matrix
  rownames(margins) <- rows
  # Replace NA with 0 because retail sectors should not have additional margins
  margins[is.na(margins)] <- 0
  # Extract margins from Supply
  margins_Supply <- rowSums(Supply[rows, c("TRADE", "TRANS")])
  # Allocate margins_Supply throughout Use based on margins matrix
  margins_ratio_m <- margins/rowSums(margins)
  margins_ratio_m[is.na(margins_ratio_m)] <- 1/ncol(margins_ratio_m)
  UseSUT_PRO <- UseSUT_PUR[rows, cols] - diag(margins_Supply) %*% as.matrix(margins_ratio_m)
  
  # Convert from PRO to BAS by removing tax less subsidies from the Supply table
  # Note: import duties (MDTY) is considered tax on imported goods, see page 3 of
  # https://apps.bea.gov/scb/pdf/2015/09%20September/0915_supply_use_tables_for_the_united_states.pdf
  tax_less_subsidies <- rowSums(Supply[rows, io_codes$TaxLessSubsidiesCodes])
  # Allocate tax_less_subsidies throughout Use based on consumption of commodities
  ratio_m <- UseSUT_PRO/rowSums(UseSUT_PRO)
  ratio_m[is.na(ratio_m)] <- 1/ncol(ratio_m)
  UseSUT_BAS <- UseSUT_PRO - diag(tax_less_subsidies) %*% as.matrix(ratio_m)
  # Append right columns, including T001 and T109, and bottom rows, including
  # Value Added and totals, back to Use table
  UseSUT_BAS <- rbind(cbind(UseSUT_BAS,
                            UseSUT_PUR[rows, setdiff(colnames(UseSUT_PUR),
                                                     colnames(UseSUT_BAS))]),
                      UseSUT_PUR[setdiff(rownames(UseSUT_PUR),
                                         rownames(UseSUT_BAS)), ])
  return(UseSUT_BAS)
}

#' Generate tax less subsidies table using BEA Supply table which include
#' total product supply in basic price and tax less subsidies for all commodities
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A data.frame containing CommodityCode, basic price, tax less subsidies,
#' and producer price of total product supply
generateTaxLessSubsidiesTable <- function(model) {
  schema <- getSchemaCode(model$specs)
  # Load Supply table
  Supply <- get(paste(na.omit(c(model$specs$BaseIOLevel, "Supply", model$specs$IOYear, schema)),
                      collapse = "_"))
  # Get basic price and tax less subsidies vectors from Supply
  import_cols <- getVectorOfCodes(model$specs$BaseIOSchema,
                                  model$specs$BaseIOLevel,
                                  "Import")
  import_cols <- import_cols[!startsWith(import_cols, "F")]
  taxlesssubsidies_col <- getVectorOfCodes(model$specs$BaseIOSchema,
                                           model$specs$BaseIOLevel,
                                           "TaxLessSubsidies")
  TaxLessSubsidies <- cbind(rowSums(Supply[model$Commodities$Code,
                                           c(model$Industries$Code, import_cols)]),
                            Supply[model$Commodities$Code, taxlesssubsidies_col])
  colnames(TaxLessSubsidies)[1] <- "BasicValue"
  # Calculate Producer price
  TaxLessSubsidies$ProducerValue <- rowSums(TaxLessSubsidies)
  # Assign Code_Loc to TaxLessSubsidies
  TaxLessSubsidies <-  merge(TaxLessSubsidies,
                             model$Commodities[,c("Code","Name", "Code_Loc")],
                             by.x = 0, by.y = "Code", all.y = TRUE)
  return(TaxLessSubsidies)
}
