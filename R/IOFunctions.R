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
  for (s in industryoutputfractions) {
    if (abs(1-s)>0.01) {
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
  if (year==2012) {
    for (s in CommodityCPI) {
      if (abs(100-s)>0.3) {
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
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @return A Domestic Use table with rows as commodity codes and columns as industry and final demand codes
generateDomesticUse <- function(Use, model) {
  # Load Import matrix
  if (model$specs$BaseIOLevel != "Sector") {
    Import <- get(paste(model$specs$BaseIOLevel, "Import",
                        model$specs$IOYear, "BeforeRedef", sep = "_"))*1E6
  } else {
    # Load Summary level Import matrix
    Import <- get(paste("Summary_Import", model$specs$IOYear, "BeforeRedef", sep = "_"))*1E6
    # Aggregate Import from Summary to Sector
    Import <- as.data.frame(aggregateMatrix(as.matrix(Import), "Summary", "Sector", model))
  }
  Import <- Import[rownames(Use), colnames(Use)]
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
    Supply <- get(paste(model$specs$BaseIOLevel, "Supply", model$specs$IOYear,
                        sep = "_")) * 1E6
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
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @return An international trade adjustment vector with names as commodity codes
generateInternationalTradeAdjustmentVector <- function(Use, model) {
  # Load Import matrix
  if (model$specs$BaseIOLevel!="Sector") {
    Import <- get(paste(model$specs$BaseIOLevel, "Import", model$specs$IOYear, "BeforeRedef", sep = "_"))*1E6
  } else {
    # Load Summary level Import matrix
    Import <- get(paste("Summary_Import", model$specs$IOYear, "BeforeRedef", sep = "_"))*1E6
    # Aggregate Import from Summary to Sector
    Import <- as.data.frame(aggregateMatrix(as.matrix(Import), "Summary", "Sector", model))
  }
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

#' Create import Use table and validate domestic+import against full use model .
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @return A model object with explicit import components.
buildModelwithImportFactors <- function(model) {
  # Deriving the economic component of the Swedish equation for import factors: f^(d+m) = S^d*L^d*y^d + Q^t*A^m*L^d*y^d + Q^t*y^m + f^h
  # S and Q are the environmental intensity matrices, so getting rid of those components we would have the following (where x is the economic component of f)
  # x^(d+m) = L^d*y^d + A^m*L^d*y^d + y^m + f^h
  # Since f^h is not currently part of the useeior model calculations, we drop it:
  # x^(d+m) = L^d*y^d + A^m*L^d*y^d + y^m 
  # The resulting expression should be equivalent to the x = L*y such that
  # x^(d+m) = x = L*y
  
  cat("\n Building Import A (A_m) accounting for ITA in Domestic FD.\n")
  # Re-derive import values in Use and final demand
  # _m denotes import-related structures
  model$UseTransactions_m <- model$UseTransactions - model$DomesticUseTransactions

  # Including InternationalTradeAdjustment in DomesticFinalDemand for import factors calculations
  model$DomesticFDWithITA <- model$DomesticFinalDemand
  model$DomesticFDWithITA[,"F050/US"] <- model$InternationalTradeAdjustment
  model$ImportFinalDemand <- model$FinalDemand - model$DomesticFDWithITA
  
  model$U_n_m <- normalizeIOTransactions(model$UseTransactions_m, model$IndustryOutput) #normalized imported Use
  
  if(model$specs$CommodityorIndustryType == "Commodity") {
    logging::loginfo("Building commodity-by-commodity A_m matrix (imported direct requirements)...")
    model$A_m <- model$U_n_m %*% model$V_n
  } else if(model$specs$CommodityorIndustryType == "Industry") {
    logging::loginfo("Building industry-by-industry A_m matrix (imported direct requirements)...")
    model$A_m <- model$V_n %*% model$U_d_m
  }
  model$M <- model$B %*% model$L
  model$M_m <- loadExternalImportFactors(model)
  
  # Fill in flows for M_m not found in Import Factors but that exist in model and align order
  M_m <- rbind(model$M_m, model$M[setdiff(rownames(model$M), rownames(model$M_m)),])
  M_m <- M_m[rownames(model$M), ]

  
  # # Check that import values are the same as the original import data 
  # # Note that this check is not meant to be included in the code
  # Import <- get(paste("Summary_Import", model$specs$IOYear, "BeforeRedef", sep = "_"))*1E6
  # rownames(Import) <-rownames(UseTransactions_m)
  # colnames(Import[1:71]) <- colnames(UseTransactions_m[1:71])
  # temp <- UseTransactions_m - Import[,1:71]
  # sum(sum(temp)) == 0 # should be TRUE
  calculateAndValidateImportA(model)

  return(model)
}


#' Create import direct requirements table based on import use and import y tables
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @param UseTransactions_m A use table that describes the import portion of production only.
# #' @param FD_m A final demand TABLE that describes the import portion of final demand only
#' @param y A final demand VECTOR used for calculating the conventional model results, L*Y
#' @param y_d A final demand VECTOR used for validating the model results using the A_m calculation.
#' @return A calculated direct requirements table
# calculateAndValidateImportA <- function(model, UseTransactions_m, FD_m, y = NULL, y_d = NULL){
calculateAndValidateImportA <- function(model, y = NULL, y_d = NULL){
  
  U_n_m <- model$U_n_m
  A_m <- model$A_m
  
  
  # Calculate production demand vector
  FD_columns <- unlist(sapply(list("HouseholdDemand", "InvestmentDemand", 
                                   "ChangeInventories", "Export", "Import",
                                   "GovernmentDemand"),
                              getVectorOfCodes, ioschema = model$specs$BaseIOSchema,
                              iolevel = model$specs$BaseIOLevel))
  FD_columns <- model$FinalDemandMeta$Code_Loc[which(model$FinalDemandMeta$Code %in% FD_columns)] #get the right column names, with location ids
  
  # calculate "standard" x
  y <- rowSums(model$FinalDemand[,c(FD_columns)])
  x <- model$L %*% y # if y = I, then x = model$L. I <- diag(nrow(model$A))
  
  # Calculate x as domestic + import components
  y_m <- rowSums(model$ImportFinalDemand[,c(FD_columns)])
  y_d <- rowSums(model$DomesticFDWithITA[,c(FD_columns)])
  
  x_d <- model$L_d %*% y_d
  x_dm <- x_d + A_m%*%x_d + y_m
  
  
  # Validate results
  rel_dif_x <- (x-x_dm)/x_dm
  failures <- compare2RVectorTotals(x_dm, x)
  
  
  
  # TODO: Move this to new function - testing adding environmental import factors
  # Including the environmental components, S^d and Q^t, of the Swedish equation for import factors: f^(d+m) = S^d*L^d*y^d + Q^t*A^m*L^d*y^d + Q^t*y^m + f^h
  # Since f^h is not currently part of the useeior model calculations, we drop it:

  # f^(d+m) = S^d*L^d*y^d + Q^t*A^m*L^d*y^d + Q^t*y^m (eq 1)

  # Note that: 
  # S^d = model$B, where model$B is used for both domestic and non-domestic calculations in the standard results calculations.
  # L^d = model$L_d.
  # y^d = y_d as defined in the code above
  # Q^t = model$M. This is because the non-domestic L matrix in the M calculation (model$B %*% model$L) takes the place of the MRIO-based L matrix, 
        # even though the satellite table of environmental coefficients (B) is equivalent in this case.
  # A^m = A_m as defined in the code above
  # y^m = y_m as defined in the code above
  
  # Thus, Eq. 1 then becomes
  
  # f^(d+m) = model$B %*% model$L_d %*% y_d + model$M %*% A_m %*% model$L_d %*% y_d + model$M %*% y_m (eq 2)
  
  # "Standard" result calculation is model$B %*% model$L %*% y = model$M %*% y
  cat("\n Calculating results using import A (A_m).\n")
  standard_M <- model$M
  result_Standard <- standard_M %*% y
  M_m <- standard_M #Q^t = M_m = imported M matrix, which for validation purposes is equivalent to the current standard_M
  result_M <- (model$B %*% model$L_d %*% y_d) + (M_m %*% A_m %*% model$L_d %*% y_d + M_m %*% y_m) # parentheses used to denote (domestic) and (import) components

  rel_dif_result <- (result_Standard - result_M)/result_M
  result_failures <- compare2RVectorTotals(result_M, result_Standard)

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
  UsePUR <- get(paste(specs$BaseIOLevel, "Use", specs$IOYear, "PUR", Redef, sep = "_"))
  UsePRO <- get(paste(specs$BaseIOLevel, "Use", specs$IOYear, "PRO", Redef, sep = "_"))
  # Load Supply table
  Supply <- get(paste(specs$BaseIOLevel, "Supply", specs$IOYear, sep = "_"))
  
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
  # Load Supply table
  Supply <- get(paste(model$specs$BaseIOLevel, "Supply", model$specs$IOYear,
                      sep = "_"))
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
