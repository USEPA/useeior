# Validation functions

#' Compares the total flows against the model flow totals result calculation with the total demand
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param use_domestic, a logical value indicating whether to use domestic demand vector
#' @param tolerance, a numeric value, tolerance level of the comparison
#' @return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#' @export
compareEandLCIResult <- function(model, use_domestic = FALSE, tolerance = 0.05) {
  # Prepare left side of the equation
  CbS_cast <- standardizeandcastSatelliteTable(model$CbS,model)
  B <- as.matrix(CbS_cast)
  Chi <- generateChiMatrix(model, "Industry")
  # Check if Chi columns match B
  if (!identical(colnames(B), colnames(Chi))) {
    stop("columns in Chi and B do not match")
  }
  Chi <- Chi[match(rownames(B), rownames(Chi)), ]
  B_chi <- B*Chi
  
  # Generate E
  E <- prepareEfromtbs(model)
  E <- as.matrix(E[rownames(B), colnames(B)])
  
  B_chi <- removeHybridProcesses(model, B_chi)
  E <- removeHybridProcesses(model, E)
  
  # Adjust E and B_chi if model is commodity-based
  if (model$specs$CommodityorIndustryType=="Commodity") {
    #transform E with commodity mix to put in commodity form
    E <- t(model$C_m %*% t(E)) 
    #Need to transform B_Chi to be in commodity form
    B_chi <- B_chi %*% model$V_n
  }

  # Calculate scaling factor c=Ly
  c <- calculateProductofLeontiefAndProductionDemand(model, use_domestic)

  #LCI = B dot Chi %*% c
  LCI <- t(calculateDirectPerspectiveLCI(B_chi, c))
  
  # Calculate relative differences
  rel_diff <- (LCI - E)/E
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  # Add LCI and E to validation list
  validation <- c(list("LCI" = LCI, "E" = E), validation)
  return(validation)
}

#' Calculate scaling vector with appropriate production demand vector
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param use_domestic, a logical value indicating whether to use domestic demand vector
#' @return c, a numeric vector with total $ values for each sector in model
calculateProductofLeontiefAndProductionDemand <- function (model, use_domestic) {
  if (use_domestic) {
    f <- model$DemandVectors$vectors[endsWith(names(model$DemandVectors$vectors), "Production_Domestic")][[1]]
    if (model$specs$IODataSource=="stateior") {
      f <- (f + model$DemandVectors$vectors[endsWith(names(model$DemandVectors$vectors), "Production_Domestic")][[2]])
    }
    y <- as.matrix(formatDemandVector(f, model$L_d))
    c <- getScalingVector(model$L_d, y)
  } else {
    f <- model$DemandVectors$vectors[endsWith(names(model$DemandVectors$vectors), "Production_Complete")][[1]]
    if (model$specs$IODataSource=="stateior") {
      f <- (f + model$DemandVectors$vectors[endsWith(names(model$DemandVectors$vectors), "Production_Complete")][[2]])
    }
    y <- as.matrix(formatDemandVector(f, model$L))
    c <- getScalingVector(model$L, y)
  }
  c <- removeHybridProcesses(model, c)
  return(c)  
}

#' Compares the total sector output against the model result calculation with the demand vector. and direct perspective.
#' Uses the model$FinalDemand and model$L
#' Works for the domestic model with the equivalent tables
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param use_domestic, a logical value indicating whether to use domestic demand vector
#' @param tolerance, a numeric value, tolerance level of the comparison
#' @return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#' @export
compareOutputandLeontiefXDemand <- function(model, use_domestic=FALSE, tolerance=0.05) {
  # Generate output and scaling vector
  if(model$specs$CommodityorIndustryType == "Commodity") {
    x <- model$q
  } else {
    x <- model$x
  }
  x <- removeHybridProcesses(model, x)
  
  # Calculate scaling factor c=Ly
  c <- calculateProductofLeontiefAndProductionDemand(model, use_domestic)
  
  # Row names should be identical
  if (!identical(rownames(c), names(x))) {
    stop("Sectors not aligned in model ouput variable and calculation result")
  }
  # Calculate relative differences
  rel_diff <- (c - x)/x
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  # Add c and x to validation list
  validation <- c(list("c" = c, "x" = x), validation)
  return(validation)
}

#' Compares the total commodity output against the summation of model domestic Use and production demand
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param tolerance, a numeric value, tolerance level of the comparison
#' @return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#' @export 
compareCommodityOutputandDomesticUseplusProductionDemand <- function(model, tolerance=0.05) {
  q <- removeHybridProcesses(model, model$q)
  demand <- model$DemandVectors$vectors[endsWith(names(model$DemandVectors$vectors),"Production_Domestic")][[1]]
  if (model$specs$IODataSource=="stateior") {
    demand <- (demand + model$DemandVectors$vectors[endsWith(names(model$DemandVectors$vectors), "Production_Domestic")][[2]])
  }
  x <- rowSums(model$U_d[removeHybridProcesses(model, model$Commodities$Code_Loc),
                         removeHybridProcesses(model, model$Industries$Code_Loc)]) +
       removeHybridProcesses(model, demand)
  # Row names should be identical
  if (!identical(names(q), names(x))) {
    stop("Sectors not aligned in model ouput variable and calculation result")
  }
  
  # Calculate relative differences
  rel_diff <- (q - x)/q
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  # Add q and x to validation list
  validation <- c(list("q" = q, "x" = x), validation)
  return(validation)
}

#' Compares the total commodity output multiplied by Market Share matrix and transformed by commodity CPI
#' against the total industry output transformed by industry CPI
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param tolerance, a numeric value, tolerance level of the comparison
#' @return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#' @export 
compareCommodityOutputXMarketShareandIndustryOutputwithCPITransformation <- function(model, tolerance=0.05) {
  commodityCPI_ratio <- model$MultiYearCommodityCPI[, "2017"]/model$MultiYearCommodityCPI[, "2012"]
  commodityCPI_ratio[is.na(commodityCPI_ratio)] <- 1
  
  industryCPI_ratio <- model$MultiYearIndustryCPI[, "2017"]/model$MultiYearIndustryCPI[, "2012"]
  industryCPI_ratio[is.na(industryCPI_ratio)] <- 1
  
  q <- removeHybridProcesses(model, model$q * commodityCPI_ratio)
  x <- as.numeric(model$C_m %*% removeHybridProcesses(model, model$x * industryCPI_ratio))
  
  # Calculate relative differences
  rel_diff <- (q - x)/x
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  # Add q and x to validation list
  validation <- c(list("q" = q, "x" = x), validation)
  return(validation)
}

#' Concatenate all satellite flows in model
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
prepareEfromtbs <- function(model) {
  E <- standardizeandcastSatelliteTable(model$TbS,model)
  return(E)
}

#' Generate Chi matrix, i.e. ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param output_type Either Commodity or Industry, default is Commodity
#' @return Chi matrix contains ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
generateChiMatrix <- function(model, output_type = "Commodity") {
  # Extract ModelYearOutput from model based on output_type
  if (output_type=="Commodity") {
    ModelYearOutput <- model$q
  } else {
    ModelYearOutput <- model$x
  }
  # Generate FlowYearOutput, convert it to model IOYear $
  TbS <- model$TbS
  FlowYearOutput <- data.frame()
  for (year in unique(TbS$Year)) {
    output <- model[[paste0("MultiYear", output_type, "Output")]][, as.character(year), drop = FALSE]
    # Adjust industry output to model year $
    CPI_df <- model[[paste0("MultiYear", output_type, "CPI")]][, as.character(c(model$specs$IOYear, year))]
    DollarRatio <- CPI_df[, as.character(model$specs$IOYear)]/CPI_df[, as.character(year)]
    # Replace NA with 1 in DollarRatio
    DollarRatio[is.na(DollarRatio)] <- 1
    output <- output * DollarRatio
    flows <- unique(TbS[TbS$Year==year, "Flow"])
    FlowYearOutput_y <- do.call(rbind, rep(output, times = length(flows)))
    rownames(FlowYearOutput_y) <- flows
    colnames(FlowYearOutput_y) <- rownames(output)
    FlowYearOutput <- rbind(FlowYearOutput, FlowYearOutput_y)
  }
  # Calculate Chi: divide FlowYearOutput by ModelYearOutput
  Chi <- as.matrix(sweep(FlowYearOutput[unique(TbS$Flow), ], 2, ModelYearOutput, "/"))
  # Replace 0 with 1
  Chi[Chi==0] <- 1
  Chi[is.na(Chi)] <- 1
  return(Chi)
}

#' Gets industry output from model Use and Make and checks if they are the same
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param tolerance A numeric value setting tolerance of the comparison
compareIndustryOutputinMakeandUse <- function(model, tolerance) {
  # Calculate Industry Output (x) from Make and Use tables
  x_make <-rowSums(model$V)
  x_use <- colSums(model$U[model$Commodities$Code_Loc, model$Industries$Code_Loc]) +
    colSums(model$U[model$ValueAddedMeta$Code_Loc, model$Industries$Code_Loc])
  # Sort x_make and x_use to have the same industry order (default model$Industries)
  x_make <- x_make[order(model$Industries$Code_Loc)]
  x_use <- x_use[order(model$Industries$Code_Loc)]
  # Check if x_make and x_use have the same industry order
  if (!identical(names(x_make), names(x_use))) {
    stop("industries in Make and Use do not match")
  }
  # Calculate relative differences in x_make and x_use
  rel_diff <- (x_use - x_make)/x_make
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  # Add x_use and x_make to validation list
  validation <- c(list("x_use" = x_use, "x_make" = x_make), validation)
  return(validation)
}

#' Validate result based on specified tolerance
#' @param result A data object to be validated
#' @param abs_diff A logical value indicating whether to validate absolute values
#' @param tolerance A numeric value setting tolerance of the comparison
#' @return A list contains confrontation details and validation results
validateResult <- function(result, abs_diff = TRUE, tolerance) {
  result[is.na(result)] <- 0
  # Validate result
  if (abs_diff) {
    validation <- as.data.frame(abs(result) <= tolerance)
  } else {
    validation <- as.data.frame(result <= tolerance)
  }
  validation$rownames <- rownames(validation)
  return(validation)
}

#' Extract validation passes or failures
#' @param validation A data.frame contains validation details
#' @param failure A logical value indicating whether to report failure or not
#' @return A data.frame contains validation results
extractValidationResult <- function(validation, failure = TRUE) {
  df <- reshape2::melt(validation, id.vars = "rownames")
  if (failure) {
    result <- df[df$value==FALSE, c("rownames", "variable")]
  } else {
    result <- df[df$value==TRUE, c("rownames", "variable")]
  }
  result[] <- sapply(result, as.character)
  return(result)
}

#' Format validation result
#' @param result Validation result to be formatted
#' @param abs_diff A logical value indicating whether to validate absolute values
#' @param tolerance A numeric value setting tolerance of the comparison
#' @return A list contains formatted validation results
formatValidationResult <- function(result, abs_diff = TRUE, tolerance) {
  # Validate result
  validation <- validateResult(result, abs_diff, tolerance)
  # Extract passes and failures
  passes <- extractValidationResult(validation, failure = FALSE)
  failures <- extractValidationResult(validation, failure = TRUE)
  N_passes <- nrow(passes)
  N_failures <- nrow(failures)
  return(list("RelativeDifference" = as.data.frame(result),
              "Pass" = passes, "N_Pass" = N_passes,
              "Failure" = failures, "N_Failure" = N_failures))
}

#' Check order of names (n1 and n2). Stop function execution if n1 != n2.
#' @param n1 Name vector #1
#' @param n2 Name vector #2
#' @param note Note about n1 and n2
checkNamesandOrdering <- function(n1, n2, note) {
  if (!identical(n1, n2)) {
    stop(paste(note, "not the same or not in the same order."))
  }
}

#' Run validation checks and print to console
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @export
printValidationResults <- function(model) {
  print("Validate that commodity output can be recalculated (within 1%) with the model total requirements matrix (L) and demand vector (y) for US production")
  econval <- compareOutputandLeontiefXDemand(model, tolerance = 0.01)
  print(paste("Number of sectors passing:",econval$N_Pass))
  print(paste("Number of sectors failing:",econval$N_Fail))
  print(paste("Sectors failing:", paste(unique(econval$Failure$rownames), collapse = ", ")))
  
  print("Validate that commodity output can be recalculated (within 1%) with model total domestic requirements matrix (L_d) and model demand (y) for US production")
  econval <- compareOutputandLeontiefXDemand(model, use_domestic=TRUE, tolerance = 0.01)
  print(paste("Number of sectors passing:",econval$N_Pass))
  print(paste("Number of sectors failing:",econval$N_Fail))
  print(paste("Sectors failing:", paste(unique(econval$Failure$rownames), collapse = ", ")))
  
  print("Validate that flow totals by commodity (E_c) can be recalculated (within 1%) using the model satellite matrix (B), market shares matrix (V_n), total requirements matrix (L), and demand vector (y) for US production")
  modelval <- compareEandLCIResult(model, tolerance = 0.01)
  print(paste("Number of flow totals by commodity passing:",modelval$N_Pass))
  print(paste("Number of flow totals by commodity failing:",modelval$N_Fail))
  
  print("Validate that flow totals by commodity (E_c) can be recalculated (within 1%) using the model satellite matrix (B), market shares matrix (V_n), total domestic requirements matrix (L_d), and demand vector (y) for US production")
  dom_val <- compareEandLCIResult(model, use_domestic=TRUE, tolerance = 0.01)
  print(paste("Number of flow totals by commodity passing:",dom_val$N_Pass))
  print(paste("Number of flow totals by commodity failing:",dom_val$N_Fail))
  print(paste("Sectors with flow totals failing:", paste(unique(dom_val$Failure$variable), collapse = ", ")))  
  
  print("Validate that commodity output are properly transformed to industry output via MarketShare")
  q_x_val <- compareCommodityOutputXMarketShareandIndustryOutputwithCPITransformation(model, tolerance = 0.01)
  print(paste("Number of flow totals by commodity passing:",q_x_val$N_Pass))
  print(paste("Number of flow totals by commodity failing:",q_x_val$N_Fail))
  print(paste("Sectors with flow totals failing:", paste(unique(q_x_val$Failure$rownames), collapse = ", ")))
  
if (model$specs$CommodityorIndustryType=="Commodity") {
  print("Validate that commodity output equals to domestic use plus production demand")
  q_val <- compareCommodityOutputandDomesticUseplusProductionDemand(model, tolerance = 0.01)
  print(paste("Number of flow totals by commodity passing:",q_val$N_Pass))
  print(paste("Number of flow totals by commodity failing:",q_val$N_Fail))
  print(paste("Sectors with flow totals failing:", paste(unique(q_val$Failure$rownames), collapse = ", ")))
}
}

#' Removes hybrid processes form a model object for successful validation
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param object A model object in the form of a matrix or vector
#' @return object with processes removed
removeHybridProcesses <- function(model, object) {
  if (model$specs$ModelType == "EEIO-IH") {
    if(typeof(object) == 'character'){
      object <- object[!object %in% model$HybridizationSpecs$Processes$Code_Loc]
    }
    else if(!is.null(colnames(object))) {
      object <- object[, !colnames(object) %in% model$HybridizationSpecs$Processes$Code_Loc]
    }
    else if(!is.null(rownames(object))) {
      object <- as.matrix(object[!rownames(object) %in% model$HybridizationSpecs$Processes$Code_Loc, ])
    }
    else {
      object <- object[!names(object) %in% model$HybridizationSpecs$Processes$Code_Loc]
    }
  }
  return(object)
}

#' Validate Leontief matrix (L) of two-region model and final demand against
#' SoI and RoUS output.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param state A text value specifying state of interest.
#' @return A list of validation components and result.
validate2RegionLagainstOutput <- function(model, state=NULL) {
  startLogging()
  #Get model data
  iolevel <- model$specs$BaseIOLevel
  ioschema <- model$specs$BaseIOSchema
  year <- model$specs$IOYear
  state_abb <- sub(".*-","",model$specs$ModelRegionAcronyms[1]) ## Extract characters after -
  
  # Define industries and commodities
  industries <- getVectorOfCodes(ioschema, iolevel, "Industry")
  commodities <- getVectorOfCodes(ioschema, iolevel, "Commodity")
  ita_column <- ifelse(iolevel == "Detail", "F05100", "F051")
  # Define state abbreviation
  logging::loginfo(paste0("Generating A matrix of ",state_abb," (SoI) Make table ..."))
  # SoI Make
  TwoRegionMake <- as.data.frame(model$V) # will need to modify this when this part is moved to a new buildTwoRegionA matrix function
  SoI_Make <- TwoRegionMake[endsWith(rownames(TwoRegionMake), state_abb),
                            endsWith(colnames(TwoRegionMake), state_abb)]
  # SoI commodity output
  TwoRegionCommodityOutput <- model$q
  SoI_Commodity_Output <- TwoRegionCommodityOutput[endsWith(names(TwoRegionCommodityOutput),
                                                            state_abb)]
  # SoI A matrix
  SoI_A <- normalizeIOTransactions(SoI_Make, SoI_Commodity_Output)
  # Check column sums of SoI_A
  if (all(abs(colSums(SoI_A) - 1) < 1E-3)) {
    logging::loginfo("FACT CHECK: column sums of A matrix of SoI Make table == 1.")
  } else {
    logging::logwarn("Column sums of A matrix of SoI Make table != 1")
  }
  
  logging::loginfo("Generating A matrix of RoUS Make table ...")
  # RoUS Make
  RoUS_Make <- TwoRegionMake[endsWith(rownames(TwoRegionMake), "RoUS"),
                             endsWith(colnames(TwoRegionMake), "RoUS")]
  # RoUS commodity output
  RoUS_Commodity_Output <- TwoRegionCommodityOutput[endsWith(names(TwoRegionCommodityOutput),
                                                             "RoUS")]
  
  # RoUS A matrix
  RoUS_A <- normalizeIOTransactions(RoUS_Make, RoUS_Commodity_Output)
  # Check column sums of RoUS_A
  if (all(abs(colSums(RoUS_A) - 1) < 1E-3)) {
    logging::loginfo("FACT CHECK: column sums of A matrix of RoUS Make table == 1.")
  } else {
    logging::logerror("Column sums of A matrix of RoUS Make table != 1")
  }
  
  # Two-region A matrix
  #                       Commodity                Commodity                  Industry                     Industry
  #                          SoI                     RoUS                        SoI                         RoUS
  #               +-----------------------+-------------------------+---------------------------+----------------------------+
  # Commodity SoI |           0           |            0            |norm(SoI2SoI_Use,  SoI_TIO)|norm(SoI2RoUS_Use, RoUS_TIO)|
  #               +-----------------------+-------------------------+---------------------------+----------------------------+
  # Commodity RoUS|           0           |            0            |norm(RoUS2SoI_Use, SoI_TIO)|norm(RoUS2RoUS_Use,RoUS_TIO)|
  #               +-----------------------+-------------------------+---------------------------+----------------------------+
  # Industry  SoI |norm(SoI_Make, SoI_TCO)|            0            |             0             |             0              |
  #               +-----------------------+-------------------------+---------------------------+----------------------------+
  # Industry  RoUS|           0           |norm(RoUS_Make, RoUS_TCO)|             0             |             0              |
  #               +-----------------------+-------------------------+---------------------------+----------------------------+
  #                           Total column sum must equal 1                 Total column sum can but shouldn't equal 1
  
  logging::loginfo("Generating two-region Domestic Use tables ...") 
  ls <- model$DomesticUseTransactionswithTrade
  TwoRegionIndustryOutput <- model$x
  SoI_Industry_Output <- TwoRegionIndustryOutput[endsWith(names(TwoRegionIndustryOutput),
                                                          state_abb)]
  RoUS_Industry_Output <- TwoRegionIndustryOutput[endsWith(names(TwoRegionIndustryOutput),
                                                           "RoUS")]
  # If industry/comm output == 0, it's not viable to generate A matrix, hence set it to 1.
  SoI_Industry_Output[SoI_Industry_Output == 0] <- 1
  
  logging::loginfo("Generating A matrix of SoI2SoI Domestic Use table ...")
  SoI2SoI_A <- normalizeIOTransactions(ls[["SoI2SoI"]][, industries],
                                                 SoI_Industry_Output)
  
  logging::loginfo("Generating A matrix of RoUS2SoI Domestic Use table ...")
  RoUS2SoI_A <- normalizeIOTransactions(ls[["RoUS2SoI"]][, industries],
                                                  SoI_Industry_Output)
  
  logging::loginfo("Generating A matrix of SoI2RoUS Domestic Use table ...")
  SoI2RoUS_A <- normalizeIOTransactions(ls[["SoI2RoUS"]][, industries],
                                                  RoUS_Industry_Output)
  
  logging::loginfo("Generating A matrix of RoUS2RoUS Domestic Use table ...")
  RoUS2RoUS_A <- normalizeIOTransactions(ls[["RoUS2RoUS"]][, industries],
                                                   RoUS_Industry_Output)
  
  logging::loginfo("Assembling the complete A matrix ...")
  # Assemble A matrix
  A_top <- cbind(diag(rep(0, length(commodities)*2)),
                 cbind(rbind(SoI2SoI_A, RoUS2SoI_A),
                       rbind(SoI2RoUS_A, RoUS2RoUS_A)))
  colnames(A_top) <- c(1:ncol(A_top))
  A_btm <- cbind(as.matrix(Matrix::bdiag(list(as.matrix(SoI_A),
                                              as.matrix(RoUS_A)))),
                 diag(rep(0, length(industries)*2)))
  A <- as.matrix(rbind(A_top, setNames(A_btm, colnames(A_top))))
  rownames(A) <- paste(c(rep(c(state, "RoUS"), each = length(commodities)),
                         rep(c(state, "RoUS"), each = length(industries))),
                       c(rep(commodities, 2), rep(industries, 2)),
                       c(rep("Commodity", length(commodities)*2),
                         rep("Industry", length(industries)*2)),
                       sep = ".")
  colnames(A) <- rownames(A)
  
  logging::loginfo("Generating the L matrix ...")
  # Calculate L matrix
  I <- diag(nrow(A))
  L <- solve(I - A, tol = 1E-20)
  #----------------- END OF BUILD A, L MATRICES
  logging::loginfo("Calculating y (Final Demand totals) of SoI and RoUS ...")
  # Calculate Final Demand (y)
  FD_columns  <- unlist(sapply(list("HouseholdDemand", "InvestmentDemand",
                                    "ChangeInventories", "Export", "Import",
                                    "GovernmentDemand"),
                               getVectorOfCodes, iolevel = "Summary"))
  ita_column <- ifelse(iolevel == "Detail", "F05100", "F051")
  SoI2SoI_y   <- rowSums(ls[["SoI2SoI"]][, c(FD_columns, ita_column, "ExportResidual")])
  SoI2RoUS_y  <- rowSums(ls[["SoI2RoUS"]][, c(FD_columns, ita_column)])
  RoUS2SoI_y  <- rowSums(ls[["RoUS2SoI"]][, c(FD_columns, ita_column)])
  RoUS2RoUS_y <- rowSums(ls[["RoUS2RoUS"]][, c(FD_columns, ita_column, "ExportResidual")])
  y <- c(SoI2SoI_y + SoI2RoUS_y, RoUS2SoI_y + RoUS2RoUS_y, rep(0, length(industries)*2))
  names(y) <- rownames(L)
  
  logging::loginfo("Validating L*y == commodity and industry output ...")
  # Validate L * y == Output
  # Output = c(SoI_TCO, RoUS_TCO, SoI_TIO, RoUS_TIO)
  output <- c(SoI_Commodity_Output, RoUS_Commodity_Output,
              SoI_Industry_Output, RoUS_Industry_Output)
  validation <- as.data.frame(L %*% y - output)
  colnames(validation) <- "L*y-output"
  
  validation$rel_diff <- validation$`L*y-output`/output
  validation$Ly <- as.numeric(L %*% y)
  validation$output <- output
  validation[validation$output == 1, "rel_diff"] <- 0
  
  logging::loginfo("Validation complete.")
  return(list(A = A, L = L, y = y, Validation = validation))
}

#' Compare commodity or industry output calculated from Make and Use tables.
#' @param model A model list object with model specs and IO tables listed
#' @param output_type A string indicating commodity or industry output.
#' @return A vector of relative difference between commodity or industry output
#' calculated from Supply and Use tables.
compareOutputfromMakeandUse <- function(model, output_type = "Commodity") {
  # Calculate output
  if (output_type == "Commodity") {
    # commodity output
    output_make <- colSums(model$MakeTransactions)
    output_use <- rowSums(model$UseTransactions) + rowSums(model$FinalDemand)
  } else {
    # industry output
    output_make <- rowSums(model$MakeTransactions)
    output_use <- colSums(model$UseTransactions) + colSums(model$UseValueAdded)
  }
  # Compare output
  rel_diff <- (output_make - output_use) / output_use
  rel_diff[is.nan(rel_diff)] <- 0
  return(rel_diff)
}

#' Validate the results of the model build using the Import Factor approach (i.e., coupled model approach)
#' @param model, An EEIO model object with model specs and crosswalk table loaded
#' @param demand, A demand vector, has to be name of a built-in model demand vector, e.g. "Production" or "Consumption". Consumption used as default.
#' @return A calculated direct requirements table
validateImportFactorsApproach <- function(model, demand = "Consumption"){
  if(model$specs$IODataSource == "stateior"){
    location <- model$specs$ModelRegionAcronyms[[1]]
  } else {
    location <- NULL
  }
  # Compute standard final demand
  y <- prepareDemandVectorForStandardResults(model, demand, location = location, use_domestic_requirements = FALSE)
  # Equivalent to as.matrix(rowSums(model$U[1:numCom, (numInd+1):(numInd+numFD)])). Note that both of these include negative values from F050
  
  
  # Retrieve domestic final demand production vector from model$DemandVectors
  y_d  <- prepareDemandVectorForStandardResults(model, demand, location = location, use_domestic_requirements = TRUE)
  # Equivalent to as.matrix(rowSums(model$DomesticFDWithITA[,c(model$FinalDemandMeta$Code_Loc)]))
  
  # Calculate import demand vector y_m.
  y_m <- prepareDemandVectorForImportResults(model, demand, location = location)

  cat("\nTesting that final demand vector is equivalent between standard and coupled model approaches. I.e.: y = y_m + y_d.\n")
  print(all.equal(y, y_d+y_m))

  # Calculate "Standard" economic throughput (x)
  x <- model$L %*% y 
  
  # Calculate economic throughput using coupled model approach
  # Revised equation from RW email (2023-11-01):
  # x_dm <- L_d * Y_d + L*A_m*L_d*Y_d + L*Y_m
  
  x_dm <- (model$L_d %*% y_d) + (model$L %*% model$A_m %*% model$L_d %*% y_d + model$L %*% y_m)
  
  cat("\nTesting that economic throughput is equivalement between standard and coupled model approaches for the given final demand vector.\n") 
  cat("I.e.,: x = x_dm.\n")
  print(all.equal(x, x_dm))
  
  # Calculate "Standard" environmental impacts
  M <- model$B %*% model$L
  LCI <- M %*% y # Equivalent to model$M %*% y,
  
  # Calculate LCI using coupled model approach
  # Revised equation from RW email (2023-11-01):
  # LCI <- (s_d * L_d * Y_d) + (s*L*A_m*L_d*Y_d + s*L*Y_m). I.e., s in RW email is analogous to model$B
  # For validation, model$M = model$M_m, whereas in normally we'd be using model$M_m instead of model$M
  
  LCI_dm <- (model$M_d %*% y_d) + (M %*% model$A_m %*% model$L_d %*% y_d + M %*% y_m)
  
  cat("\nTesting that LCI results are equivalent between standard and coupled model approaches (i.e., LCI = LCI_dm) when\n")
  cat("assuming model$M = model$M_m.\n")
  print(all.equal(LCI, LCI_dm))
  
  # Calculate LCIA using standard approach
  LCIA <- t(model$C %*% M %*% diag(as.vector(y))) #same as result_std_consumption$LCIA_f, above
  colnames(LCIA) <- rownames(model$N_m)
  rownames(LCIA) <- colnames(model$N_m)
  
  # Calculate LCIA using coupled model approach
  y_d <- diag(as.vector(y_d))
  y_m <- diag(as.vector(y_m))
  
  LCI_dm <- (model$M_d %*% y_d) + (M %*% model$A_m %*% model$L_d %*% y_d + M %*% y_m)
  
  LCIA_dm <- t(model$C %*% (LCI_dm))
  colnames(LCIA_dm) <- rownames(model$N_m)
  rownames(LCIA_dm) <- colnames(model$N_m)
  
  cat("\nTesting that LCIA results are equivalent between standard and coupled model approaches (i.e., LCIA = LCIA_dm) when\n")
  cat("assuming model$M = model$M_m.\n")
  all.equal(LCIA_dm, LCIA)
  
}
