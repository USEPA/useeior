#ValidateModel.R


#'Compares the total flows against the model flow totals result calculation with the total demand
#'@param model, EEIOmodel object completely built
#'@param use_domestic, a boolean value indicating whether to use domestic demand vector
#'@param tolerance, a numeric value, tolerance level of the comparison
#'@return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#'@export
compareEandLCIResult <- function(model, use_domestic = FALSE, tolerance = 0.05) {
  #Use L and FinalDemand unless use_domestic, in which case use L_d and DomesticFinalDemand
  #c = diag(L%*%y)
  if (use_domestic) {
    y <- as.matrix(formatDemandVector(rowSums(model$DomesticFinalDemand),model$L_d))
    c <- getScalingVector(model$L_d, y)
  } else {
    y <- as.matrix(formatDemandVector(rowSums(model$FinalDemand),model$L_d))
    c <- getScalingVector(model$L, y)
  }

  CbS_cast <- standardizeandcastSatelliteTable(model$CbS,model)
  B <- as.matrix(CbS_cast)
  Chi <- generateChiMatrix(model, "Industry")
  B_chi <- B*Chi
  
  ##Prepare left side of the equation
  E <- prepareEfromtbs(model)
  E <- as.matrix(E[rownames(B), ])
  
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    #transform E with commodity mix to put in commodity form
    C <- generateCommodityMixMatrix(model)
    E <-  t(C %*% t(E)) 
    #Need to transform B_Chi to be in commodity form
    B_chi <- B_chi %*% model$V_n
  }
  
  #LCI = B dot Chi %*% c   
  LCI <- t(calculateDirectPerspectiveLCI(B_chi, c))
  
  # Calculate relative differences
  rel_diff <- (LCI - E)/E
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  return(validation)
}

#'Compares the total sector output against the model result calculation with the demand vector. and direct perspective.
#'Uses the model$FinalDemand and model$L
#'Works for the domestic model with the equivalent tables
#'@param model, EEIOmodel object completely built
#'@param use_domestic, a boolean value indicating whether to use domestic demand vector
#'@param tolerance, a numeric value, tolerance level of the comparison
#'@return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#'@export
compareOutputandLeontiefXDemand <- function(model, use_domestic=FALSE, tolerance=0.05) {
  if (use_domestic) {
    y <- as.matrix(formatDemandVector(rowSums(model$DomesticFinalDemand),model$L_d))
    c <- getScalingVector(model$L_d, y)
  } else {
    y <- as.matrix(formatDemandVector(rowSums(model$FinalDemand),model$L))
    c <- getScalingVector(model$L, y)
  }
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    #determine if output to compare is commodity or industry
    x <-  model$CommodityOutput
  } else {
    x <- model$IndustryOutput
  }
  # Row names should be identical
  if (!identical(rownames(c), names(x))) {
    stop("Sectors not aligned in model ouput variable and calculation result")
  }
  # Calculate relative differences
  rel_diff <- (c - x)/x
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  return(validation)
}

#'Compares the total commodity output against the summation of model domestic Use and production demand
#'@param model, EEIOmodel object completely built
#'@param tolerance, a numeric value, tolerance level of the comparison
#'@return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#'@export 
compareCommodityOutputandDomesticUseplusProductionDemand <- function(model, tolerance=0.05) {
  p <- model$CommodityOutput
  x <- rowSums(model$DomesticUseTransactions) + model$DemandVectors$vectors[["2012_us_production_complete"]]
  
  #Row names should be identical
  identical(names(p), names(x))
  
  # Calculate relative differences
  rel_diff <- (p - x)/p
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  return(validation)
}

#'Compares the total commodity output multiplied by Market Share matrix and transformed by commodity CPI
#'against the total industry output transformed by industry CPI
#'@param model, EEIOmodel object completely built
#'@param tolerance, a numeric value, tolerance level of the comparison
#'@return A list with pass/fail validation result and the cell-by-cell relative diff matrix
#'@export 
compareCommodityOutputXMarketShareandIndustryOutputwithCPITransformation <- function(model, tolerance=0.05) {
  commodityCPI_ratio <- model$MultiYearCommodityCPI[, "2017"]/model$MultiYearCommodityCPI[, "2012"]
  commodityCPI_ratio[is.na(commodityCPI_ratio)] <- 1
  
  industryCPI_ratio <- model$MultiYearIndustryCPI[, "2017"]/model$MultiYearIndustryCPI[, "2012"]
  industryCPI_ratio[is.na(industryCPI_ratio)] <- 1
  
  q <- model$CommodityOutput * commodityCPI_ratio
  CommodityMix <- generateCommodityMixMatrix(model)
  x <- as.numeric(CommodityMix %*% (model$IndustryOutput * industryCPI_ratio))
  
  # Calculate relative differences
  rel_diff <- (q - x)/x
  
  # Generate Pass/Fail comparison results
  validation <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance)
  return(validation)
}

#'Concatenate all satellite flows in model
#'@param model, EEIOmodel object completely built
prepareEfromtbs <- function(model) {
  E <- standardizeandcastSatelliteTable(model$TbS,model)
  return(E)
}

#' Generate Chi matrix, i.e. ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
#' @param model A completely built EEIOmodel object
#' @param output_type Either Commodity or Industry, default is Commodity
#' @return Chi matrix contains ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
generateChiMatrix <- function(model, output_type = "Commodity") {
  # Extract ModelYearOutput from model based on output_type
  ModelYearOutput <- model[[paste0(output_type, "Output")]]
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
  Chi <- as.matrix(sweep(FlowYearOutput[rownames(model$B), ], 2, ModelYearOutput, "/"))
  # Replace 0 with 1
  Chi[Chi==0] <- 1
  # Rename Chi columns to match B and E
  colnames(Chi) <- apply(data.frame(colnames(Chi), model$specs$PrimaryRegionAcronym),
                         1, FUN = joinStringswithSlashes)
  return(Chi)
}

#' Gets industry output from model Use and Make and checks if they are the same
#' @param model, a built model object
compareIndustryOutputinMakeandUse <- function(model) {
  # Calculate Industry Output (x) from Make and Use tables
  x_make <-rowSums(model$MakeTransactions)
  x_use <- colSums(model$UseTransactions) + colSums(model$UseValueAdded)
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
  return(validation)
}

#' Validate result based on specified tolerance
#' @param result A data object to be validated
#' @param abs_diff A boolean value indicating whether to validate absolute values
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
#' @param failure A boolean value indicating whether to report failure or not
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
#' @param df A data.frame to be validated
#' @param abs_diff A boolean value indicating whether to validate absolute values
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