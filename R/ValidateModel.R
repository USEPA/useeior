#ValidateModel.R


#'Compares the total flows against the model flow totals result calculation with the total demand
#'@param model, EEIOmodel object completely built
compareEandLCIResult <- function(model,output_type,use_domestic=FALSE, tolerance=0.05) {
  
  #Use L and FinalDemand unless use_domestic, in which case use L_d and DomesticFinalDemand
  #c = diag(L%*%y)
  if (use_domestic) {
    y <- as.matrix(formatDemandVector(rowSums(model$DomesticFinalDemand),model$L_d))
    c <- getScalingVector(model$L_d, y)
  } else {
    y <- as.matrix(formatDemandVector(rowSums(model$FinalDemand),model$L_d))
    c <- getScalingVector(model$L, y)
  }

  if (model$specs$CommoditybyIndustryType=="Commodity") {

    if (output_type=="Commodity") {
      B <- model$B 
      
    } else {
      #industry approach
      B <- createBfromFlowDataandOutput(model)
      B <- as.matrix(CbS_cast)
    } 

    
    ##Prepare left side of the equation
    E <- prepareEfromtbs(model)
    E <- as.matrix(E[rownames(B), ])
        #transform E with commodity mix
    C <- generateCommodityMixMatrix(model)
    E <-  t(C %*% t(E))  
    
  } else {
    stop("This function cannot yet handle industry type models")
  }

  #Prepare calculation
  #LCI = B dot Chi %*% c 
  Chi <- generateChiMatrix(model, output_type)
  B_chi <- B*Chi
  LCI <- t(calculateDirectPerspectiveLCI(B_chi, c))

  rel_diff <- (LCI - E)/E
  return(rel_diff)
}

#'Compares the total sector output against the model result calculation with the demand vector. and direct perspective.
#'Uses the model$FinalDemand and model$L
#'Works for the domestic model with the equivalent tables
#'@param model, EEIOmodel object completely built
#'@return vector, a vector of relative different in calculation from sector output by sector 
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
  rel_diff <- (c - x)/x
  return(rel_diff)
}

#'Compares the total commodity output against the summation of model domestic Use and production demand
#'@param model, EEIOmodel object completely built
#'@return vector, a vector of relative different in calculation from sector output by sector 
compareCommodityOutputandDomesticUseplusProductionDemand <- function(model, tolerance=0.05) {
  p <- model$CommodityOutput
  x <- rowSums(model$DomesticUseTransactions) + model$DemandVectors$vectors[["2012_us_production_complete"]]
  
  #Row names should be identical
  identical(names(p), names(x))
  
  rel_diff <- (p - x)/p
  return(rel_diff)
}


#'Concatenate all satellite flows in model
#'@param model, EEIOmodel object completely built
prepareEfromtbs <- function(model) {
  df <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  df$Flow <- apply(df[, c("Flowable", "Context", "Unit")], 1, FUN = joinStringswithSlashes)
  E <- standardizeandcastSatelliteTable(df,model)
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
  for (flow in unique(TbS$Flow)) {
    output <- as.data.frame(model[[paste0("MultiYear", output_type, "Output")]])[, FALSE]
    #output <- as.data.frame(model[["MultiYearIndustryOutput"]])[, FALSE]
    # Determine sector-year combination
    sector_year <- unique(cbind.data.frame(TbS[TbS$Flow==flow, "Sector"], TbS[TbS$Flow==flow, "Year"], stringsAsFactors = FALSE))
    colnames(sector_year) <- c("Sector", "Year")
    sector_year <- na.omit(sector_year)
    if (nrow(sector_year)>0) {
      # Get output for each sector-year combination
      for (i in 1:nrow(sector_year)) {
        sector <- sector_year[i, "Sector"]
        year <- sector_year[i, "Year"]
        output[sector, as.character(year)] <- model[[paste0("MultiYear", output_type, "Output")]][sector, as.character(year)]
        #output[sector, as.character(year)] <- model[["MultiYearIndustryOutput"]][sector, as.character(year)]
        # Adjust output to model year $ by CPI
        CPI_df <- model[[paste0("MultiYear", output_type, "CPI")]][sector, as.character(c(model$specs$IOYear, year))]
        #CPI_df <- model[["MultiYearIndustryCPI"]][sector, as.character(c(model$specs$IOYear, year))]
        DollarRatio <- CPI_df[, as.character(model$specs$IOYear)]/CPI_df[, as.character(year)]
        # Replace NA with 1 in DollarRatio
        DollarRatio[is.na(DollarRatio)] <- 1
        output[sector, as.character(year)] <- output[sector, as.character(year)] * DollarRatio
      }
    } else {
      output[, 1] <- 0
    }
    # Average over the years to get FlowYearOutput_f
    FlowYearOutput_f <- as.data.frame(t(rowMeans(output, na.rm = TRUE)))[, rownames(model[[paste0("MultiYear", output_type, "Output")]])]
    # Replace NaN with model year industry output
    for (column in colnames(FlowYearOutput_f)) {
      if (FlowYearOutput_f[, column]=="NaN") {
        FlowYearOutput_f[, column] <- ModelYearOutput[column]
      }
    }
    rownames(FlowYearOutput_f) <- flow
    # if (output_type=="Commodity") {
    #   # Use CommodityMix to transform IndustryOutput to CommodityOutput
    #   CommodityMix <- generateCommodityMixMatrix(model)
    #   FlowYearOutput_f[flow, ] <- as.numeric(CommodityMix %*% as.numeric(FlowYearOutput_f[flow, ]))
    # }
    FlowYearOutput <- rbind(FlowYearOutput, FlowYearOutput_f)
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
  x_make <- x_make[order(model$Industries)]
  x_use <- x_use[order(model$Industries)]
  # Check if x_make and x_use have the same industry order
  if (!identical(names(x_make), names(x_use))) {
    stop("industries in Make and Use do not match")
  }
  # Calculate relative differences in x_make and x_use
  rel_diff <- (x_use - x_make)/x_make
  return(rel_diff)
}

