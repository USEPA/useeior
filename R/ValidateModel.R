#ValidateModel.R


#'Compares the total flows against the model result calculation with the domestic demand vector and direct perspective
#'@param model, EEIOmodel object completely built
compareEandDomesticLCIResult <- function(model, tolerance=0.05) {
  
  ##Prepare right side of the equation
  #Adjust B with Chi
  Chi <- generateChiMatrix(model)
  B_chi <- model$B*Chi 
  #Prepare calculation
  #LCI = B_chi diag(L_d y)
  y <- as.matrix(formatDemandVector(model$DemandVectors$vectors[["2012_us_production_complete"]],model$L_d))
  c <- getScalingVector(model$L_d, y)
  LCI <- t(calculateDirectPerspectiveLCI(B_chi, c))
  
  ##Prepare left side of the equation
  E <- prepareEfromtbs(model)
  E <- as.matrix(E[rownames(LCI),])
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    #transform E by market shares
    E <-  E %*% model$V_n
  }
  
  rel_diff <- (LCI - E)/E
  #rule <- validate::validator(abs(LCI_a - E_c)/E_c <= tolerance)
  #confrontation <- validate::confront(LCI_a, rule, E_c)
  #confrontation <- validate::as.data.frame(confrontation)
  #validation <- merge(confrontation, validate::as.data.frame(rule))
  #return(validation)
  return(rel_diff)
}

#'Compares the total sector output against the model result calculation with the domestic demand vector and direct perspective
#'@param model, EEIOmodel object completely built
#'@return vector, a vector of relative different in calculation from sector output by sector 
compareOutputandDomesticResult <- function(model, tolerance=0.05) {
  y <- as.matrix(formatDemandVector(model$DemandVectors$vectors[["2012_us_production_complete"]],model$L_d))
  c <- getScalingVector(model$L_d, y)
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    #transform E by market shares
    x <-  model$CommodityOutput
  } else {
    x <- model$IndustryOutput
  }
  
  #Row names should be identical
  identical(rownames(c),rownames(x))
  
  rel_diff <- (c - x)/x
  return(rel_diff)
}


#'Concatenate all satellite flows in model
#'@param model, EEIOmodel object completely built
prepareEfromtbs <- function(model) {
  df <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  E <- standardizeandcastSatelliteTable(df,model)
  return(E)
}

#' Generate Chi matrix, i.e. ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
#' @param model A completely built EEIOmodel object
#' @param output_type Either Commodity or Industry, default is Commodity
#' @return Chi matrix contains ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
generateChiMatrix <- function(model, output_type = "Commodity") {
  # Generate ModelYearOutput based on output_type and model Commodity/Industry type 
  if (output_type=="Commodity") {
    if (model$specs$CommoditybyIndustryType=="Industry") {
      ModelYearOutput <- generateCommodityOutputforYear(model)
    } else {
      ModelYearOutput <- model$CommodityOutput
    }
  } else {
    if (model$specs$CommoditybyIndustryType=="Commodity") {
      ModelYearOutput <- model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE]
    } else {
      ModelYearOutput <- model$IndustryOutput
    }
  }
  # Generate FlowYearOutput
  TbS <- do.call(rbind, model$SatelliteTables$totals_by_sector)
  TbS[, "Flow"] <- apply(TbS[, c("Flowable", "Context", "Unit")], 1, FUN = joinStringswithSlashes)
  FlowYearOutput <- data.frame()
  for (year in unique(TbS$Year)) {
    if (model$specs$ModelRegionAcronyms=="RoUS") {
      IsRoUS <- TRUE
    } else {
      IsRoUS <- FALSE
    }
    # Generate industry output by year
    IndustryOutputVector <- model$GDP$BEAGrossOutputIO[model$Industries, as.character(year)]
    # Adjust industry output to model year $
    DollarRatio <- model$GDP$BEACPIIO[model$Industries, as.character(model$specs$IOYear)]/model$GDP$BEACPIIO[model$Industries, as.character(year)]
    IndustryOutputVector <- IndustryOutputVector * DollarRatio
    # Generate a commodity x industry CommodityMix matrix
    CommodityMix <- generateCommodityMixMatrix(model)
    # Use CommodityMix to transform IndustryOutput to CommodityOutput
    CommodityOutput <- as.data.frame(CommodityMix %*% IndustryOutputVector)
    flows <- unique(TbS[TbS$Year==year, "Flow"])
    FlowYearOutput_y <- do.call(rbind.data.frame, rep(CommodityOutput, times = length(flows)))
    rownames(FlowYearOutput_y) <- flows
    colnames(FlowYearOutput_y) <- rownames(CommodityOutput)
    FlowYearOutput <- rbind(FlowYearOutput, FlowYearOutput_y)
  }
  # Calculate Chi: divide ModelYearOutput by FlowYearOutput
  Chi <- as.matrix(sweep(FlowYearOutput[rownames(model$B), ], 2,
                           ModelYearOutput[colnames(FlowYearOutput), ], "/"))
  # Replace NA with 0
  Chi[is.na(Chi)] <- 0
  # Rename Chi columns to match B and E
  colnames(Chi) <- apply(data.frame(colnames(Chi), model$specs$PrimaryRegionAcronym),
                         1, FUN = joinStringswithSlashes)
  return(Chi)
}


