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

#'Compares the total sector output against the model result calculation with the demand vector. and direct perspective.
#'Uses the model$FinalDemand and model$L
#'Works for the domestic model with the equivalent tables
#'@param model, EEIOmodel object completely built
#'@return vector, a vector of relative different in calculation from sector output by sector 
compareOutputandLeontiefXDemand <- function(model, domestic=FALSE, tolerance=0.05) {
  if (domestic) {
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
  identical(rownames(p), names(x))
  
  rel_diff <- (p - x)/p
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
    ModelYearOutput <- model$CommodityOutput
  } else {
    ModelYearOutput <- model$IndustryOutput
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
    IndustryOutput <- model$GDP$BEAGrossOutputIO[model$Industries, as.character(year)]
    # Adjust industry output to model year $
    DollarRatio <- model$GDP$BEACPIIO[model$Industries, as.character(model$specs$IOYear)]/model$GDP$BEACPIIO[model$Industries, as.character(year)]
    IndustryOutput <- IndustryOutput * DollarRatio
    # Generate a commodity x industry CommodityMix matrix
    CommodityMix <- generateCommodityMixMatrix(model)
    # Use CommodityMix to transform IndustryOutput to CommodityOutput
    CommodityOutput <- as.data.frame(CommodityMix %*% IndustryOutput)
    flows <- unique(TbS[TbS$Year==year, "Flow"])
    FlowYearOutput_y <- do.call(rbind.data.frame, rep(CommodityOutput, times = length(flows)))
    rownames(FlowYearOutput_y) <- flows
    colnames(FlowYearOutput_y) <- rownames(CommodityOutput)
    FlowYearOutput <- rbind(FlowYearOutput, FlowYearOutput_y)
  }
  # Calculate Chi: divide ModelYearOutput by FlowYearOutput
  Chi <- as.matrix(sweep(FlowYearOutput[rownames(model$B), ], 2, ModelYearOutput, "/"))
  # Replace NA with 0
  Chi[is.na(Chi)] <- 0
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

