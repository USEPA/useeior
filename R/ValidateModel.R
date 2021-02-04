#ValidateModel.R


#'Compares the total flows against the model result calculation with the domestic demand vector and direct perspective
#'@param model, EEIOmodel object completely built
compareEandDomesticLCIResult <- function(model, tolerance=0.05) {
  E <- prepareEfromtbs(model)
  result_domestic <- calculateEEIOModel(model, "DIRECT", demand = "Production", use_domestic = TRUE)
  LCI <- data.frame(t(result_domestic$LCI_d))
  #clean up
  rm(result_domestic)
  
  list_E_LCI <- harmonizeDFsbyrowname(E,LCI)
  E <- list_E_LCI[[1]]
  LCI <- list_E_LCI[[1]]
  
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    #transform E by market shares
    Ux_hat <- generateMarketSharesfromMake(model)
    E <- as.matrix(E)
    E <-  E %*% Ux_hat
    E <- data.frame(E)
  }
  
  #Adjust LCI with Chi
  Chi <- generateChiMatrix(model)
  #Dot multiply LCI and Chi
  LCI_a <- LCI*Chi
  
  rule <- validate::validator(abs(LCI_a - E)/E <= tolerance)
  confrontation <- validate::confront(LCI_a, rule, E)
  confrontation <- validate::as.data.frame(confrontation)
  validation <- merge(confrontation, validate::as.data.frame(rule))
  return(validation)
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
    IndustryOutputVector <- model$GDP$BEAGrossOutputIO[, as.character(year)]
    # Adjust industry output to model year $
    DollarRatio <- model$GDP$BEACPIIO[, as.character(model$specs$IOYear)]/model$GDP$BEACPIIO[, as.character(year)]
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
  Chi <- as.matrix(1/sweep(FlowYearOutput[rownames(model$B), ], 2,
                           ModelYearOutput[colnames(FlowYearOutput), ], "/"))
  colnames(Chi) <- apply(data.frame(colnames(Chi), model$specs$PrimaryRegionAcronym),
                         1, FUN = joinStringswithSlashes)
  return(Chi)
}


