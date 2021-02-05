#ValidateModel.R


#'Compares the total flows against the model result calculation with the domestic demand vector and direct perspective
#'@param model, EEIOmodel object completely built
compareEandDomesticLCIResult <- function(model, tolerance=0.05) {
  E <- prepareEfromtbs(model)
  result_domestic <- calculateEEIOModel(model, "DIRECT", demand = "Production", use_domestic = TRUE)
  LCI <- t(result_domestic$LCI_d)
  #Converting here to a df changed the column order
  #LCI <- data.frame(colnames(LCI)=LCI)
  #clean up
  rm(result_domestic)
  
  inboth <- intersect(row.names(E),row.names(LCI))
  #list_E_LCI <- harmonizeDFsbyrowname(E,LCI)
  E <- E[inboth,]
  LCI <- LCI[inboth,]
  
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    #transform E by market shares
    E_c <-  as.matrix(E) %*% model$V_n
  }
  
  #Adjust LCI with Chi
  Chi <- generateChiMatrix(model)
  #Dot multiply LCI and Chi
  LCI_a <- LCI*Chi
  
  diff <- abs(LCI_a - E_c)/E_c
  #rule <- validate::validator(abs(LCI_a - E_c)/E_c <= tolerance)
  #confrontation <- validate::confront(LCI_a, rule, E_c)
  #confrontation <- validate::as.data.frame(confrontation)
  #validation <- merge(confrontation, validate::as.data.frame(rule))
  #return(validation)
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
  Chi <- as.matrix(1/sweep(FlowYearOutput[rownames(model$B), ], 2,
                           ModelYearOutput[colnames(FlowYearOutput), ], "/"))
  # Replace NA with 0
  Chi[is.na(Chi)] <- 0
  # Rename Chi columns to match B and E
  colnames(Chi) <- apply(data.frame(colnames(Chi), model$specs$PrimaryRegionAcronym),
                         1, FUN = joinStringswithSlashes)
  return(Chi)
}


