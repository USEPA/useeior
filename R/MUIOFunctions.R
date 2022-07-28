#' Obtain specs that indicate which sectors need to 
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified WIO specs.
getMUIOSectors <- function (model, configpaths = NULL){
  
  # model$WIOSpecs <- vector(mode='list')
  # 
  # for (configFile in model$specs$WIOSpecs){
  #   logging::loginfo(paste0("Loading WIO specification file for ", configFile, "..."))
  #   config <- getConfiguration(configFile, "WIO", configpaths)
  #   
  #   if('WIO' %in% names(config)){
  #     model$WIOSpecs <- append(model$WIOSpecs, config$WIO)
  #   }
  # }
  MUIOSectors <- data.frame()
  if(is.null(model$DisaggregationSpecs))
  {
    # TODO: read in file with MUIO specs that are not included in the disaggregation_Env
  } else{
    for(disagg in model$DisaggregationSpecs){
      MUIOSectors <- rbind(MUIOSectors, subset(disagg$NAICSSectorCW, disagg$NAICSSectorCW$SectorType == "MUIO"))
    }
  }
  
  model$MUIOSectors <- MUIOSectors
  return(model)
}


#' Convert MUIO sectors to physical units
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified WIO specs.
convertSectorsToPhysical <- function (model, configpaths = NULL){
  
  temp <- 1
  # Get the sectors we want to convert to physical, as well as the "other" sector that will house the off-diagonal values from the original sector
  physicalMUIOSectors <- subset(model$MUIOSectors, model$MUIOSectors$Unit != "USD")
  economicMUIOSectors <- subset(model$MUIOSectors, model$MUIOSectors$Unit == "USD")
  
  # Get indeces for physical industries and commodities in the Make and Use tables
  physComIndeces <- which(model$Commodities$Code_Loc %in% physicalMUIOSectors$USEEIO_Code)
  physIndIndeces <- which(model$Industries$Code_Loc %in% physicalMUIOSectors$USEEIO_Code)
  econComIndeces <- which(model$Commodities$Code_Loc %in% economicMUIOSectors$USEEIO_Code)
  econIndIndeces <- which(model$Industries$Code_Loc %in% economicMUIOSectors$USEEIO_Code)
  
  # Convert to physical units using the price
  model$UseTransactions[physComIndeces, ] <- model$UseTransactions[physComIndeces, ]*(1/physicalMUIOSectors$Price)
  model$FinalDemand[physComIndeces, ] <- model$FinalDemand[physComIndeces, ]*(1/physicalMUIOSectors$Price)
  
  model$DomesticUseTransactions[physComIndeces, ] <- model$DomesticUseTransactions[physComIndeces, ]*(1/physicalMUIOSectors$Price)
  model$DomesticFinalDemand[physComIndeces, ] <- model$DomesticFinalDemand[physComIndeces, ]*(1/physicalMUIOSectors$Price)
  
  # Replace all values in make transactions with 0s except the intersections
  model$MakeTransactions[physIndIndeces,] <- 0 
  model$MakeTransactions[, physComIndeces] <- 0 
  model$MakeTransactions[physIndIndeces, physComIndeces] <- diag(rowSums(model$UseTransactions[physComIndeces,]) # Place the entire production of the physical sectors in the intersection of the make table
                                                                 + rowSums(model$FinalDemand[physComIndeces,]))
  
  # Move physical sectors to top-left quadrant of tables, followed by non-physical MUIO sectors

  # Reorder commodities
  MUIOComOrder <- c(physComIndeces,econComIndeces) # Create an int vector with the physical MUIO commodity indeces followed by the economic MUIO indices
  comOrder <- 1:dim(model$Commodities)[1]  # Create an ordered int vector from 1 to number of commodities
  comOrder <- comOrder[-(MUIOComOrder)] # Remove ints corresponding to the MUIO commodity indeces from their proper order
  comOrder <- c(MUIOComOrder, comOrder) # Add ints corresponding to the MUIO commodity indeces to the beginning of the list
  
  # Reorder industries
  MUIOIndOrder <- c(physIndIndeces, econIndIndeces)
  indOrder <- 1:dim(model$Industries)[1]
  indOrder <- indOrder[-(MUIOIndOrder)]
  indOrder <- c(MUIOIndOrder, indOrder)
  
  # Rearrange relevant model objects
  model$UseTransactions <- model$UseTransactions[comOrder, indOrder]
  model$DomesticUseTransactions <- model$UseTransactions[comOrder, indOrder]
  model$FinalDemand <- model$FinalDemand[comOrder, ]
  model$DomesticFinalDemand <- model$DomesticFinalDemand[comOrder, ]
  model$UseValueAdded <- model$UseValueAdded[, indOrder]
  model$MakeTransactions <- model$MakeTransactions[indOrder, comOrder]
  model$Commodities <- model$Commodities[comOrder, ]
  model$Industries <- model$Industries[indOrder, ]
  model$InternationalTradeAdjustment <- model$InternationalTradeAdjustment[comOrder]
  model$MultiYearCommodityOutput <- model$MultiYearCommodityOutput[comOrder,]
  model$MultiYearIndustryOutput <- model$MultiYearIndustryOutput[indOrder,]
  model$MultiYearCommodityCPI <- model$MultiYearCommodityCPI[comOrder,]
  model$MultiYearIndustryCPI <- model$MultiYearIndustryCPI[indOrder,]
  
  # Calculate new industry and commodity totals 
  # TODO: Replace this with a call to calculateWIOOutputs? Rename that function?
  model$IndustryOutput <- rowSums(model$MakeTransactions)
  model$CommodityOutput <- rowSums(model$UseTransactions) + rowSums(model$FinalDemand)
  
  # Replace only multi year outputs for the curret model year?
  multiYearComOutputIndex <- which(colnames(model$MultiYearCommodityOutput) == model$specs$IOYear)
  multiYearIndOutputIndex <- which(colnames(model$MultiYearIndustryOutput) == model$specs$IOYear)
  model$MultiYearCommodityOutput[,multiYearComOutputIndex] == model$CommodityOutput
  model$MultiYearIndustryOutput[,multiYearIndOutputIndex] == model$IndustryOutput
  
  temp <- 2
  
  return(model)
  
}