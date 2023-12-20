#' Obtain specs that indicate which sectors need to 
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified MUIO specs.
getMUIOSectors <- function (model, configpaths = NULL){
  
  MUIOSectors <- data.frame()
  if (is.null(model$DisaggregationSpecs)) {
    model$MUIOSpecs <- vector(mode='list')
    for (configFile in model$specs$MUIOSpecs){
      logging::loginfo(paste0("Loading MUIO specification file for ", configFile, "..."))
      config <- getConfiguration(configFile, "muio", configpaths)
      
      if('MUIO' %in% names(config)){
        model$MUIOSpecs <- append(model$MUIOSpecs, config$MUIO)
      }
    }
    model <- disaggregateSetup(model, configpaths, "MUIO")
    
    for(muio in model$MUIOSpecs){
      MUIOSectors <- rbind(MUIOSectors, subset(muio$NAICSSectorCW, muio$NAICSSectorCW$SectorType == "MUIO"))
    }
    
  } else {
    for(disagg in model$DisaggregationSpecs){
      MUIOSectors <- rbind(MUIOSectors, subset(disagg$NAICSSectorCW, disagg$NAICSSectorCW$SectorType == "MUIO"))
    }
  }
  
  model$MUIOSectors <- MUIOSectors
  return(model)
}


#' Reorder physical sectors such that they are in the top left of the Make and Use MUIO tables, as well as reorder all the relevant model objects.
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param physComIndeces Indeces that specify the location of the physical MUIO commodities in the commodity lists
#' @param physIndIndeces Indeces that specify the location of the physical MUIO industries in the industry lists
#' @param econComIndeces Indeces that specify the location of the economic MUIO commodities (i.e., the "Other" sectors, X in code) in the commodity lists
#' @param econIndIndeces Indeces that specify the location of the economic MUIO industries (i.e., the "Other" sectors, X in code) in the industry lists
#' @return A model with the specified MUIO sectors in physical units and rearranged sector orders (optional)
reorderPhysicalSectors <- function (model, physComIndeces, physIndIndeces, econComIndeces, econIndIndeces){
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
  
  model <- reorderModelSectors(model, comOrder, indOrder)

  
  return(model)
}

#' Convert MUIO sectors to physical units
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified MUIO specs.
convertSectorsToPhysical <- function (model, configpaths = NULL){
  
  logging::loginfo("Converting select sectors to physical units...")
  # Get the sectors we want to convert to physical, as well as the "other" sector that will house the off-diagonal values from the original sector
  physicalMUIOSectors <- subset(model$MUIOSectors, model$MUIOSectors$Unit != "USD")
  economicMUIOSectors <- subset(model$MUIOSectors, model$MUIOSectors$Unit == "USD")
  
  # Resort to match model
  physicalMUIOSectors <- physicalMUIOSectors[order(match(physicalMUIOSectors$USEEIO_Code, model$Commodities$Code_Loc)),]
  
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
  # Place the entire production of the physical sectors in the intersection of the make table
  physSum <- rowSums(model$UseTransactions[physComIndeces,]) + rowSums(model$FinalDemand[physComIndeces,])
  
  # if replacing only one row, can't call diag function
  if(length(physSum) == 1){
    model$MakeTransactions[physIndIndeces, physComIndeces] <- physSum
  } else{
    model$MakeTransactions[physIndIndeces, physComIndeces] <- diag(physSum)
  }
  
  # Calculate new industry and commodity totals 
  # TODO: Replace this with a call to calculateWIOOutputs? Rename that function?
  model$IndustryOutput <- rowSums(model$MakeTransactions)
  model$CommodityOutput <- rowSums(model$UseTransactions) + rowSums(model$FinalDemand)
  
  # Replace Unit column in model$Industries and model$Commodities objects with appropriate unit
  model$Commodities$Unit[physComIndeces] <- physicalMUIOSectors$Unit
  model$Industries$Unit[physIndIndeces] <- physicalMUIOSectors$Unit
  
  # Adjust MultiYear Objects for MUIO
  model <- adjustMultiYearObjectsForMUIO(model, physComIndeces, physIndIndeces, econComIndeces, econIndIndeces)
  
  # Move physical sectors to top-left quadrant of tables, followed by non-physical MUIO sectors
  model <- reorderPhysicalSectors(model, physComIndeces, physIndIndeces, econComIndeces, econIndIndeces)
  
  return(model)
  
}

#' Adjust MultiYearObjects in MUIO models such that they have the same dimensions as the other model objects
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param physComIndeces Indeces that specify the location of the physical MUIO commodities in the commodity lists
#' @param physIndIndeces Indeces that specify the location of the physical MUIO industries in the industry lists
#' @param econComIndeces Indeces that specify the location of the economic MUIO commodities (i.e., the "Other" sectors, X in code) in the commodity lists
#' @param econIndIndeces Indeces that specify the location of the economic MUIO industries (i.e., the "Other" sectors, X in code) in the industry lists
#' @return A model with the multiyear objects adjusted for the MUIO industries/commodities.
adjustMultiYearObjectsForMUIO <- function(model, physComIndeces, physIndIndeces, econComIndeces, econIndIndeces){
  
  # Adjust MultiYearCommodity/Indistry Output values for MUIO
  # Assumes constant physical totals and economic totals for MUIO sectors across years, similar to WIO
  
  model$MultiYearIndustryOutput[physIndIndeces,] <- rep(model$IndustryOutput[physIndIndeces], ncol(model$MultiYearIndustryOutput))
  model$MultiYearIndustryOutput[econIndIndeces,] <- rep(model$IndustryOutput[econIndIndeces], ncol(model$MultiYearIndustryOutput))
  
  model$MultiYearCommodityOutput[physComIndeces,] <- rep(model$CommodityOutput[physComIndeces], ncol(model$MultiYearCommodityOutput))
  model$MultiYearCommodityOutput[econComIndeces,] <- rep(model$CommodityOutput[econComIndeces], ncol(model$MultiYearCommodityOutput))
  
  #Adjust MultiYearCPI values for MUIO
  #Assumes constant CPI for MUIO sectors across years, similar to WIO
  model$MultiYearIndustryCPI[physIndIndeces,] <- rep(100, ncol(model$MultiYearIndustryCPI))
  model$MultiYearIndustryCPI[econIndIndeces,] <- rep(100, ncol(model$MultiYearIndustryCPI))
  
  model$MultiYearCommodityCPI[physComIndeces,] <- rep(100, ncol(model$MultiYearCommodityCPI))
  model$MultiYearCommodityCPI[econComIndeces,] <- rep(100, ncol(model$MultiYearCommodityCPI))

  return(model)
}
