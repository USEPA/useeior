#' Aggregate a model based on specified source file
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return An aggregated model.
aggregateModel <- function (model){

  logging::loginfo("Initializing Aggregation of IO tables...")
  for (aggSpec in model$AggregationSpecs){
    #aggregating economic tables
    model$MakeTransactions <- aggregateMakeTable(model, aggSpec)
    model$UseTransactions <- aggregateUseTable(model, aggSpec)
    model$DomesticUseTransactions <- aggregateUseTable(model, aggSpec, domestic = TRUE)
    model$UseValueAdded <- aggregateVA(model, aggSpec)
    #model$FinalDemand <- aggregateFD(model, aggSpec) #todo
    #model$DomesticFinalDemand <- aggregateFD(model, aggSpec) #todo
    #model$MarginSectors <- aggregateMarginSectors(model, aggSpec) #todo
    #model$Margins <- aggregateMargins(model, aggSpec)
    
    #model$ValueAddedMeta <- aggregateVAMeta(model, aggSpec) #todo
    #model$FinalDemandMeta <- aggregateFDMeta(model, aggSpec) #todo
    
    #aggregating Crosswalk
    model$crosswalk <- aggregateMasterCrosswalk(model, aggSpec)
    
    #obtaining indeces to aggregate sectors in remaining model objects
    agg <- aggSpec$Sectors
    
    mainComIndex <- getIndex(model$Commodities$Code_Loc, agg[1])#first item in Aggregation is the sector to aggregate to, not to be removed
    mainIndIndex <- getIndex(model$Industries$Code_Loc, agg[1])
    comIndecesToAggregate <- which(model$Commodities$Code_Loc %in% agg[-1]) #find com indeces containing references to the sectors to be aggregated
    indIndecesToAggregate <- which(model$Industries$Code_Loc %in% agg[-1]) #find ind indeces containing references to the sectors to be aggregated
    
    #aggregating (i.e. removing) sectors from model lists
    #aggregate Industry lists
    if(length(indIndecesToAggregate)!=0){
      
      model$Industries <- removeRowsFromList(model$Industries, indIndecesToAggregate)
      model$MultiYearIndustryCPI <- aggregateMultiYearCPI(model, mainIndIndex, indIndecesToAggregate, "Industry")
      model$MultiYearIndustryOutput <- aggregateMultiYearOutput(model$MultiYearIndustryOutput, mainIndIndex, indIndecesToAggregate)
    }
    
    #aggregate Commodity lists
    if(length(comIndecesToAggregate !=0)){
      model$Commodities <- removeRowsFromList(model$Commodities, comIndecesToAggregate)
      model$MultiYearCommodityCPI <- aggregateMultiYearCPI(model, mainIndIndex, indIndecesToAggregate, "Commodity")
      model$MultiYearIndustryOutput <- aggregateMultiYearOutput(model$MultiYearIndustryOutput, mainComIndex, comIndecesToAggregate)
      #model$ImportCosts <- aggregateImportCosts(model$Commodities, comIndecesToAggregate) #todo
    }
    
    model <- calculateIndustryCommodityOutput(model)

  }
  
  return(model)
}


#' Obtain aggregation specs from input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of agg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified aggregation and disaggregation specs.
getAggregationSpecs <- function (model, configpaths = NULL){

  model$AggregationSpecs <- vector(mode = 'list')
  
  for (configFile in model$specs$AggregationSpecs){
    logging::loginfo(paste0("Loading aggregation specification file for ", configFile, "..."))
    config <- getConfiguration(configFile, "agg", configpaths)
    if('Aggregation' %in% names(config)){
      model$AggregationSpecs <- append(model$AggregationSpecs, config$Aggregation)
    }
  
  }

  return(model)
}

#' Aggregate satellite tables from static file based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param aggregationSpecs Specifications for aggregation
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param sat The abbreviation for the satellite table.
#' @return A standardized satellite table with aggregated sectors added.
aggregateSectorsinTBS <- function (model, aggregationSpecs, sattable, sat){
  
  newSatTable <- sattable
  agg <- aggregationSpecs$Sectors
  
  #variable to determine length of Code substring, i.e., code length minus geographic identifier and separator character (e.g. "/US")
  codeLength <- nchar(gsub("/.*", "", agg[1]))
  aggCodes <- substr(agg,1,codeLength)
  
  if(any(newSatTable$Sector %in% aggCodes[-1])) {
    newSatTable$Sector[which(newSatTable$Sector %in% aggCodes[-1])] <- aggCodes[1]
    newSatTable <- collapseTBS(newSatTable, model)
  }
  
  return(newSatTable)
}

#' Aggregate MultiYear CPI model objects
#' @param model An EEIO model object with model specs and IO tables loaded.
#' @param mainIndex Index to aggregate the others to.
#' @param indecesToAggregate List of indeces to aggregate.
#' @param type String to designate either commodity or industry
#' @return newCPI A dataframe with the aggregatded CPI values by year.
aggregateMultiYearCPI <- function(model, mainIndex, indecesToAggregate, type){
  
  if(type == "Industry"){
    originalCPI <- model$MultiYearIndustryCPI
    originalOutput <- model$MultiYearIndustryOutput
    
  } else {
    originalCPI <- model$MultiYearCommodityCPI  
    originalOutput <- model$MultiYearCommodityOutput
  }
  
  newCPI <- originalCPI
  
  aggOutputs <- originalOutput[mainIndex,] + colSums(originalOutput[indecesToAggregate,])#get aggregated total for all relevant indeces (denominator for ratios)
  
  mainIndexOutputRatios <-  originalOutput[mainIndex,] / aggOutputs #get ratio of sector to be aggregated to
  
  aggIndecesOutputRatios <- sweep(as.matrix(originalOutput[indecesToAggregate,]),2, as.matrix(aggOutputs), "/") #get ratios of sectors to be aggregated (removed)
  
  aggIndecesOutputRatios <- data.frame(aggIndecesOutputRatios)#convert back to df.
  colnames(aggIndecesOutputRatios) <- colnames(mainIndexOutputRatios)
  
  newCPI[mainIndex,] <- newCPI[mainIndex,]*mainIndexOutputRatios + colSums(newCPI[indecesToAggregate,]*aggIndecesOutputRatios)#calculating weighted average for main CPI row
  
  newCPI <- removeRowsFromList(newCPI, indecesToAggregate)
  
  return(newCPI)
  
}


#TODO: rewrite this function to use matrix calculations when possible
#' Aggregate the MakeTable based on specified source file
#' @param model An EEIO model object with model specs and IO tables loaded.
#' @param aggregationSpecs Specifications for aggregation
#' @return An aggregated MakeTable.
aggregateMakeTable <- function(model, aggregationSpecs){
  
  agg <- aggregationSpecs$Sector

  count <- 1
  

  for (sector in agg){
    
    if(count == 1){
      count <- count + 1
      next #first sector in agg is the one we are aggregating to, so skip
    } 
    
    model$MakeTransactions <- aggregateSector(model, agg[1], agg[count], "Make")
    count <- count + 1
  }
  
  #remove rows and cols from model
  agg <- agg[-1]
  model$MakeTransactions <- model$MakeTransactions[!(rownames(model$MakeTransactions)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
  model$MakeTransactions <- model$MakeTransactions[,!(colnames(model$MakeTransactions)) %in% agg] #as above but with cols
  
  return(model$MakeTransactions)
  
}


#TODO: rewrite this function to use matrix calculations when possible
#' Aggregate the UseTable based on specified source file
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param aggregationSpecs Specifications for aggregation
#' @param domestic Boolean to indicate whether to aggregate the UseTransactions or DomesticUseTransactions table 
#' @return An aggregated UseTransactions or DomesticUseTransactions Table.
aggregateUseTable <- function(model, aggregationSpecs, domestic = FALSE){
  
  agg <- aggregationSpecs$Sectors

  count <- 1
  
  for (sector in agg){
    
    if(count == 1){
      count <- count + 1
      next #first sector in agg is the one we are aggregating to, so skip
    } 
    
    if(domestic == TRUE){
      model$DomesticUseTransactions <- aggregateSector(model, agg[1], agg[count], "Use", domestic)
    }else{
      model$UseTransactions <- aggregateSector(model, agg[1], agg[count], "Use", domestic = FALSE)  
    }
    
    count <- count + 1
  }
  
  #remove rows and cols from model
  agg <- agg[-1]
  
  if(domestic == TRUE){
    
    table <- model$DomesticUseTransactions[!(rownames(model$DomesticUseTransactions)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
    table <- model$DomesticUseTransactions[,!(colnames(model$DomesticUseTransactions)) %in% agg] #as above but with cols
    
  }else{
    table <- model$UseTransactions[!(rownames(model$UseTransactions)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
    table <- model$UseTransactions[,!(colnames(model$UseTransactions)) %in% agg] #as above but with cols

  }

  return(table)
  
}


#TODO: rewrite this function to use matrix calculations when possible
#' Aggregate the MakeTable based on specified source file
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param aggregationSpecs Specifications for aggregation
#' @return An aggregated MakeTable.
aggregateVA <- function(model, aggregationSpecs){
  
  agg <- aggregationSpecs$Sectors

  count <- 1

  for (sector in agg){
    
    if(count == 1){
      count <- count + 1
      next #first sector in agg is the one we are aggregating to, so skip
    } 
    
    model$UseValueAdded <- aggregateSector(model, agg[1], agg[count], "VA")
    count <- count + 1
  }
  
  #remove rows and cols from model
  agg <- agg[-1]
  model$UseValueAdded <- model$UseValueAdded[!(rownames(model$UseValueAdded)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
  model$UseValueAdded <- model$UseValueAdded[,!(colnames(model$UseValueAdded)) %in% agg] #as above but with cols
  
  return(model$UseValueAdded)
  
}



#' Aggregate a sector in a table
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param mainSector  Sector to aggregate to (string)
#' @param sectorToRemove Sector to be aggregated into mainSector, then removed from table (string)
#' @param tableType String to designate either Make or Use table
#' @param domestic Boolean to indicate whether to aggregate the UseTransactions or DomesticUseTransactions table 
#' @return aggregated table 
aggregateSector <- function(model, mainSector, sectorToRemove, tableType, domestic = FALSE){
  #get correct indeces
  if(tableType == "Use") {
    mainRowIndex <- getIndex(model$Commodities$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$Commodities$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    
    if(domestic == TRUE) {
      table <- model$DomesticUseTransactions
    } else {
      table <- model$UseTransactions  
    }
    
  } else if(tableType == "Make") {
    mainRowIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Commodities$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Commodities$Code_Loc, sectorToRemove)
    
    table <- model$MakeTransactions
    
  } else if(tableType == "VA") {
    mainRowIndex <- getIndex(model$ValueAddedMeta$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$ValueAddedMeta$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    
    table <- model$UseValueAdded
    
  } else {
    #continue
  }
  
  if(length(removeRowIndex) != 0 && length(mainRowIndex) !=0){#if there a row to remove and merge with the main sector
    table[mainRowIndex,] <- table[mainRowIndex,] + table[removeRowIndex,]#add rows together
    table[removeRowIndex,] <- table[removeRowIndex,] - table[removeRowIndex,]# subtract row from itself to ensure table total is unchanged
  }
  
  if(length(removeColIndex)!=0 && length(mainColIndex) !=0){#if there a col to remove and merge with the main sector
    table[,mainColIndex] <- table[,mainColIndex] + table[,removeColIndex]#add cols together
    table[,removeColIndex] <- table[,removeColIndex] - table[,removeColIndex]# subtract col from itself to ensure table total is unchanged#need to check this line to make sure sum removecol = 0
    
  }

  return(table)
}


#' Return the index where a sector occurrs in a sectorList 
#' @param sectorList Dataframe (of strings) to match the index of the sector param
#' @param sector String of the sector to look the index for
#' @return Index of sector in sectorList
getIndex <- function(sectorList, sector){
  index <- which(sectorList %in% sector)

  return(index)
  
}


#' Aggregate the MasterCrosswalk on the selected sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param aggregationSpecs Specifications for aggregation
#' @return crosswalk with aggregated sectors removed
aggregateMasterCrosswalk <- function (model, aggregationSpecs){
  
  agg <- aggregationSpecs$Sectors

  new_cw <- model$crosswalk #variable to return with complete changes to crosswalk#temp

  secLength <- regexpr(pattern ='/',agg[1]) - 1 #used to determine the length of the sector codes. E.g., detail would be 6, while summary would generally be 3 though variable, and sector would be variable
  # Update the sector and summary fields to match the aggregated sector
  new_cw$BEA_Sector[which(new_cw$USEEIO %in% substr(agg[-1],1,secLength))] <- new_cw$BEA_Sector[new_cw$USEEIO == substr(agg[1],1,secLength)][1]
  new_cw$BEA_Summary[which(new_cw$USEEIO %in% substr(agg[-1],1,secLength))] <- new_cw$BEA_Summary[new_cw$USEEIO == substr(agg[1],1,secLength)][1]
  # Update the value in USEEIO column with the aggregated sector
  new_cw$USEEIO[which(new_cw$USEEIO %in% substr(agg[-1],1,secLength))] <- substr(agg[1],1,secLength)

  return(new_cw)
  
}

#' Remove specific rows from the specified list object in the model
#' @param sectorList Model object to be aggregated 
#' @param indecesToAggregate List of indeces of sectors to remove from list (i.e. aggregated sectors)
#' @return An aggregated sectorList
removeRowsFromList <- function(sectorList, indecesToAggregate){

  newList <- sectorList[-(indecesToAggregate),]
  
  return(newList)

}


#' Aggregate MultiYear Output model objects
#' @param originalOutput MultiYear Output dataframe
#' @param mainIndex Index to aggregate the others to.
#' @param indecesToAggregate List of indeces to aggregate.
#' @return model A dataframe with the disaggregated GDPGrossOutputIO by year.
aggregateMultiYearOutput <- function(originalOutput, mainIndex, indecesToAggregate){
    
  newOutput <- originalOutput
  
  newOutput[mainIndex,] <- originalOutput[mainIndex,]+colSums(originalOutput[indecesToAggregate,])
  newOutput <- newOutput[-(indecesToAggregate),]
  
  return(newOutput)
  
}



