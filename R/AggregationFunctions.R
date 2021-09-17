#' Aggregate a model based on specified source file
#' @param model Model file loaded with IO tables
#' @return An aggregated model.
aggregateModel <- function (model){

  logging::loginfo(paste0("Aggregating sectors to ",model$DisaggregationSpecs$Aggregation$Sectors[1], "..."))
  #aggregating economic tables
  model$MakeTransactions <- aggregateMakeTable(model)
  model$UseTransactions <- aggregateUseTable(model)
  model$DomesticUseTransactions <- aggregateUseTable(model, domestic = TRUE)
  model$UseValueAdded <- aggregateVA(model)
  #model$FinalDemand <- aggregateFD(model) #todo
  #model$DomesticFinalDemand <- aggregateFD(model) #todo
  #model$MarginSectors <- aggregateMarginSectors(model) #todo
  #model$Margins <- aggregateMargins(model)
  
  #model$ValueAddedMeta <- aggregateVAMeta(model) #todo
  #model$FinalDemandMeta <- aggregateFDMeta(model) #todo
  
  #aggregating Crosswalk
  model$crosswalk <- aggregateMasterCrosswalk(model)
  
  #obtaining indeces to aggregate sectors in remaining model objects
  #agg <- model$DisaggregationSpecs$Aggregation 
  agg <- model$DisaggregationSpecs$Aggregation$Sectors
  mainComIndex <- getIndex(model$Commodities$Code_Loc, agg[1])#first item in Aggregation is the sector to aggregate to, not to be removed
  mainIndIndex <- getIndex(model$Industries$Code_Loc, agg[1])
  comIndecesToAggregate <- which(model$Commodities$Code_Loc %in% agg[2:length(agg)]) #find com indeces containing references to the sectors to be aggregated
  indIndecesToAggregate <- which(model$Industries$Code_Loc %in% agg[2:length(agg)]) #find ind indeces containing references to the sectors to be aggregated
  
  #aggregating (i.e. removing) sectors from model lists
  #aggregate Industry lists
  if(length(indIndecesToAggregate)!=0){
    
    model$Industries <- removeRowsFromList(model$Industries, indIndecesToAggregate)
    model$MultiYearIndustryCPI <- aggregateMultiYearCPI(model, mainIndIndex, indIndecesToAggregate, "Industry")
    model$MultiYearIndustryOutput <- aggregateMultiYearOutput(model$MultiYearIndustryOutput, mainIndIndex, indIndecesToAggregate)
    model$IndustryOutput <- aggregateOutputs(model, indIndecesToAggregate, "Industry")#aggregate model$IndustryOutput object
  }
  
  #aggregate Commodity lists
  if(length(comIndecesToAggregate !=0)){
    model$Commodities <- removeRowsFromList(model$Commodities, comIndecesToAggregate)
    model$MultiYearCommodityCPI <- aggregateMultiYearCPI(model, mainIndIndex, indIndecesToAggregate, "Commodity")
    model$MultiYearIndustryOutput <- aggregateMultiYearOutput(model$MultiYearIndustryOutput, mainComIndex, comIndecesToAggregate)
    model$CommodityOutput <- aggregateOutputs(model, comIndecesToAggregate, "Commodity")#aggregate model$CommoditOutput object
  }
    
  
  return(model)
}

#' Calculate updated Commodity and Industry Output model objects after aggregation
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param indecesToAggregate List of indeces to aggregate.
#' @param type String to designate either commodity or industry
#' @return  An aggregated Industry or Commodity output object.
aggregateOutputs <- function(model, indecesToAggregate, type)
{

  if(type == "Industry"){
    model$IndustryOutput <- colSums(model$UseTransactions)+colSums(model$UseValueAdded) #calculate new output total
    return (model$IndustryOutput)

  }else { #assumes commodity
    model$CommodityOutput <- rowSums(model$UseTransactions)+rowSums(model$FinalDemand)
    return(model$CommodityOutput)

  }
  

}


#TODO: Complete this function
#' Aggregate satellite tables from static file based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param sat The abbreviation for the satellite table.
#' @return A standardized satellite table with aggregated sectors added.
aggSatelliteTable <- function (model, sattable, sat){
  

  #if(!is.null(ENV_AGG_FILE)) #TODO: implement option to aggregate according to specified file
  #if(FLOWSA) #TODO: implement option to aggregate based on flowsa data (e.g. by NAICS sector as mapped by crosswalk)
  #else #Default aggregation: sum up the relevant sectors in tbs
  
  #obtaining indeces to aggregate sectors in remaining model objects
  newSatTable <- sattable
  #agg <- model$DisaggregationSpecs$Aggregation
  agg <- model$DisaggregationSpecs$Aggregation$Sectors
  
  #variable to determine length of Code substring, i.e., code length minus geographic identifer and separator character (e.g. "/US")
  codeLength <- nchar(gsub("/.*", "", agg[1]))
  aggCodes <- substr(agg,1,codeLength)
  
  mainIndeces <- getIndex(newSatTable$Sector, aggCodes[1])#row indeces in sattable where the sector field contains the code for the sector to aggregate to
  
  
  for (sectorCode in aggCodes[2:length(aggCodes)]){
    indecesToAggregate <- which(newSatTable$Sector %in% sectorCode) #row indeces in sattable where the Sector field contaisn the codes for the sectors to be aggregated
    
    if(length(indecesToAggregate) !=0){
      sattableToAgg <- newSatTable[indecesToAggregate,]#get rows corresponding to flows included in the sector to be aggregated
      
      for(currentRow in 1:nrow(sattableToAgg)){
        
        #get index in sattable that matches the flow in the main sector with the flow in the current sector to be aggregated
        sattableMainRowIndex <- which(newSatTable$Sector == aggCodes[1] & newSatTable$Flowable == sattableToAgg$Flowable[currentRow])
        
        #add value from sector to add to main sector
        newSatTable$FlowAmount[sattableMainRowIndex] <- newSatTable$FlowAmount[sattableMainRowIndex] + sattableToAgg$FlowAmount[currentRow]
        
      }
      
      newSatTable <- removeRowsFromList(newSatTable, indecesToAggregate)#remove aggregated sectors
      
    }else{
      next
    }

  }#end for loop
  
  return(newSatTable)
}

#' Aggregate MultiYear CPI model objects
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param mainIdex Index to aggregate the others to.
#' @param indecesToAggregate List of indeces to aggregate.
#' @param type String to designate either commodity or industry
#' @return newCPI A dataframe with the aggregatded CPI values by year.
aggregateMultiYearCPI <- function(model, mainIndex, indecesToAggregate, type){
  
  if(type == "Industry"){
    
    originalCPI <- model$MultiYearIndustryCPI
    originalOutput <- model$MultiYearIndustryOutput
    
  }else {
    
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
#' @param model Model file loaded with IO tables
#' @return An aggregated MakeTable.
aggregateMakeTable <- function(model){
  
  #agg <- model$DisaggregationSpecs$Aggregation
  agg <- model$DisaggregationSpecs$Aggregation$Sectors

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
  agg <- agg[2:length(agg)]
  model$MakeTransactions <- model$MakeTransactions[!(rownames(model$MakeTransactions)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
  model$MakeTransactions <- model$MakeTransactions[,!(colnames(model$MakeTransactions)) %in% agg] #as above but with cols
  
  return(model$MakeTransactions)
  
}


#TODO: rewrite this function to use matrix calculations when possible
#' Aggregate the UseTable based on specified source file
#' @param model Model file loaded with IO tables
#' @param domestic Boolean to indicate whether to aggregate the UseTransactions or DomesticUseTransactions table 
#' @return An aggregated UseTransactions or DomesticUseTransactions Table.
aggregateUseTable <- function(model, domestic = FALSE){
  
  #agg <- model$DisaggregationSpecs$Aggregation
  agg <- model$DisaggregationSpecs$Aggregation$Sectors

  
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
  agg <- agg[2:length(agg)]
  
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
#' @return An aggregated MakeTable.
aggregateVA <- function(model){
  
  #agg <- model$DisaggregationSpecs$Aggregation
  agg <- model$DisaggregationSpecs$Aggregation$Sectors

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
  agg <- agg[2:length(agg)]
  model$UseValueAdded <- model$UseValueAdded[!(rownames(model$UseValueAdded)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
  model$UseValueAdded <- model$UseValueAdded[,!(colnames(model$UseValueAdded)) %in% agg] #as above but with cols
  
  return(model$UseValueAdded)
  
}




#' Aggregate a sector in a table
#' @param model Model file loaded with IO tables
#' @param mainSector  Sector to aggregate to (string)
#' @param sectorToRemove Sector to be aggregated into mainSector, then removed from table (string)
#' @param tableType String to designate either Make or Use table
#' @param domestic Boolean to indicate whether to aggregate the UseTransactions or DomesticUseTransactions table 
#' @return aggregated table 
aggregateSector <- function(model, mainSector, sectorToRemove, tableType, domestic = FALSE){
  #get correct indeces
  if(tableType == "Use"){
    mainRowIndex <- getIndex(model$Commodities$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$Commodities$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    
    if(domestic == TRUE){
      table <- model$DomesticUseTransactions
    }else{
      table <- model$UseTransactions  
    }
    
    
  }else if(tableType == "Make"){
    mainRowIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Commodities$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Commodities$Code_Loc, sectorToRemove)
    
    table <- model$MakeTransactions
    
  }else if(tableType == "VA"){
    mainRowIndex <- getIndex(model$ValueAddedMeta$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$ValueAddedMeta$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    
    table <- model$UseValueAdded
    
  }
  else{
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
  
  # if(length(index)==0){
  #   logging::logwarn(paste0("'", sector, "' string does not occur in current list.'"))
  # 
  # }
  
  return(index)
  
}


#' Disaggregate the MasterCrosswalk to include the new sectors for disaggregation
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return crosswalk with aggregated sectors removed
aggregateMasterCrosswalk <- function (model){
  
  
  #agg <- model$DisaggregationSpecs$Aggregation
  agg <- model$DisaggregationSpecs$Aggregation$Sectors

  crosswalk <- model$crosswalk#temp variable for storing intermediate changes
  new_cw <- crosswalk#variable to return with complete changes to crosswalk#temp

  #deterime which rows and columns to modify
  cwColIndex <- match(paste0("BEA_", model$specs$BaseIOLevel), colnames(crosswalk)) #search for concatenation of "BEA" and model$specs$BaseIOlevel object in crosswalk column names
  OriginalCodeLength <- regexpr(pattern ='/',agg[1]) - 1 #used to determine the length of the sector codes. E.g., detail would be 6, while summary would generally be 3 though variable, and sector would be variable
  aggCodeLength <- regexpr(pattern ='/',agg[2]) - 1 #used to determine length of disaggregated sector codes.
  
  rowIndecesToRemove <- which(new_cw[,cwColIndex] %in% substr(agg[2:length(agg)],1,aggCodeLength)) #find row indeces containing references to the sectors to be aggregated
  new_cw <-new_cw[-(rowIndecesToRemove),] #remove rows from model that have the same rownames as values in agg list
  
  
  return(new_cw)
  
}

#' Remove specific rows from the specified list object in the model
#' @param sectorList Model object to be aggregated 
#' @param indencesToAggregate List of indeces of sectors to remove from list (i.e. aggregated sectors)
#' @return An aggregated sectorList
removeRowsFromList <- function(sectorList, indecesToAggregate){

  newList <- sectorList[-(indecesToAggregate),]
  
  return(newList)

}



#' Aggregate MultiYear Output model objects
#' @param originalOutput MultiYear Output dataframe
#' @param mainIdex Index to aggregate the others to.
#' @param indecesToAggregate List of indeces to aggregate.
#' @return model A dataframe with the disaggregated GDPGrossOutputIO by year.
aggregateMultiYearOutput <- function(originalOutput, mainIndex, indecesToAggregate){
    
  newOutput <- originalOutput
  
  newOutput[mainIndex,] <- originalOutput[mainIndex,]+colSums(originalOutput[indecesToAggregate,])
  newOutput <- newOutput[-(indecesToAggregate),]
  
  return(newOutput)
  
}



