#' Aggregate a model based on specified source file
#' @param model Model file loaded with IO tables
#' @return An aggregated model.
aggregateModel <- function (model){
  

  #aggregating economic tables
  model$MakeTransactions <- aggregateMakeTable(model)
  model$UseTransactions <- aggregateUseTable(model)
  model$DomesticUseTransactions <- aggregateUseTable(model, domestic = TRUE)

  
  #aggregating Crosswalk
  model$crosswalk <- aggregateMasterCrosswalk(model)
  
  #aggregating (i.e. removing) sectors from model lists
  agg <- model$DisaggregationSpecs$Aggregation[2:length(model$DisaggregationSpecs$Aggregation)]#first item in Aggregation is the sector to aggregate to, not to be removed
  comIndecesToRemove <- which(model$Commodities$Code_Loc %in% agg) #find row indeces containing references to the sectors to be aggregated
  indIndecesToRemove <- which(model$Industries$Code_Loc %in% agg)
  
  #aggregate Industry lists
  if(length(indIndecesToRemove)!=0){
    model$Industries <- removeRowsFromList(model$Industries, indIndecesToRemove)
    model$MultiYearIndustryCPI <- removeRowsFromList(model$MultiYearIndustryCPI, indIndecesToRemove)
    
  }
  
  #aggregate Commodity lists
  if(length(comIndecesToRemove !=0)){
    
  }
    
  temp <-1  
  
  return(model)
}

#TODO: rewrite this function to use matrix calculations when possible
#' Aggregate the MakeTable based on specified source file
#' @param model Model file loaded with IO tables
#' @return An aggregated MakeTable.
aggregateMakeTable <- function(model){
  
  agg <- model$DisaggregationSpecs$Aggregation
  logging::loginfo(paste0("Aggregating sectors to'", agg[1], "'."))
  
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
  
  agg <- model$DisaggregationSpecs$Aggregation
  logging::loginfo(paste0("Aggregating sectors to'", agg[1], "'."))
  
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

#' Aggregate a sector in a table by adding the values from 
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
    
  }else{
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
  
  index <- which(sectorList==sector)
  
  if(length(index)==0){
    logging::logwarn(paste0("'", sector, "' string does not occur in current list.'"))

  }
  
  return(index)
  
}


#' Disaggregate the MasterCrosswalk to include the new sectors for disaggregation
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return crosswalk with aggregated sectors removed
aggregateMasterCrosswalk <- function (model){
  
  
  agg <- model$DisaggregationSpecs$Aggregation

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
#' @param indencesToRemove List of indeces of sectors to remove from list (i.e. aggregated sectors)
#' @return An aggregated sectorList
removeRowsFromList <- function(sectorList, indecesToRemove){

  newList <- sectorList[-(indecesToRemove),]
  
  return(newList)

}
