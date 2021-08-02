#' Aggregate a model based on specified source file
#' @param model Model file loaded with IO tables
#' @return An aggregated model.
aggregateModel <- function (model){
  
  temp <-1
  
  model$MakeTransactions <- aggregateMakeTable(model)
  model$UseTransactions <- aggregateUseTable(model)
  
  
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
#' @return An aggregated UseTable.
aggregateUseTable <- function(model){
  
  agg <- model$DisaggregationSpecs$Aggregation
  logging::loginfo(paste0("Aggregating sectors to'", agg[1], "'."))
  
  count <- 1
  
  
  for (sector in agg){
    
    if(count == 1){
      count <- count + 1
      next #first sector in agg is the one we are aggregating to, so skip
    } 
    
    model$UseTransactions <- aggregateSector(model, agg[1], agg[count], "Use")
    count <- count + 1
  }
  
  #remove rows and cols from model
  agg <- agg[2:length(agg)]
  model$UseTransactions <- model$UseTransactions[!(rownames(model$UseTransactions)) %in% agg,] #remove rows from model that have the same rownames as values in agg list
  model$UseTransactions <- model$UseTransactions[,!(colnames(model$UseTransactions)) %in% agg] #as above but with cols
  
  return(model$UseTransactions)
  
}

#' Aggregate a sector in a table by adding the values from 
#' @param model Model file loaded with IO tables
#' @param mainSector  Sector to aggregate to (string)
#' @param sectorToRemove Sector to be aggregated into mainSector, then removed from table (string)
#' @param tableType String to designate either Make or Use table 
#' @return aggregated table 
aggregateSector <- function(model, mainSector, sectorToRemove, tableType){
  #get correct indeces
  if(tableType == "Use"){
    mainRowIndex <- getIndex(model$Commodities$Code_Loc, mainSector)
    mainColIndex <- getIndex(model$Industries$Code_Loc, mainSector)
    
    removeRowIndex <- getIndex(model$Commodities$Code_Loc, sectorToRemove)
    removeColIndex <- getIndex(model$Industries$Code_Loc, sectorToRemove)
    
    table <- model$UseTransactions
    
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