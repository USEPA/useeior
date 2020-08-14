#' Disaggregate satellite tables based on specs
#' 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' 
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (model, sattable){
  
  #Disaggregate satellite tables
  
  return(sattable_disaggregated)
}
#' 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' 
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateMakeTable <- function (model){
  
  #specify type of disaggregation
  disaggType = model$DisaggregationSpecs$DisaggregationType

  #disaggregation can be of types "Predefined" or "UserDefined". 
  if(disaggType == "Predefined"){
    
    #Predefined disaggregation assumes 1 industry/commodity disaggregated uniformly into several, with  
    #values along the intersections disaggregated uniformly along the diagonal.
   
    originalMake<-model$Make
    modMake<-originalMake
    
    #Determine number of commodities and industries in originalMake
    nCommodities <- ncol(originalMake)-1
    nIndustries <- nrow(originalMake)-1 
    
    #Deterine number of commodities and industries in DisaggSpecs
    numNewSectors <- length(model$DisaggregationSpecs$PredefinedSectors$DisaggregatedSectorCodes) 
    
    #Determine commodity and industry indeces corresponding to the original sector code
    originalRowIndex <- which(rownames(originalMake)==model$DisaggregationSpecs$PredefinedSectors$OriginalSectorCode)
    originalColIndex <- which(colnames(originalMake)==model$DisaggregationSpecs$PredefinedSectors$OriginalSectorCode)
    
    #Determine end index of disaggregated sectors
    endRowIndex <- originalRowIndex + numNewSectors -1
    endColIndex <- originalColIndex + numNewSectors -1
    
    
  ########Row disaggregation
    #Copy original row (ind) for disaggregation
    originalRowVector <- originalMake[originalRowIndex,]
    
    #Divide original row (ind) by number of new sectors to calculate values of individual rows
    originalRowVector <- originalRowVector/numNewSectors
    
    #Create new rows with the uniform values
    disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors), ]
    
    #Rename rows to use the disaggregated codes
    rownames(disaggRows) <- model$DisaggregationSpecs$PredefinedSectors$DisaggregatedSectorCodes
    
    
  ########Columnn disaggregation
    #Copy original Column (Com) for disaggregation
    originalColVector <-originalMake[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
    
    #Divide original Column by number of new setors to calculate te values of individual rows
    originalColVector <- originalColVector/numNewSectors
    
    #Create new cols with the uniform values
    disaggCols <- originalColVector[, rep(seq_len(ncol(originalColVector)), numNewSectors)]
  
    #Rename cols to use the disaggregted codes
    colnames(disaggCols) <- model$DisaggregationSpecs$PredefinedSectors$DisaggregatedSectorCodes
    
    
  ########Intersection Disaggregation
    originalIntersection <- originalMake[originalRowIndex:originalRowIndex+numNewSectors-1, originalColIndex:originalColIndex+numNewSectors-1]
    
    #Populate disaggregated intersection assuming equal values along the diagonal. Matrix variable. 
    disaggIntersection <- diag(originalIntersection,numNewSectors,numNewSectors)
    
    #Convert to data frame
    disaggIntersection = as.data.frame(t(disaggIntersection))
    
    #rename rows and columns
    colnames(disaggIntersection) <- model$DisaggregationSpecs$PredefinedSectors$DisaggregatedSectorCodes
    rownames(disaggIntersection) <- model$DisaggregationSpecs$PredefinedSectors$DisaggregatedSectorCodes
    
    
  ########Avengers Assemble
    
    #Assembling all columns above disaggregated rows, including all disaggregated columns
    disaggTable <- cbind(originalMake[1:originalRowIndex-1,1:originalColIndex-1],  #above diagg rows, from 1st col to col right before disaggregation
                        disaggCols[1:originalRowIndex-1,],                        #insert disaggregated cols before disaggregated rows
                        originalMake[1:originalRowIndex-1,-(1:originalColIndex)]) #include all cols except from 1st col to disaggregated col
    
    #Inserting intersection into disaggregated rows
    disaggRows <- cbind(disaggRows[,1:originalColIndex-1],  #from 1st col to col right before disaggregation
                        disaggIntersection,                 #insert disaggregated intersection
                        disaggRows[,-(1:originalColIndex)]) #include all cols except from 1s col to disaggregated col
    
    #Appending rest of original rows to partially assembled DMake
    disaggTable <- rbind(disaggTable,disaggRows)
    
    #Assembling all columns below disaggregated rows, including all disaggregated columns
    disaggTableBottom <- cbind(originalMake[-(1:originalRowIndex),1:originalColIndex-1],  #below disagg rows, from 1st col to col right before disaggregation
                               disaggCols[-(1:originalRowIndex),],                        #insert disaggregated cols below disaggregated rows
                               originalMake[-(1:originalRowIndex),-(1:originalColIndex)]) #below disagg rows, all columns after disagg columns 
    
    #Appeding bottom part of the table to top part of the table
    disaggTable <- rbind(disaggTable, disaggTableBottom)
    
  } else if(disaggType == "UserDefined"){
    #TODO: perform UserDefined disaggregationd
    
  } else {
    
    logging::loginfo("Disaggregation not performed, type not defined")
    break
  }
  
  
  
  return(disaggTable)
}
