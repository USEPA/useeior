


disaggregateModel <- function (model){
  for (disagg in model$specs$DisaggregationSpecs){
    disaggregationConfigFile <- disagg
    logging::loginfo(paste("Reading disaggregation for", disaggregationConfigFile, sep=" "))
    model$DisaggregationSpecs <- getModelConfiguration(disaggregationConfigFile)
  }
  logging::loginfo("Initializing Disaggregation of IO tables...")
  model$MakeTransactions <- disaggregateMakeTable(model)
  #model$UseTransactions <- disaggregateUseTable(model)
  #model$UseValueAdded
  
  
  return(model)
  

}



#' Disaggregate satellite tables based on specs
#' 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' 
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (model, sattable){
  
  # For each disaggregation:
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    # Subset the totals from the original sector
    old_sector_totals <- subset(sattable, SectorCode==disagg$OriginalSectorCode, colnames(sattable))
    i<-0
    for (new_sector in disagg$DisaggregatedSectorCodes){
      i<-i+1
      new_sector_totals <- old_sector_totals
      new_sector_totals$SectorCode <- disagg$DisaggregatedSectorCodes[[i]]
      new_sector_totals$SectorName <- disagg$DisaggregatedSectorNames[[i]]
      
      # If satellite table data is provided for the new sector assign it here
      
      # Else if satellite table is disaggregated proportional to quantity do that here
      if(!is.null(disagg$NewSectorsOutput)){
        new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount * 
                                           (disagg$NewSectorsOutput[[i]] / Reduce("+",disagg$NewSectorsOutput)))
      }
      
      # Else, divide equally across new sectors
      else
        new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount / length(disagg$DisaggregatedSectorCodes))
      
      # Modify other metadata or DQI?
      
      
      # Append to the main dataframe
      sattable <- rbind(sattable,new_sector_totals)
    }
    # Remove the old_sector_totals
    sattable_disaggregated <- subset(sattable, SectorCode!=disagg$OriginalSectorCode)
  }
  
  return(sattable_disaggregated)
}

#' Disaggregate make table based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' 
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateMakeTable <- function (model){
  
  for (disagg in model$DisaggregationSpecs$Disaggregation){
  
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
  
    #disaggregation can be of types "Predefined" or "UserDefined". 
    if(disaggType == "Predefined"){
      
      #Predefined disaggregation assumes 1 industry/commodity disaggregated uniformly into several, with  
      #values along the intersections disaggregated uniformly along the diagonal.
     
      originalMake<-model$MakeTransactions

      #Determine number of commodities and industries in originalMake
      nCommodities <- ncol(originalMake)
      nIndustries <- nrow(originalMake) 
      
      #Deterine number of commodities and industries in DisaggSpecs
      numNewSectors <- length(disagg$DisaggregatedSectorCodes) 
      
      #Determine commodity and industry indeces corresponding to the original sector code
      originalRowIndex <- which(rownames(originalMake)==disagg$OriginalSectorCode)
      originalColIndex <- which(colnames(originalMake)==disagg$OriginalSectorCode)
      
      #Determine end index of disaggregated sectors
      endRowIndex <- originalRowIndex + numNewSectors
      endColIndex <- originalColIndex + numNewSectors
      
      
    ########Row disaggregation
      #Copy original row (ind) for disaggregation
      originalRowVector <- originalMake[originalRowIndex,]
      
      #Divide original row (ind) by number of new sectors to calculate values of individual rows
      originalRowVector <- originalRowVector/numNewSectors
      
      #Create new rows with the uniform values
      disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors), ]
      
      #Rename rows to use the disaggregated codes
      rownames(disaggRows) <- disagg$DisaggregatedSectorCodes
      
      
    ########Columnn disaggregation
      #Copy original Column (Com) for disaggregation
      originalColVector <-originalMake[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
      
      #Divide original Column by number of new sectors to calculate the values of individual rows
      originalColVector <- originalColVector/numNewSectors
      
      #Create new cols with the uniform values
      disaggCols <- originalColVector[, rep(seq_len(ncol(originalColVector)), numNewSectors)]
    
      #Rename cols to use the disaggregted codes
      colnames(disaggCols) <- disagg$DisaggregatedSectorCodes
      
      
    ########Intersection Disaggregation
      originalIntersection <- originalMake[originalRowIndex, originalColIndex]
      
      #Divide intersection by number of new sectors
      originalIntersection <- originalIntersection/numNewSectors
      
      #Populate disaggregated intersection assuming equal values along the diagonal. Matrix variable. 
      disaggIntersection <- diag(originalIntersection,numNewSectors,numNewSectors)
      
      #Convert to data frame
      disaggIntersection = as.data.frame(t(disaggIntersection))
      
      #rename rows and columns
      colnames(disaggIntersection) <- disagg$DisaggregatedSectorCodes
      rownames(disaggIntersection) <- disagg$DisaggregatedSectorCodes
      
      
    ########Assemble table
      
      #Assembling all columns above disaggregated rows, including all disaggregated columns
      disaggTable <- cbind(originalMake[1:originalRowIndex-1,1:originalColIndex-1], #above diagg rows, from 1st col to col right before disaggregation
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
  }
  
  
  return(disaggTable)
}
