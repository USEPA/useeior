


disaggregateModel <- function (model){

  for (disagg in model$specs$DisaggregationSpecs){
    disaggregationConfigFile <- disagg
    logging::loginfo(paste("Reading disaggregation for", disaggregationConfigFile, sep=" "))
    model$DisaggregationSpecs <- getModelConfiguration(disaggregationConfigFile)
  }
  logging::loginfo("Initializing Disaggregation of IO tables...")
  #model$Make <- disaggregateMakeTable(model)
  model$MakeTransactions <- disaggregateMakeTable(model)
  #model$Use <- disaggregateUseTable(model)
  model$UseTransactions <- disaggregateUseTable(model)
  model$DomesticUseTransactions <- disaggregateUseTable(model, TRUE)
  counter = 1
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    model$UseValueAdded <- disaggregateRows(model$UseValueAdded, disagg)
    model$CommodityOutput <- disaggregateCols(model$CommodityOutput, disagg)
    model$CPI <- disaggregateCols(model$CPI, disagg, TRUE)
    model$FinalDemand <- disaggregateCols(model$FinalDemand, disagg)
    model$DomesticFinalDemand <- disaggregateCols(model$DomesticFinalDemand, disagg)

    index <- match(disagg$OriginalSectorCode, model$SectorNames$SectorCode)
    newNames <- data.frame("SectorCode" = disagg$DisaggregatedSectorCodes, "SectorName"=disagg$DisaggregatedSectorNames)
    model$SectorNames <- rbind(model$SectorNames[1:index-1,],newNames,model$SectorNames[-(1:index),])

    # margins tables need to be adjusted as the index is not the sector code like other dataframes
    #model$IntermediateMargins <- disaggregateCols(model$IntermediateMargins, disagg)
    #model$FinalConsumerMargins <- disaggregateCols(model$FinalConsumerMargins, disagg)
    
    # Need to disaggregate original BEA vectors for use later
    model$BEA$UseCommodityOutput <- disaggregateCols(model$BEA$UseCommodityOutput, disagg)
    model$BEA$MakeIndustryOutput <- disaggregateCols(model$BEA$MakeIndustryOutput, disagg)
    model$GDP$BEAGrossOutputIO <- disaggregateCols(model$GDP$BEAGrossOutputIO, disagg)
    model$GDP$BEACPIIO <- disaggregateCols(model$GDP$BEACPIIO, disagg, TRUE)
    
    if(!is.null(disagg$MakeFile)){
      disagg$MakeFileDf <- utils::read.csv(system.file("extdata", disagg$MakeFile, package = "useeior"),
                                           header = TRUE, stringsAsFactors = FALSE)}
    if(!is.null(disagg$UseFile)){
      disagg$UseFileDf <- utils::read.csv(system.file("extdata", disagg$UseFile, package = "useeior"),
                                           header = TRUE, stringsAsFactors = FALSE)}      
    if(!is.null(disagg$EnvFile)){
      disagg$EnvFileDf <- utils::read.csv(system.file("extdata", disagg$EnvFile, package = "useeior"),
                                           header = TRUE, stringsAsFactors = FALSE)}
    # Need to assign these DFs back to the modelspecs
    model$DisaggregationSpecs$Disaggregation[[counter]] <- disagg
    counter <- counter + 1
  }
  
  
  
  return(model)
  

}



#' Disaggregate satellite tables based on specs
#' 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param sat The abbreviation for the satellite table.
#' 
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (model, sattable, sat){
  
  # For each disaggregation:
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    # If satellite table data is provided for the new sector assign it here
    if(!is.null(disagg$EnvFileDF)){
      new_sector_totals <- disagg$EnvFileDF
      # Select only those rows from the disaggregation env file that apply for this satellite table
      new_sector_totals <- subset(new_sector_totals, SatelliteTable==sat$Abbreviation, colnames(sattable))
      
      included_sectors <- unique(new_sector_totals[,"SectorCode"])
      if (!identical(sort(included_sectors),sort(disagg$DisaggregatedSectorCodes))){
        logging::loginfo("Error: Satellite table does not include all disaggregated sectors")
      }
      
      # Append to the main dataframe
      sattable <- rbind(sattable,new_sector_totals)
    }
    
    else{
      # Subset the totals from the original sector
      old_sector_totals <- subset(sattable, SectorCode==disagg$OriginalSectorCode, colnames(sattable))
      
      i<-0
      for (new_sector in disagg$DisaggregatedSectorCodes){
        i<-i+1
        new_sector_totals <- old_sector_totals
        new_sector_totals$SectorCode <- disagg$DisaggregatedSectorCodes[[i]]
        new_sector_totals$SectorName <- disagg$DisaggregatedSectorNames[[i]]

        # If satellite table is disaggregated proportional to quantity do that here
        if(!is.null(disagg$NewSectorsOutput)){
          new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount * 
                                             (disagg$NewSectorsOutput[[i]] / Reduce("+",disagg$NewSectorsOutput)))
        }
        
        # Else, divide equally across new sectors
        else{
          new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount / length(disagg$DisaggregatedSectorCodes))
        }
        # Modify other metadata or DQI?
        
        
        # Append to the main dataframe
        sattable <- rbind(sattable,new_sector_totals)
    }}
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
      
      disaggRows <- disaggregateRow(originalRowVector,disagg)
      
    ########Columnn disaggregation
      #Copy original Column (Com) for disaggregation
      originalColVector <-originalMake[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
      
      disaggCols <- disaggregateCol(originalColVector,disagg)
      

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
      #TODO: perform UserDefined disaggregation
      
    } else {
      
      logging::loginfo("Disaggregation not performed, type not defined")
      break
    }
  }
  
  
  return(disaggTable)
}

disaggregateUseTable <- function(model, domestic = FALSE){
  
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
    
    #disaggregation can be of types "Predefined" or "UserDefined". 
    if(disaggType == "Predefined"){
      
      #Predefined disaggregation assumes 1 industry/commodity disaggregated uniformly into several, with  
      #values along the intersections disaggregated uniformly along the diagonal.
      
      if(domestic){
        originalUse<-model$DomesticUseTransactions  
      }
      else{
        originalUse<-model$UseTransactions
      }
      
      #Determine number of commodities and industries in originalUse
      nCommodities <- nrow(originalUse)
      nIndustries <- ncol(originalUse) 
      
      #Deterine number of commodities and industries in DisaggSpecs
      numNewSectors <- length(disagg$DisaggregatedSectorCodes) 
      
      #Determine commodity and industry indeces corresponding to the original sector code
      originalRowIndex <- which(rownames(originalUse)==disagg$OriginalSectorCode)
      originalColIndex <- which(colnames(originalUse)==disagg$OriginalSectorCode)
      
      #Determine end index of disaggregated sectors
      endRowIndex <- originalRowIndex + numNewSectors
      endColIndex <- originalColIndex + numNewSectors
      
      
      ########Row disaggregation
      #Copy original row (com) for disaggregation
      originalRowVector <- originalUse[originalRowIndex,]
      
      disaggRows <- disaggregateRow(originalRowVector,disagg)
      
      ########Columnn disaggregation
      #Copy original Column (ind) for disaggregation
      originalColVector <-originalUse[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
      
      disaggCols <- disaggregateCol(originalColVector,disagg)
      
      
      ########Intersection Disaggregation
      originalIntersection <- originalUse[originalRowIndex, originalColIndex]
      
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
      disaggTable <- cbind(originalUse[1:originalRowIndex-1,1:originalColIndex-1], #above diagg rows, from 1st col to col right before disaggregation
                           disaggCols[1:originalRowIndex-1,],                        #insert disaggregated cols before disaggregated rows
                           originalUse[1:originalRowIndex-1,-(1:originalColIndex)]) #include all cols except from 1st col to disaggregated col
      
      #Inserting intersection into disaggregated rows
      disaggRows <- cbind(disaggRows[,1:originalColIndex-1],  #from 1st col to col right before disaggregation
                          disaggIntersection,                 #insert disaggregated intersection
                          disaggRows[,-(1:originalColIndex)]) #include all cols except from 1s col to disaggregated col
      
      #Appending rest of original rows to partially assembled DMake
      disaggTable <- rbind(disaggTable,disaggRows)
      
      #Assembling all columns below disaggregated rows, including all disaggregated columns
      disaggTableBottom <- cbind(originalUse[-(1:originalRowIndex),1:originalColIndex-1],  #below disagg rows, from 1st col to col right before disaggregation
                                 disaggCols[-(1:originalRowIndex),],                        #insert disaggregated cols below disaggregated rows
                                 originalUse[-(1:originalRowIndex),-(1:originalColIndex)]) #below disagg rows, all columns after disagg columns 
      
      #Appeding bottom part of the table to top part of the table
      disaggTable <- rbind(disaggTable, disaggTableBottom)
      
    } else if(disaggType == "UserDefined"){
      #TODO: perform UserDefined disaggregation
      
    } else {
      
      logging::loginfo("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
}

disaggregateRows <- function (RowVectors, disagg_specs, duplicate=FALSE){
  
  originalColIndex <- which(colnames(RowVectors)==disagg_specs$OriginalSectorCode)
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  ColVector <- RowVectors[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
  disaggCols <- disaggregateCol (ColVector, disagg_specs, duplicate)
  
  disaggRows <- cbind(RowVectors[,1:originalColIndex-1],  #from 1st col to col right before disaggregation
                      disaggCols,                         #insert disaggregated cols
                      RowVectors[,-(1:originalColIndex)]) #include all cols except from 1s col to disaggregated col
  
  return(disaggRows)
  
}

disaggregateCols <- function (ColVectors, disagg_specs, duplicate=FALSE){
  
  originalRowIndex <- which(rownames(ColVectors)==disagg_specs$OriginalSectorCode)
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  RowVector <- ColVectors[originalRowIndex,,drop=FALSE]
  disaggRows <- disaggregateRow (RowVector, disagg_specs, duplicate)

  disaggCols <- rbind(ColVectors[1:originalRowIndex-1,,drop=FALSE],  #from 1st row to row right before disaggregation
                      disaggRows,                                    #insert disaggregated rows
                      ColVectors[-(1:originalRowIndex),,drop=FALSE]) #include all rows except from 1s row to disaggregated row
  
  return(disaggCols)
  
}

disaggregateRow <- function (originalRowVector, disagg_specs, duplicate = FALSE){
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (!duplicate){
      #Divide original row (ind) by number of new sectors to calculate values of individual rows
      originalRowVector <- originalRowVector/numNewSectors
  }
  
  #Create new rows with the uniform values
  disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
  
  #Rename rows to use the disaggregated codes
  rownames(disaggRows) <- disagg_specs$DisaggregatedSectorCodes
  
  return(disaggRows)
}

disaggregateCol <- function (originalColVector, disagg_specs, duplicate = FALSE){
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (!duplicate){
      #Divide original Column by number of new sectors to calculate the values of individual rows
      originalColVector <- originalColVector/numNewSectors
  }
  
  #Create new cols with the uniform values
  disaggCols <- originalColVector[, rep(seq_len(ncol(originalColVector)), numNewSectors)]
  
  #Rename cols to use the disaggregted codes
  colnames(disaggCols) <- disagg_specs$DisaggregatedSectorCodes
  
  return(disaggCols)
}
