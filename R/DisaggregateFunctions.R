


disaggregateModel <- function (model){
  disaggregationConfigFile <- model$specs$DisaggregationSpecs
  logging::loginfo(paste("Reading disaggregation for", disaggregationConfigFile, sep=" "))
  model$DisaggregationSpecs <- getModelConfiguration(disaggregationConfigFile)
  
  logging::loginfo("Initializing Disaggregation of IO tables...")
  disaggregatedMake <- disaggregateMakeTable(model)
  
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
  for (disagg in model$DisaggregationSpecs){
    
    # Subset the totals from the original sector
    old_sector_totals <- subset(sattable, SectorCode==disagg$OldSector, colnames(sattable))
    i<-0
    for (new_sector in disagg$NewSectors){
      i<-i+1
      new_sector_totals <- old_sector_totals
      new_sector_totals$SectorCode <- disagg$NewSectors[[i]]
      #new_sector_totals$SectorName <- newname
      
      # If satellite table data is provided for the new sector assign it here
      
      # Else if satellite table is disaggregated proportional to quantity do that here
      if(!is.null(disagg$NewSectorsOutput)){
        new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount * 
                                           (disagg$NewSectorsOutput[[i]] / Reduce("+",disagg$NewSectorsOutput)))
      }
      
      # Else, divide equally across new sectors
      else
        new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount / length(disagg$NewSectors))
      
      # Modify other metadata or DQI?
      
      
      # Append to the main dataframe
      sattable <- rbind(sattable,new_sector_totals)
    }
    # Remove the old_sector_totals
    sattable_disaggregated <- subset(sattable, SectorCode!=disagg$OldSector)
  }
  
  return(sattable_disaggregated)
}

#' Disaggregate make table based on specs
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
    
    
  } else if(disaggType == "UserDefined"){
    #TODO: perform UserDefined disaggregation
    
  } else {
    
    logging::loginfo("Disaggregation not performed, type not defined")
    break
  }
  
  
  
  return(maketable_disaggregated)
}
