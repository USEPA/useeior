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
