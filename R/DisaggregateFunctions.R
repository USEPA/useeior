#' Disaggregate a model based on specified source file
#' @param model Model file loaded with IO tables
#' @return A disaggregated model.
disaggregateModel <- function (model){

  for (disagg in model$specs$DisaggregationSpecs){
    disaggregationConfigFile <- disagg
    logging::loginfo(paste("Reading disaggregation for", disaggregationConfigFile, sep=" "))
    model$DisaggregationSpecs <- getModelConfiguration(disaggregationConfigFile)
  }

  counter = 1
  for (disagg in model$DisaggregationSpecs$Disaggregation){
   
    #TODO: Need to make disaggregateMake and disaggregateUse be able to disaggregate these tables, not just the MakeTransactions and UseTransactions.Right now, uncommenting the lines to disaggregate the Make and Use tables returns the result of the disaggregatation of the transactions tables.

    disagg$NAICSSectorCW <- utils::read.csv(system.file("extdata", disagg$SectorFile, package = "useeior"),
                                            header = TRUE, stringsAsFactors = FALSE, colClasses=c("NAICS_2012_Code"="character",
                                                                                                  "USEEIO_Code"="character"))
    index <- match(disagg$OriginalSectorCode, model$SectorNames$SectorCode)
    newNames <- unique(data.frame("SectorCode" = disagg$NAICSSectorCW$USEEIO_Code, "SectorName"=disagg$NAICSSectorCW$USEEIO_Name))
    disagg$DisaggregatedSectorNames <- as.list(levels(newNames[, 'SectorName']))
    disagg$DisaggregatedSectorCodes <- as.list(levels(newNames[, 'SectorCode']))
    
    if(!is.null(disagg$MakeFile)){
      disagg$MakeFileDF <- utils::read.csv(system.file("extdata", disagg$MakeFile, package = "useeior"),
                                           header = TRUE, stringsAsFactors = FALSE, colClasses=c("IndustryCode"="character",
                                                                                                 "CommodityCode"="character"))}
    if(!is.null(disagg$UseFile)){
      disagg$UseFileDF <- utils::read.csv(system.file("extdata", disagg$UseFile, package = "useeior"),
                                          header = TRUE, stringsAsFactors = FALSE)}      
    if(!is.null(disagg$EnvFile)){
      disagg$EnvFileDF <- utils::read.csv(system.file("extdata", disagg$EnvFile, package = "useeior"),
                                          header = TRUE, stringsAsFactors = FALSE, colClasses=c("SectorCode"="character"))}
    # Need to assign these DFs back to the modelspecs
    model$DisaggregationSpecs$Disaggregation[[counter]] <- disagg
    
    logging::loginfo("Initializing Disaggregation of IO tables...")
    #model$Use <- disaggregateUseTable(model)
    model$UseTransactions <- disaggregateUseTable(model)
    #model$Make <- disaggregateMakeTable(model) 
    model$MakeTransactions <- disaggregateMakeTable(model)
    
    if(disagg$DisaggregationType == "Userdefined"){
      
      notUniformFlag = TRUE
      
    }else{
      
      notUniformFlag = FALSE
      
    }
    
    model$UseValueAdded <- disaggregateRows(model$UseValueAdded, disagg, notUniform = notUniformFlag)
    model$FinalDemand <- disaggregateCols(model$FinalDemand, disagg, notUniform = notUniformFlag)

    #balancedDisaggFullUse <- balanceDisagg(model, disagg)
    model$DomesticUseTransactions <- disaggregateUseTable(model, domestic = TRUE)
    #model$UseValueAdded <- disaggregateRows(model$UseValueAdded, disagg)
    model$CommodityOutput <- disaggregateCols(model$CommodityOutput, disagg)
    model$CPI <- disaggregateCols(model$CPI, disagg, duplicate = TRUE)
    #model$FinalDemand <- disaggregateCols(model$FinalDemand, disagg)
    model$DomesticFinalDemand <- disaggregateCols(model$DomesticFinalDemand, disagg)

    model$SectorNames <- rbind(model$SectorNames[1:index-1,],newNames,model$SectorNames[-(1:index),])

    model$crosswalk <- disaggregateMasterCrosswalk(model$crosswalk, disagg)
    
    # margins tables need to be adjusted as the index is not the sector code like other dataframes
    #model$IntermediateMargins <- disaggregateCols(model$IntermediateMargins, disagg)
    #model$FinalConsumerMargins <- disaggregateCols(model$FinalConsumerMargins, disagg)
    
    # Need to disaggregate original BEA vectors for use later
    model$BEA$UseCommodityOutput <- disaggregateCols(model$BEA$UseCommodityOutput, disagg)
    model$BEA$MakeIndustryOutput <- disaggregateCols(model$BEA$MakeIndustryOutput, disagg)
    model$GDP$BEAGrossOutputIO <- disaggregateCols(model$GDP$BEAGrossOutputIO, disagg)
    model$GDP$BEACPIIO <- disaggregateCols(model$GDP$BEACPIIO, disagg, duplicate = TRUE)
    
    counter <- counter + 1
  }
  
  return(model)
  
}



#' Disaggregate satellite tables from static file based on specs
#' 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param sat The abbreviation for the satellite table.
#' 
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (model, sattable, sat){
  
  # For each disaggregation:
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    default_disaggregation <- FALSE
    # If satellite table data is provided for the new sector assign it here
    if(!is.null(disagg$EnvFileDF)){
      new_sector_totals <- disagg$EnvFileDF
      # Select only those rows from the disaggregation env file that apply for this satellite table
      new_sector_totals <- subset(new_sector_totals, SatelliteTable==sat$Abbreviation, colnames(sattable))
      if(nrow(new_sector_totals)==0){
        logging:loginfo(paste0("Warning: No data found for disaggregation of ",sat))
        default_disaggregation <- TRUE
      }
      else{
        # Check for errors in sattelite table
        included_sectors <- unique(new_sector_totals[,"SectorCode"])
        if (!identical(sort(included_sectors),sort(disagg$DisaggregatedSectorCodes))){
          logging::loginfo("Error: Satellite table does not include all disaggregated sectors")
        }
        
        # Append to the main dataframe
        sattable <- rbind(sattable,new_sector_totals)
      }
    }
    else{
      default_disaggregation <- TRUE
    }
    
    if(default_disaggregation){
      # Subset the totals from the original sector
      old_sector_totals <- subset(sattable, SectorCode==disagg$OriginalSectorCode, colnames(sattable))
      
      i<-0
      for (new_sector in disagg$DisaggregatedSectorCodes){
        i<-i+1
        new_sector_totals <- old_sector_totals
        new_sector_totals$SectorCode <- disagg$DisaggregatedSectorCodes[[i]]
        new_sector_totals$SectorName <- disagg$DisaggregatedSectorNames[[i]]

        # If satellite table is disaggregated proportional to gross output do that here
        if(!is.null(disagg$MakeFileDF)){
          GrossOutputAlloc <- subset(disagg$MakeFileDF, 
                                     (IndustryCode == disagg$OriginalSectorCode & CommodityCode == new_sector))
          if(nrow(GrossOutputAlloc)==0){
            allocation <- 0
          }
          else{
            allocation <- GrossOutputAlloc$PercentMake
          }
          new_sector_totals$FlowAmount <- (new_sector_totals$FlowAmount * allocation)
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
    #TODO: maybe get rid of predefined and user defined and just go by whether .csv files are included in the yml file in the if statement?
    if(disaggType == "Predefined"){
      
      disaggTable <- UniformMakeDisagg(model, disagg)
      
      
    } else if(disaggType == "Userdefined"){
     
      disaggTable <- SpecifiedMakeDisagg(model, disagg)
      
    } else {
      
      logging::loginfo("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
}

#' Disaggregate Use table based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' 
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateUseTable <- function (model, domestic = FALSE){
  
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
    
    #disaggregation can be of types "Predefined" or "UserDefined". 
    #TODO: maybe get rid of predefined and user defined and just go by whether .csv files are included in the yml file in the if statement?
    if(disaggType == "Predefined"){
      
      disaggTable <- UniformUseDisagg(model, disagg, domestic)
      
      
    } else if(disaggType == "Userdefined"){
      
      disaggTable <- SpecifiedUseDisagg(model, disagg, domestic)
      
    } else {
      
      logging::loginfo("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
}

#' Disaggregate make table uniformly based on the number of new sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' 
#' @return A standardized make table with old sectors removed and new, uniformly disaggregated sectors added.
UniformMakeDisagg <- function (model, disagg){
  
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
  
  
  #------------------------------------------------------------------------------
  return(disaggTable)
  
}

UniformUseDisagg <- function(model, disagg, domestic = FALSE){
  
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

disaggregateRows <- function (RowVectors, disagg_specs, duplicate=FALSE, notUniform = FALSE){
  
  originalColIndex <- which(colnames(RowVectors)==disagg_specs$OriginalSectorCode)
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  ColVector <- RowVectors[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
  disaggCols <- disaggregateCol (ColVector, disagg_specs, duplicate, notUniform)
  
  disaggRows <- cbind(RowVectors[,1:originalColIndex-1],  #from 1st col to col right before disaggregation
                      disaggCols,                         #insert disaggregated cols
                      RowVectors[,-(1:originalColIndex)]) #include all cols except from 1s col to disaggregated col
  
  return(disaggRows)
  
}

disaggregateCols <- function (ColVectors, disagg_specs, duplicate=FALSE, notUniform = FALSE ){
  
  originalRowIndex <- which(rownames(ColVectors)==disagg_specs$OriginalSectorCode)
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  RowVector <- ColVectors[originalRowIndex,,drop=FALSE]
  disaggRows <- disaggregateRow (RowVector, disagg_specs, duplicate, notUniform)

  disaggCols <- rbind(ColVectors[1:originalRowIndex-1,,drop=FALSE],  #from 1st row to row right before disaggregation
                      disaggRows,                                    #insert disaggregated rows
                      ColVectors[-(1:originalRowIndex),,drop=FALSE]) #include all rows except from 1s row to disaggregated row
  
  return(disaggCols)
  
}

disaggregateRow <- function (originalRowVector, disagg_specs, duplicate = FALSE, notUniform = FALSE){
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (duplicate){
    
    #For handling CPI. Just copy the CPI values of the original sector to for all the disaggregated sectors.
    disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
      
  }else if(notUniform){
    
    percentages <- getDisaggCommodityPercentages(disagg_specs)#get defaul disaggregated industry percentages
    disaggRows <- originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),, drop=FALSE]#repeat the original vector numNewSector times
    disaggRows <- disaggRows * percentages[,3]#multiply the values in the repeated vector by the default percentages to get values allocated by industry totals
    
  }else{
    
    #Create new rows with the uniform values
    uniformRowVector <- originalRowVector/numNewSectors
    disaggRows <-uniformRowVector[rep(seq_len(nrow(uniformRowVector)), numNewSectors),,drop=FALSE]
    
  }

  
  #Rename rows to use the disaggregated codes
  rownames(disaggRows) <- disagg_specs$DisaggregatedSectorCodes
  
  return(disaggRows)
}

#TODO: Make sure to update disaggregateCOl and DisaggregateRow to handle duplicate flag appropriattely (this is used for CPI and GDP disaggregation)
disaggregateCol <- function (originalColVector, disagg_specs, duplicate = FALSE, notUniform = FALSE){
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (duplicate){
      #For handling CPI. Just copy the CPI values of the original sector to for all the disaggregated sectors.
      disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
  }
  
  else if(notUniform){
    
    percentages <- getDisaggIndustryPercentages(disagg_specs)#get defaul disaggregated industry percentages
    disaggCols <- originalColVector[, rep(seq_len(ncol(originalColVector)), numNewSectors)]#repeat the original vector numNewSector times
    disaggCols <- disaggCols * t(percentages[,3])#multiply the values in the repeated vector by the default percentages to get values allocated by industry totals
    
  }else{
    
    #Create new cols with the uniform values
    uniformColVector <- originalColVector/numNewSectors
    disaggCols <- uniformColVector[, rep(seq_len(ncol(uniformColVector)), numNewSectors)]
    
  }
  

  
  #Rename cols to use the disaggregted codes
  colnames(disaggCols) <- disagg_specs$DisaggregatedSectorCodes
  
  return(disaggCols)
}

#' Disaggregate the MasterCrosswalk to include the new sectors for disaggregation
#' @param crosswalk MasterCrosswalk from NAICS to BEA for the specified detail level. columns = "NAICS" and "BEA"
#' 
#' @return crosswalk with new sectors added.
disaggregateMasterCrosswalk <- function (crosswalk, disagg){
  # update the crosswalk by updating the BEA codes for disaggregation or adding new NAICS_like codes
    updated_cw <- disagg$NAICSSectorCW[, c("NAICS_2012_Code","USEEIO_Code")]
    names(updated_cw)[names(updated_cw)=='NAICS_2012_Code'] <- "NAICS"
    names(updated_cw)[names(updated_cw)=='USEEIO_Code'] <- "BEA"

    crosswalk <- merge(crosswalk, updated_cw, by = "NAICS", all = TRUE)
    crosswalk$BEA <- ifelse(is.na(crosswalk$BEA.y),crosswalk$BEA.x,crosswalk$BEA.y)
    crosswalk <- crosswalk[,c("NAICS","BEA")]
  
  return(crosswalk)
}

##-------------------------------TODO: Move functions below to a new file, specifiedDisaggregateFunctions?

#' Disaggregate make table based on the allocations specified in the files referenced in the diaggregation specs.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' 
#' @return A standardized make table with old sectors removed and new disaggregated sectors added based on the allocations in the disaggregation specs.
SpecifiedMakeDisagg <- function (model, disagg){
  
  if(!is.null(disagg$MakeFileDF)){
    
    #Local variable for original sector code
    originalSectorCode <- disagg$OriginalSectorCode
    #Local variable for new sector codes
    newSectorCodes <- disagg$DisaggregatedSectorCodes
    #Local variable for Make table allocations
    makeAllocations <- disagg$MakeFileDF
    
    ##Extract data from Disaggregation csv
    #Industry allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- getDisaggIndustryPercentages(disagg)
    
    #Allocations for make intersection. Get rows of DF where only new sector codes are present in both the industryCode and commodityCode columns. 
    intersectionPercentages <-subset(makeAllocations, IndustryCode %in% newSectorCodes & CommodityCode %in% newSectorCodes)
    
    #Allocations for column (commodity) disaggregation. 
    #Get rows of the DF which do not contain the original sector code or the new sector codes in the industry column (e.g., get only non 562 sector codes when doing waste disaggregation),
    #and where only the new sector codes are present in the commodity column.
    colPercentages <- subset(makeAllocations, !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes) & CommodityCode %in% newSectorCodes)
    
    #Allocations for the row (industry) disaggregation. Get all rows of the DF where new sector codes are in the industryCode column, and neither the original nor new sector codes are in the commodityColumn. 
    rowsPercentages <- subset(makeAllocations, IndustryCode %in% newSectorCodes & !(CommodityCode %in% originalSectorCode) & !(CommodityCode %in% newSectorCodes))
    
    
    ###Disaggregate Make Rows, Columns, and Intersection while using the allocation data extracted from the Disaggregationcsv. 
    #Assigning allocations for disaggregated rows
    allocRowDF  <- DisaggAllocations(model,disagg,rowsPercentages,"MakeRow")
    
    #Assignning allocation for disaggregated columns
    AllocColDF <- DisaggAllocations(model,disagg,colPercentages,"MakeCol") 
    
    #Assigning allocations for disaggregated intersection
    AllocIntersectionDF <- DisaggAllocations(model,disagg,intersectionPercentages,"MakeIntersection")
    
    #----------------------- code shared with uniformMakeDisagg
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
    
    #-----------------------end of code shared with uniformMakeDisagg
    
    #DisaggMake <- AssembleMake(originalMake, originalRowIndex, originalColIndex, AllocColDF, allocRowDF, AllocIntersectionDF)
    DisaggMake <- AssembleTable(originalMake, originalRowIndex, originalColIndex, AllocColDF, allocRowDF, AllocIntersectionDF)
    
  }else{
    
    #todo: error handling, no csv was read in terminate execution
    DisaggMake <- NULL;
  }
  #End of if(!is.null(disagg$MakeFileDF)) loop
  
  return(DisaggMake)
  
}#End of specifiedMakeDisagg Function


#' Disaggregate make table based on the allocations specified in the files referenced in the diaggregation specs.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param domestic Flag that indicates where to use the Domestic Use or UseTrasanctions table
#' 
#' @return A standardized make table with old sectors removed and new disaggregated sectors added based on the allocations in the disaggregation specs.
SpecifiedUseDisagg <- function (model, disagg, domestic = FALSE){
  
  if(!is.null(disagg$UseFileDF)){
    
    #Local variable for original sector code
    originalSectorCode <- disagg$OriginalSectorCode
    #Local variable for new sector codes
    newSectorCodes <- disagg$DisaggregatedSectorCodes
    #Local variable for Make table allocations
    UseAllocations <- disagg$UseFileDF
    #Column names in Final Demand
    fdColNames <- colnames(model$FinalDemand)
    
    ##Extract data from Disaggregation csv
    #Commodity allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- getDisaggCommodityPercentages(disagg)
    
    #Allocations for intersection. Get rows of DF where only new sector codes are present in both the industryCode and commodityCode columns. 
    intersectionPercentages <-subset(UseAllocations, IndustryCode %in% newSectorCodes & CommodityCode %in% newSectorCodes)
    
    #Allocations for column (commodity) disaggregation. 
    #Get rows of the DF which do not contain the original sector code or the new sector codes in the commodity column (e.g., get only non 562 sector codes when doing waste disaggregation),
    #and where only the new sector codes are present in the industry column.
    colPercentages <- subset(UseAllocations, !(CommodityCode %in% originalSectorCode) & !(CommodityCode %in% newSectorCodes) & IndustryCode %in% newSectorCodes)
    
    #Allocations for the row (commodity) disaggregation. Get all rows of the DF where:
    #new sector codes are in the CommodityCode column; the FD column codes are not in the IndustryCode; 
    #and neither the original nor new sector codes are in the IndustryCode column. 
    #rowsPercentages <- subset(UseAllocations, CommodityCode %in% newSectorCodes & !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes))
    rowsPercentages <- subset(UseAllocations, CommodityCode %in% newSectorCodes & !(IndustryCode %in% fdColNames) & !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes))
    
    tempbreak <-1;
    
    ###Disaggregate Make Rows, Columns, and Intersection while using the allocation data extracted from the Disaggregationcsv. 
    #Assigning allocations for disaggregated rows
    allocRowDF  <- DisaggAllocations(model,disagg,rowsPercentages,"UseRow", domestic)
    
    #Assignning allocation for disaggregated columns
    AllocColDF <- DisaggAllocations(model,disagg,colPercentages,"UseCol", domestic) 
   
    tempbreak <-2;
    
    #Assigning allocations for disaggregated intersection
    AllocIntersectionDF <- DisaggAllocations(model,disagg,intersectionPercentages,"UseIntersection", domestic)
    
    #-----------------------code shared with uniformUseDisagg
    
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
    
    
    #-----------------------uniformUseDisagg
    
    DisaggUse <- AssembleTable(originalUse, originalRowIndex, originalColIndex, AllocColDF, allocRowDF, AllocIntersectionDF)
    
  }else{
    
    #todo: error handling, no csv was read in terminate execution
    DisaggUse <- NULL;
  }
  #End of if(!is.null(disagg$MakeFileDF)) loop
  
  return(DisaggUse)
  
}#End of specifiedUseDisagg Function

#' @param OriginalMake Dataframe. The original Make table before disaggregation
#' @param originalRowIndex Integer. The row index, in the original Make table, of the sector to be disaggregated
#' @param OriginalColIndex Integer. The column index, in the original Make table, of the sector to be disaggregated
#' @param disaggCols Dataframe. Previously disaggregated columns of the Make table.
#' @param disaggRows Dataframe. Previously disaggregated rows of the Make table.
#' @param disaggIntersecion Dataframe. Previously disaggregated intersection of the Make table.
#'d
#' @return The Make table as a dataframe with the disaggregated rows, columns, and intersection included
AssembleMake <- function (originalMake, originalRowIndex, originalColIndex, disaggCols, disaggRows, disaggIntersection){
  
  
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

  return(disaggTable)

}



#' @param OriginalTable Dataframe. The original table before disaggregation
#' @param originalRowIndex Integer. The row index, in the original table, of the sector to be disaggregated
#' @param OriginalColIndex Integer. The column index, in the original table, of the sector to be disaggregated
#' @param disaggCols Dataframe. Previously disaggregated columns of the table.
#' @param disaggRows Dataframe. Previously disaggregated rows of the table.
#' @param disaggIntersecion Dataframe. Previously disaggregated intersection of the table.
#'d
#' @return The Make table as a dataframe with the disaggregated rows, columns, and intersection included
AssembleTable <- function (originalTable, originalRowIndex, originalColIndex, disaggCols, disaggRows, disaggIntersection){
  
  
  #Assembling all columns above disaggregated rows, including all disaggregated columns
  disaggTable <- cbind(originalTable[1:originalRowIndex-1,1:originalColIndex-1], #above diagg rows, from 1st col to col right before disaggregation
                       disaggCols[1:originalRowIndex-1,],                        #insert disaggregated cols before disaggregated rows
                       originalTable[1:originalRowIndex-1,-(1:originalColIndex)]) #include all cols except from 1st col to disaggregated col
  
  #Inserting intersection into disaggregated rows
  disaggRows <- cbind(disaggRows[,1:originalColIndex-1],  #from 1st col to col right before disaggregation
                      disaggIntersection,                 #insert disaggregated intersection
                      disaggRows[,-(1:originalColIndex)]) #include all cols except from 1s col to disaggregated col
  
  #Appending rest of original rows to partially assembled DMake
  disaggTable <- rbind(disaggTable,disaggRows)
  
  #Assembling all columns below disaggregated rows, including all disaggregated columns
  disaggTableBottom <- cbind(originalTable[-(1:originalRowIndex),1:originalColIndex-1],  #below disagg rows, from 1st col to col right before disaggregation
                             disaggCols[-(1:originalRowIndex),],                        #insert disaggregated cols below disaggregated rows
                             originalTable[-(1:originalRowIndex),-(1:originalColIndex)]) #below disagg rows, all columns after disagg columns 
  
  #Appeding bottom part of the table to top part of the table
  disaggTable <- rbind(disaggTable, disaggTableBottom)
  
  return(disaggTable)
  
}

#' Allocate values specified by the .yml disaggregation specs to the correct places in a disaggregated row/column of the Use/Make tables. 
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param allocPercentages Dataframe. A subset of the dataframe that contains the percentages to allocate to specific industry and commodity combinations in the disaggregated vector. Parameter use coordinated with @param VectorToDisagg.
#' @param vectorToDisagg String. A parameter to indicate what table and what part of that table is being disaggregated (e.g. "MakeCol" or "Intersection") 
#' @param domestic. Boolean. Flag to indicate where to use the DomesticUse or UseTransactions table
#' 
#' @return A dataframe with the values specified in the disaggSpecs assigned to the correct Make or Use table indeces.
DisaggAllocations <- function (model, disagg, allocPercentages, vectorToDisagg, domestic = FALSE){
  
  #Local variable for new sector codes
  newSectorCodes <- disagg$DisaggregatedSectorCodes
  numNewSectors <- length(newSectorCodes)
  #Local variable for original sector code
  originalSectorCode <- disagg$OriginalSectorCode
  

  #These different if blocks are needed because of the different dimensions of the manual and default allocation vectors needed for disaggregating 
  #the Make and Use rows and columns. Each block initializes the manual and default allocation values for the relevant rows or columns.
  if(vectorToDisagg == "MakeRow")
  {
    
    #Set up for manual allocations
    #Get original table
    originalTable <- model$MakeTransactions
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[originalVectorIndex,]
    #Get original row or column sum
    originalVectorSum <- data.frame(rowSums(originalVector))
    
    #Create new rows to store manual allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = ncol(originalTable), nrow = length(newSectorCodes)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- names(originalVector)
    rownames(manualAllocVector) <- newSectorCodes
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2

    #Set up for default allocations
    #Get default percentages (i.e. for non-manual allocation) 
    #Industry allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- subset(disagg$MakeFileDF, IndustryCode %in% originalSectorCode) #get all rows from the MakeFileDF that have the OriginalSectorCode in the IndustryCode column  

    #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
    if(nrow(defaultPercentages)==0){
      
      defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))#assigning default disaggregation percentage totals assuming uniform split
     
    }else{
      
      defaultPercentages <- defaultPercentages[, 3, drop=FALSE]#Extracting the column with the percentages
    }
    
    #Create new rows to store default allocation values by copying the original row a number of times equal to the number of new sectors
    defaultAllocVector <- rbind(originalVector, originalVector[rep(1,numNewSectors-1),])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- defaultAllocVector*defaultPercentages[,1]
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- names(originalVector)
    rownames(defaultAllocVector) <- newSectorCodes
    
  }else if(vectorToDisagg == "MakeCol"){
    
    #Get original table
    originalTable <- model$MakeTransactions
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[,originalVectorIndex, drop = FALSE]
    #Get original row or column sum
    originalVectorSum <- data.frame(colSums(originalVector))
    
    #Create new cols to store allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = length(newSectorCodes), nrow = nrow(originalTable)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- newSectorCodes
    rownames(manualAllocVector) <- rownames(originalVector)
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2
    
    #Set up for default allocations
    #Get default percentages (i.e. for non-manual allocation) 
    #Industry allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- subset(disagg$MakeFileDF, IndustryCode %in% originalSectorCode) #get all rows from the MakeFileDF that have the OriginalSectorCode in the IndustryCode column
    
    #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
    if(nrow(defaultPercentages)==0){
      
      defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))#assigning default disaggregation percentage totals assuming uniform split
      
    }else{
      
      defaultPercentages <- defaultPercentages[, 3, drop=FALSE]#Extracting the column with the percentages
    }
    
    #Create new columns to store default allocation values by copying the original column a number of times equal to the number of new sectors
    defaultAllocVector <- cbind(originalVector, originalVector[,rep(1,numNewSectors-1)])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- data.frame(t(t(defaultAllocVector)*defaultPercentages[,1]))
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- newSectorCodes
    rownames(defaultAllocVector) <- rownames(originalVector)
    
  }else if(vectorToDisagg == "MakeIntersection"){
    
    #Get original table
    originalTable <- model$MakeTransactions
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalRowIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
    originalColIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[originalRowIndex,originalColIndex, drop=FALSE]
    #Get original row or column sum
    originalVectorSum <- data.frame(colSums(originalVector))
    
    #Create new intersection to store allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = length(newSectorCodes), nrow = length(newSectorCodes)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- newSectorCodes
    rownames(manualAllocVector) <- newSectorCodes
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2
    
    #Set up for default allocations
    #Get default percentages (i.e. for non-manual allocation) 
    #Industry allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- subset(disagg$MakeFileDF, IndustryCode %in% originalSectorCode) #get all rows from the MakeFileDF that have the OriginalSectorCode in the IndustryCode column
    
    #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
    if(nrow(defaultPercentages)==0){
      
      defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))#assigning default disaggregation percentage totals assuming uniform split
      
    }else{
      
      defaultPercentages <- defaultPercentages[, 3, drop=FALSE]#Extracting the column with the percentages
    }
    
    #Create a dataframe to store values for the intersection. This dataframe is of dimensions [numNewSectors, 1]
    defaultAllocVector <- data.frame(originalVector[rep(1,numNewSectors),])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- defaultAllocVector*defaultPercentages[,1]
    
    #Diagonalize the populated column vector
    defaultAllocVector <- diag(defaultAllocVector[,1],numNewSectors,numNewSectors)
    defaultAllocVector <- data.frame(defaultAllocVector)

    
    #rename rows and columns
    colnames(defaultAllocVector) <- newSectorCodes
    rownames(defaultAllocVector) <- newSectorCodes
    
  }else if(vectorToDisagg == "UseRow"){
    
    #get original table
    if(domestic == TRUE){
      
      originalTable <- model$DomesticUseTransactions
    
    }else{
      
      originalTable <- model$UseTransactions
    
    }
    
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[originalVectorIndex,]
    #Get original row or column sum
    originalVectorSum <- data.frame(rowSums(originalVector))
    
    #Create new rows to store manual allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = ncol(originalTable), nrow = length(newSectorCodes)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- names(originalVector)
    rownames(manualAllocVector) <- newSectorCodes
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 2
    allocPercentagesColIndex <- 1
    
    #Set up for default allocations
    #Get default percentages (i.e. for non-manual allocation) 
    #Commodity allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- subset(disagg$UseFileDF, CommodityCode %in% originalSectorCode) #get all rows in UseAllocations that have the OriginalSectorCode in the CommodityCode column
    
    #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
    if(nrow(defaultPercentages)==0){
      
      defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))#assigning default disaggregation percentage totals assuming uniform split
      
    }else{
      
      defaultPercentages <- defaultPercentages[, 3, drop=FALSE]#Extracting the column with the percentages
    }
    
    #Create new rows to store default allocation values by copying the original row a number of times equal to the number of new sectors
    defaultAllocVector <- rbind(originalVector, originalVector[rep(1,numNewSectors-1),])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- defaultAllocVector*defaultPercentages[,1]
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- names(originalVector)
    rownames(defaultAllocVector) <- newSectorCodes
    
  }else if (vectorToDisagg == "UseCol"){
    
    #get original table
    if(domestic == TRUE){
      
      originalTable <- model$DomesticUseTransactions
      
    }else{
      
      originalTable <- model$UseTransactions
      
    }
    
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[,originalVectorIndex, drop = FALSE]
    #Get original row or column sum
    originalVectorSum <- data.frame(colSums(originalVector))
    
    #Create new cols to store allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = length(newSectorCodes), nrow = nrow(originalTable)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- newSectorCodes
    rownames(manualAllocVector) <- rownames(originalVector)
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 2
    allocPercentagesColIndex <- 1
    
    #Set up for default allocations
    #Get default percentages (i.e. for non-manual allocation) 
    #Commodity allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- subset(disagg$UseFileDF, CommodityCode %in% originalSectorCode) #get all rows in UseAllocations that have the OriginalSectorCode in the CommodityCode column
    
    #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
    if(nrow(defaultPercentages)==0){
      
      defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))#assigning default disaggregation percentage totals assuming uniform split
      
    }else{
      
      defaultPercentages <- defaultPercentages[, 3, drop=FALSE]#Extracting the column with the percentages
    }
    
    #Create new columns to store default allocation values by copying the original column a number of times equal to the number of new sectors
    defaultAllocVector <- cbind(originalVector, originalVector[,rep(1,numNewSectors-1)])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- data.frame(t(t(defaultAllocVector)*defaultPercentages[,1]))
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- newSectorCodes
    rownames(defaultAllocVector) <- rownames(originalVector)
    
  }else if(vectorToDisagg == "UseIntersection"){
    
    #get original table
    if(domestic == TRUE){
      
      originalTable <- model$DomesticUseTransactions
      
    }else{
      
      originalTable <- model$UseTransactions
      
    }
    
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalRowIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
    originalColIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[originalRowIndex,originalColIndex, drop=FALSE]
    #Get original row or column sum
    originalVectorSum <- data.frame(colSums(originalVector))
    
    #Create new intersection to store allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = length(newSectorCodes), nrow = length(newSectorCodes)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- newSectorCodes
    rownames(manualAllocVector) <- newSectorCodes
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2
    
    #Set up for default allocations
    #Get default percentages (i.e. for non-manual allocation) 
    #Industry allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- subset(disagg$UseFileDF, CommodityCode %in% originalSectorCode) #get all rows in UseAllocations that have the OriginalSectorCode in the CommodityCode column
    
    #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
    if(nrow(defaultPercentages)==0){
      
      defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))#assigning default disaggregation percentage totals assuming uniform split
      
    }else{
      
      defaultPercentages <- defaultPercentages[, 3, drop=FALSE]#Extracting the column with the percentages
    }
    
    #Create a dataframe to store values for the intersection. This dataframe is of dimensions [numNewSectors, 1]
    defaultAllocVector <- data.frame(originalVector[rep(1,numNewSectors),])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- defaultAllocVector*defaultPercentages[,1]
    
    #Diagonalize the populated column vector
    defaultAllocVector <- diag(defaultAllocVector[,1],numNewSectors,numNewSectors)
    defaultAllocVector <- data.frame(defaultAllocVector)
    
    #rename rows and columns
    colnames(defaultAllocVector) <- newSectorCodes
    rownames(defaultAllocVector) <- newSectorCodes
    
  }else{
    #todo
    #error handling
  }
  
  if(nrow(allocPercentages)>0)
  {
    #Loop to assign the manual allocations
    for (r in 1:nrow(allocPercentages)){
      
      #Get data from current row of the data imported from the yml file. 
      rowAlloc <- allocPercentages[r,allocPercentagesRowIndex]
      colAlloc <- allocPercentages[r,allocPercentagesColIndex]
      allocationValue <- allocPercentages[r,3]
      
      #Get the indeces where the allocated values go in new disaggregated rows
      rowAllocIndex <- which(rownames(manualAllocVector)==rowAlloc)
      colAllocIndex <- which(colnames(manualAllocVector)==colAlloc)
      
      #Check for indexing errors
      if(length(rowAllocIndex)==0L){
        logging::loginfo(paste("rowAlloc not found, no allocation made for row", rowAlloc, sep=" ", "in table."))
        
      }
      
      if(length(colAllocIndex)==0L){
        logging::loginfo(paste("colAlloc not found, no allocation made for column", colAlloc, sep=" ", "in table."))
        
      }
      
      
      #Calculate value based on allocation percent
      if(vectorToDisagg == "MakeRow" || vectorToDisagg == "UseRow"){
        
        value <- originalVector[colAllocIndex]*allocationValue
        
      }else if(vectorToDisagg=="MakeCol" || vectorToDisagg=="UseCol"){
        
        value <- originalVector[rowAllocIndex, 1, drop = FALSE]*allocationValue #to keep value as a dataframe
        
      }else if(vectorToDisagg == "MakeIntersection" || vectorToDisagg=="UseIntersection"){
        
        value <- originalVector[1, 1, drop = FALSE]*allocationValue #to keep value as a dataframe. Should be a 1x1 DF
        
      }
      
      #If either rowAlloc or column are not valid values, set value to 0 to avoid a runtime error
      if(ncol(value)==0){
        
        value <- 0
      }
      
      #Assign value to correct index
      manualAllocVector[rowAllocIndex, colAllocIndex] <- value
      
      
    }
  }else
  {
    logging::loginfo(paste("rowAlloc not found, no allocation made for", vectorToDisagg, sep=" "))
    
  }

  
  #replace all NAs with 0
  manualAllocVector[is.na(manualAllocVector)] <-0
  
  #Replace values in the default allocation vector with values from the Manual allocation vector to finalize the vector disaggregation.

  if(vectorToDisagg == "MakeRow"|| vectorToDisagg == "MakeIntersection" || vectorToDisagg=="UseRow" || vectorToDisagg =="UseIntersection")
  {
    #assumption is that all columns where there was a manual allocation sum up to the value in the original row/column index.
    manualIndeces <- data.frame(which(colSums(manualAllocVector) !=0 ))
    
    if(nrow(manualIndeces) > 0){
      
      for (i in 1:nrow(manualIndeces)){
        
        #replace values from manual allocation into default allocation
        tempVector <- manualAllocVector[, manualIndeces[i,1], drop=FALSE]
        defaultAllocVector[, manualIndeces[i,1]] <- tempVector
      }
      
    }

  }else if(vectorToDisagg == "MakeCol" || vectorToDisagg == "UseCol"){
    
    #assumption is that all rows where there was a manual allocation sum up to the value in the original row/column index.
    manualIndeces <- data.frame(which(rowSums(manualAllocVector) !=0 ))
    
    if(nrow(manualIndeces) > 0){
      
      for (i in 1:nrow(manualIndeces)){
        
        #replace values from manual allocation into default allocation
        tempVector <- manualAllocVector[manualIndeces[i,1], , drop=FALSE]
        defaultAllocVector[manualIndeces[i,1],] <- tempVector
      }
      
    }

  }
  else{
    
    manualIndeces <- NA;#temporary values 
  }
  

  return(defaultAllocVector)
  
}#end of DisaggAllocations function


#' @param disagg Specifications for disaggregating the current Model
#' 
#' @return A dataframe with the default disaggregation percentges for the Industries of the current model
getDisaggIndustryPercentages <-function(disagg){
  
  defaultPercentages <- subset(disagg$MakeFileDF, IndustryCode %in% disagg$OriginalSectorCode)#get all rows in MakefileDF that have the OriginalSectorCode in the IndustryCode column
  
  return(defaultPercentages)
}

#' @param disagg Specifications for disaggregating the current Model
#' 
#' @return A dataframe with the default disaggregation percentges for the Commodities of the current model
getDisaggCommodityPercentages <- function(disagg){
  
  defaultPercentages <- subset(disagg$UseFileDF, CommodityCode %in% disagg$OriginalSectorCode) #get all rows in UseAllocations that have the OriginalSectorCode in the CommodityCode column
  
  
}


#' Allocate values specified by the .yml disaggregation specs to the correct places in a disaggregated row/column of the Use/Make tables. 
#' @param  df0 data frame 0
#' 
#' @return 
# Validate df1 against df0 based on specified conditions
compareDisaggValues <- function(df0, df1, abs_diff = TRUE, tolerance) {
  # Define comparison rule
  if (abs_diff) {
    rule <- validate::validator(abs(df1 - df0) <= tolerance)
  } else {
    rule <- validate::validator(df1 - df0 <= tolerance)
  }
  
  rule <- validate::validator(df1 / df0 <= tolerance)
  # Compare df1 against df0
  confrontation <- validate::confront(df1, rule, ref = list(df0 = df0))
  confrontation <- validate::as.data.frame(confrontation)
  validation <- merge(confrontation, validate::as.data.frame(rule))
  rownames(validation) <- confrontation$rownames <- rownames(confrontation)
  validation$name <- NULL
  return(list("confrontation" = confrontation, "validation" = validation))
}


balanceDisagg <- function(model, disagg){
  
  
  #-------------Check for balance------------------------------
  # disaggFullUse <- rbind(model$UseTransactions, model$UseValueAdded)
  # 
  # tempVA <- matrix(0, nrow(model$UseValueAdded), ncol(model$FinalDemand))
  # colnames(tempVA) <- colnames(model$FinalDemand)
  # rownames(tempVA) <- rownames(model$UseValueAdded)
  # fullFD <- rbind(model$FinalDemand, tempVA)
  # 
  # disaggFullUse <- cbind(disaggFullUse, fullFD)
  disaggFullUse <-buildDisaggFullUse(model, disagg)
  
  #Get commodity and/or industry indeces corresponding to the original sector code
  disaggIndustryIndex <- which(rownames(model$MakeTransactions)==disagg$DisaggregatedSectorCode[1])
  disaggCommodityIndex <- which(colnames(model$MakeTransactions)==disagg$DisaggregatedSectorCode[1])
  disaggIndustryEndIndex <- disaggIndustryIndex + length(disagg$DisaggregatedSectorCodes)-1
  disaggCommodityEndIndex <- disaggCommodityIndex + length(disagg$DisaggregatedSectorCodes)-1
  
  #Get Disaggregated Industry totals from both Make and Use tables
  disaggMakeIndTotals <- data.frame(rowSums(model$MakeTransactions[disaggIndustryIndex:disaggIndustryEndIndex,]))
  disaggUseIndTotals <- data.frame(colSums(model$UseTransactions[, disaggIndustryIndex:disaggIndustryEndIndex]))
  
  #Get Disaggregated Commodity totals from both Make and Use Tables
  disaggMakeComTotals <- data.frame(colSums(model$MakeTransactions[, disaggCommodityIndex:disaggCommodityEndIndex]))
  disaggUseComTotals <- data.frame(rowSums(model$UseTransactions[disaggCommodityIndex:disaggCommodityEndIndex, ]))
  
  
  #Check Balance of industry totals across tables
  tolerance <- data.frame(matrix(1, nrow = length(disagg$DisaggregatedSectorCodes)))#initialized to number of disaggregated sectors by 1 col; all elements == 1
  tolerance <- tolerance*1E8#set all elements to 1E8 (between 0 and 5% for all sectors, depending on the specific sector)
  
  if(any(abs(disaggUseIndTotals - disaggMakeIndTotals) > tolerance) || any(abs(disaggUseComTotals - disaggMakeComTotals > tolerance))){
    
    #balance. Create FullUse from UseTransanctions, UseValueAdded, and Final Demand, then call ApplyRAS
    
    targetIndTotals <- data.frame(rowSums(model$MakeTransactions))
    FDIndTotals <- data.frame(colSums(model$FinalDemand))
    colnames(FDIndTotals) <- colnames(targetIndTotals) #needed for rbind step
    targetIndTotals <- rbind(targetIndTotals, FDIndTotals)
    
    targetComTotals <- data.frame(colSums(model$MakeTransactions))
    VAComTotals <- data.frame(rowSums(model$UseValueAdded))
    colnames(VAComTotals) <- colnames(targetComTotals) #needed for rbind step
    targetComTotals <- rbind(targetComTotals, VAComTotals)
    
    balancedDisaggFulluse <- applyRAS(data.matrix(disaggFullUse), targetComTotals[,1], targetIndTotals[,1], relative_diff = NULL, absolute_diff = 1E8, max_itr = 1E6)
    
    
  }
  
  #Check Baalance of commodity totals across tables
  
  
  #-------------End of Check for balance-----------------------
}


buildDisaggFullUse <- function(model, disagg){
  
  disaggFullUse <- rbind(model$UseTransactions, model$UseValueAdded)
  
  tempVA <- matrix(0, nrow(model$UseValueAdded), ncol(model$FinalDemand))
  colnames(tempVA) <- colnames(model$FinalDemand)
  rownames(tempVA) <- rownames(model$UseValueAdded)
  fullFD <- rbind(model$FinalDemand, tempVA)
  
  disaggFullUse <- cbind(disaggFullUse, fullFD)
  
  return(disaggFullUse)
  
}