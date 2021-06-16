#' Disaggregate a model based on specified source file
#' @param model Model file loaded with IO tables
#' @return A disaggregated model.
disaggregateModel <- function (model){

  for (disagg in model$specs$DisaggregationSpecs){
    disaggregationConfigFile <- disagg
    logging::loginfo(paste("Reading disaggregation for", disaggregationConfigFile, sep=" "))
    model$DisaggregationSpecs <- getConfiguration(disaggregationConfigFile, "disagg")
  }

  counter = 1
  for (disagg in model$DisaggregationSpecs$Disaggregation){
   
    disagg$NAICSSectorCW <- utils::read.csv(system.file("extdata/disaggspecs", disagg$SectorFile, package = "useeior"),
                                            header = TRUE, stringsAsFactors = FALSE, colClasses=c("NAICS_2012_Code"="character",
                                                                                                  "USEEIO_Code"="character"))
    newNames <- unique(data.frame("SectorCode" = disagg$NAICSSectorCW$USEEIO_Code,
                                  "SectorName"=disagg$NAICSSectorCW$USEEIO_Name,
                                  stringsAsFactors = TRUE))
    disagg$DisaggregatedSectorNames <- as.list(levels(newNames[, 'SectorName']))
    disagg$DisaggregatedSectorCodes <- as.list(levels(newNames[, 'SectorCode']))
    
    #reordering disaggSectorNames and DIsaggSectorCodes to match the mapping in newNames
    disagg$DisaggregatedSectorNames <- as.list(disagg$DisaggregatedSectorNames[match(newNames$SectorName,disagg$DisaggregatedSectorNames)])
    disagg$DisaggregatedSectorCodes <- as.list(disagg$DisaggregatedSectorCodes[match(newNames$SectorCode,disagg$DisaggregatedSectorCodes)])
    
    if(!is.null(disagg$MakeFile)){
      disagg$MakeFileDF <- utils::read.csv(system.file("extdata/disaggspecs", disagg$MakeFile, package = "useeior"),
                                           header = TRUE, stringsAsFactors = FALSE, colClasses=c("IndustryCode"="character",
                                                                                                 "CommodityCode"="character"))}
    if(!is.null(disagg$UseFile)){
      disagg$UseFileDF <- utils::read.csv(system.file("extdata/disaggspecs", disagg$UseFile, package = "useeior"),
                                          header = TRUE, stringsAsFactors = FALSE)}      
    if(!is.null(disagg$EnvFile)){
      disagg$EnvFileDF <- utils::read.csv(system.file("extdata/disaggspecs", disagg$EnvFile, package = "useeior"),
                                          header = TRUE, stringsAsFactors = FALSE, colClasses=c("Sector"="character"))}
    #Need to assign these DFs back to the modelspecs
    model$DisaggregationSpecs$Disaggregation[[counter]] <- disagg
    
    logging::loginfo("Initializing Disaggregation of IO tables...")
    
    #Disaggregating sector lists 
    model$Commodities <- disaggregateSectorDFs(model, disagg, "Commodity")
    model$Industries <- disaggregateSectorDFs(model, disagg, "Industry")

    #Disaggregating main model components
    model$UseTransactions <- disaggregateUseTable(model)
    model$MakeTransactions <- disaggregateMakeTable(model)
    model$FinalDemand <- disaggregateFinalDemand(model, domestic = FALSE)
    model$ValueAdded <- disaggregateVA(model)
    model$DomesticFinalDemand <- disaggregateFinalDemand(model, domestic = TRUE)
    model$DomesticUseTransactions <- disaggregateUseTable(model, domestic = TRUE)
    
    #Balancing model
    if(disagg$DisaggregationType == "Userdefined"){
      model <- balanceDisagg(model, disagg)
    }

    #Disaggregating model$CommodityOutput and model$IndustryOutput objects 
    model <- disaggregateOutputs(model)
    
    #Disaggregating MultiyearIndustryOutput and MultiYearCommodityOutput 
    model$MultiYearCommodityOutput <- disaggregateMultiYearOutput(model, disagg, output_type = "Commodity")
    model$MultiYearIndustryOutput <- disaggregateMultiYearOutput(model, disagg, output_type = "Industry")

    #Disaggregating CPI model objects. Assumption is that the disaggregated sectors have the same CPI values as the original sector. 
    model$MultiYearCommodityCPI <- disaggregateCols(model$MultiYearCommodityCPI, disagg, duplicate = TRUE)
    model$MultiYearIndustryCPI <- disaggregateCols(model$MultiYearIndustryCPI, disagg, duplicate = TRUE)

    #Disaggregating Crosswalk
    model$crosswalk <- disaggregateMasterCrosswalk(model, disagg)
    
    #Disaggreate Margins
    model$Margins <- disaggregateMargins(model, disagg)

    counter <- counter + 1
  }
  
  return(model)
  
}

#' Disaggregate Commodity and Industry Output model objects
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return model A complete EEIO model: a list with USEEIO model components and attributes.
disaggregateOutputs <- function(model)
{

  if(!is.null(model$CommodityOutput))
  {
    model$CommodityOutput <- rowSums(model$UseTransactions)+rowSums(model$FinalDemand)
    
  }
  if(!is.null(model$IndustryOutput))
  {
    model$IndustryOutput <- colSums(model$UseTransactions)+colSums(model$ValueAdded)
  }

  return(model)

}

#' Disaggregate model$Margins dataframe in the main model object
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return newMargins A dataframe which contain the margins for the disaggregated sectors
disaggregateMargins <- function(model, disagg)
{
  originalMargins <- model$Margins
  originalIndex <-  grep(disagg$OriginalSectorCode, model$Margins$Code_Loc)#get row index of the original aggregate sector in the model$Margins object
  originalRow <- model$Margins[originalIndex,]#copy row containing the Margins information for the original aggregate sector
  disaggMargins <-originalRow[rep(seq_len(nrow(originalRow)), length(disagg$DisaggregatedSectorCodes)),,drop=FALSE]#replicate the original a number of times equal to the number of disaggregate sectors
  disaggRatios <- unname(disaggregatedRatios(model, disagg, "Commodity"))#ratios needed to calculate the margins for the disaggregated sectors. Need to unname for compatibility with Rho matrix later in the model build process.
  
  #variable to determine length of Code substring, i.e., code length minus geographic identifer and separator character (e.g. "/US")
  codeLength <- nchar(gsub("/.*", "", disagg$DisaggregatedSectorCodes[1]))
  disaggMargins$Code_Loc <- unlist(disagg$DisaggregatedSectorCodes)#replace Code_Loc values from aggregate sector with Code_Loc values for disaggregated sectors. Need to unlist for compatibility with Rho matrix later in the model build process.
  disaggMargins$SectorCode <- substr(disagg$DisaggregatedSectorCodes,1,codeLength) #replace SectorCode values from aggregate sector with Code_Loc values for disaggregated sectors, except for the geographic identifer
  disaggMargins$Name <- unlist(disagg$DisaggregatedSectorNames)#replace Name values from aggregate sector with Name values for disaggregated sectors.  Need to unlist for compatibility with other functions later in the model build process.
  
  #code below mutlplies the values in the relavant columns of the Margins dataframe by the disaggRatios
  disaggMargins$ProducersValue <- disaggMargins$ProducersValue * disaggRatios
  disaggMargins$Transportation <- disaggMargins$Transportation * disaggRatios
  disaggMargins$Wholesale <- disaggMargins$Wholesale * disaggRatios
  disaggMargins$Retail <- disaggMargins$Retail * disaggRatios
  disaggMargins$PurchasersValue <- disaggMargins$PurchasersValue * disaggRatios
  
  #bind the new values to the original table
  newMargins <- rbind(originalMargins[1:originalIndex-1,], disaggMargins, originalMargins[-(1:originalIndex),])
  
  #update rownames so that the row names of the disaggregated sectors do not contain decimals (e.g., 351.1)
  rownames(newMargins) <- NULL
  newMargins <- newMargins[match(newMargins$SectorCode, model$Commodities$Code), ]
  
  return(newMargins)
}

#' Calculate ratios of throughputs from the disaggregated sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param output_type A string value indicating whether to obtain "Commodity" or "Industry" ratios
#' @return disaggRatios A dataframe which contain the disaggregated ratios for the disaggregated sectors
disaggregatedRatios <- function(model, disagg, output_type = "Commodity")
{
  
  if(output_type == "Industry")
  {
    #Get Index for Disaggregated Industries in the use table
    disaggUseStartIndex <- which(colnames(model$UseTransactions)==disagg$DisaggregatedSectorCodes[1])
    disaggUseEndIndex <- disaggUseStartIndex+length(disagg$DisaggregatedSectorCodes)-1
    
    #calculate industry ratios after disaggregation from Use table
    disaggRatios <- colSums(model$UseTransactions[,disaggUseStartIndex:disaggUseEndIndex]) + colSums(model$ValueAdded[,disaggUseStartIndex:disaggUseEndIndex])
    disaggRatios <- disaggRatios / sum(disaggRatios)
    
    
  }else #assume commodity if industry is not specified
  {
    #Get Index for Disaggregated Commodities in the use table
    disaggUseStartIndex <- which(rownames(model$UseTransactions)==disagg$DisaggregatedSectorCodes[1])
    disaggUseEndIndex <- disaggUseStartIndex+length(disagg$DisaggregatedSectorCodes)-1
    
    #calculate industry ratios after disaggregation from Use table
    disaggRatios <- rowSums(model$UseTransactions[disaggUseStartIndex:disaggUseEndIndex,]) + rowSums(model$FinalDemand[disaggUseStartIndex:disaggUseEndIndex,])
    disaggRatios <- disaggRatios / sum(disaggRatios) 
    
  }
  
  return(disaggRatios)
}

#' Disaggregate Commodity Output model object
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param output_type A string that indicates whether the Commodity or Industry output should be disaggregated
#' @return model A dataframe with the disaggregated GDPGrossOutputIO by year
disaggregateMultiYearOutput <- function(model, disagg, output_type = "Commodity")
{
 
  if(output_type == "Industry")
  {
    originalOutput = model$MultiYearIndustryOutput

    disaggRatios <- disaggregatedRatios(model, disagg, output_type)
  }
  else #assume commodity if industry is not specified
  {
    originalOutput = model$MultiYearCommodityOutput

    disaggRatios <- disaggregatedRatios(model, disagg, output_type)
  }
  
     
   
  #Determine the index of the first disaggregated sector
  originalVectorIndex <- which(rownames(originalOutput)==disagg$OriginalSectorCode)
  #Obtain row with original vector in GDPGrossOutput object
  originalVector <- originalOutput[originalVectorIndex,]
  #Create new rows where disaggregated values will be stored
  disaggOutput <-originalVector[rep(seq_len(nrow(originalVector)), length(disagg$DisaggregatedSectorCodes)),,drop=FALSE]
  
  #apply ratios to values
  disaggOutput <- disaggOutput *t(disaggRatios)
  #rename rows
  rownames(disaggOutput) <- disagg$DisaggregatedSectorCodes
  
  #bind new values to original table
  newOutputTotals <- rbind(originalOutput[1:originalVectorIndex-1,], disaggOutput, originalOutput[-(1:originalVectorIndex),])
  
  
  return(newOutputTotals)
  
}


#' Disaggregate model$Commodity or model$Industry dataframes in the main model object
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param list_type string indicating whether to disaggregate model$Industry or model$Commodity dataframe. 
#' @return newSectors A dataframe which contain the disaggregated model$Commodity or model$Industry objects
disaggregateSectorDFs <- function(model, disagg, list_type)
{

  if(list_type == "Commodity")
  {
    originalList <- model$Commodities
    originalIndex <- grep(disagg$OriginalSectorCode, model$Commodities$Code_Loc)
    newSectors <- data.frame(matrix(ncol = ncol(model$Commodities), nrow = length(disagg$DisaggregatedSectorCodes)))
    names(newSectors) <- names(model$Commodities)#rename colums for the df
  }
  else #assume industry if not specified
  {
    originalList <- model$Industries
    originalIndex <- grep(disagg$OriginalSectorCode, model$Industries$Code_Loc)
    newSectors <- data.frame(matrix(ncol = ncol(model$Industries), nrow = length(disagg$DisaggregatedSectorCodes)))
    names(newSectors) <- names(model$Industries)#rename colums for the df
  }

  #variable to determine length of Code substring, i.e., code length minus geographic identifer and separator character (e.g. "/US")
  codeLength <- nchar(gsub("/.*", "", disagg$DisaggregatedSectorCodes[1]))
  newSectors$Code <- substr(disagg$DisaggregatedSectorCodes,1,codeLength)
  newSectors$Code_Loc <- sapply(disagg$DisaggregatedSectorCodes, paste0, collapse = "")#sapply needed to convert DisaggregatedSectorCodes from list to char vector
  newSectors$Name <- sapply(disagg$DisaggregatedSectorNames, paste0, collapse = "")
  
  newSectors <- rbind(originalList[1:originalIndex-1,],newSectors,originalList[-(1:originalIndex),])
  rownames(newSectors) <- 1:nrow(newSectors)
  
  return(newSectors)
}

#' Disaggregate satellite tables from static file based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param sat The abbreviation for the satellite table.
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (model, sattable, sat){
  
  # For each disaggregation:
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    if(disagg$OriginalSectorCode %in% sattable$Sector){
      default_disaggregation <- FALSE
      # If satellite table data is provided for the new sector assign it here
      if(!is.null(disagg$EnvFileDF)){
        new_sector_totals <- disagg$EnvFileDF
        # Select only those rows from the disaggregation env file that apply for this satellite table
        new_sector_totals <- subset(new_sector_totals, SatelliteTable==sat$Abbreviation, colnames(sattable))
        if(nrow(new_sector_totals)==0){
          logging::logwarn(paste0("No data found for disaggregation of ",sat$Abbreviation," - applying default allocation"))
          default_disaggregation <- TRUE
        }
        else{
          # Check for errors in sattelite table
          included_sectors <- unique(new_sector_totals[,"Sector"])
          if (!identical(sort(included_sectors),sort(unlist(disagg$DisaggregatedSectorCodes)))){
            logging::logwarn("Satellite table does not include all disaggregated sectors")
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
        old_sector_totals <- subset(sattable, Sector==disagg$OriginalSectorCode, colnames(sattable))
        
        if(!nrow(old_sector_totals)==0){
          i<-0
          for (new_sector in disagg$DisaggregatedSectorCodes){
            i<-i+1
            new_sector_totals <- old_sector_totals
            new_sector_totals$Sector <- disagg$DisaggregatedSectorCodes[[i]]
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
        }
  
      }}
      # Remove the old_sector_totals
      sattable_disaggregated <- subset(sattable, Sector!=disagg$OriginalSectorCode)
    }
    else{ # No disaggregation needed
      sattable_disaggregated <- sattable 
    }}
  
  return(sattable_disaggregated)
}

#' Disaggregate make table based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateMakeTable <- function (model){
  
  for (disagg in model$DisaggregationSpecs$Disaggregation){
  
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
  
    #disaggregation can be of types "Predefined" or "UserDefined". 
    if(disaggType == "Predefined"){
      
      disaggTable <- uniformMakeDisagg(model, disagg)
      
      
    } else if(disaggType == "Userdefined"){
     
      disaggTable <- specifiedMakeDisagg(model, disagg)
      
    } else {
      
      logging::logwarn("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
}

#' Disaggregate Use table based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param domestic A logical value indicating whether to disaggregate domestic final demand.
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateUseTable <- function (model, domestic = FALSE){
  
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
    
    #disaggregation can be of types "Predefined" or "UserDefined". 
    if(disaggType == "Predefined"){
      
      disaggTable <- uniformUseDisagg(model, disagg, domestic)
      
      
    } else if(disaggType == "Userdefined"){
      
      disaggTable <- specifiedUseDisagg(model, disagg, domestic)
      
    } else {
      
      logging::logwarn("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
}


#' Disaggregate Final Demand based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param domestic A logical value indicating whether to disaggregate domestic final demand.
#' @return A standardized final demand table with old sectors removed and new sectors with manual and default allocations added.
disaggregateFinalDemand <- function(model, domestic = FALSE)
{

  if(domestic){
    originalFD <-model$DomesticFinalDemand
  }
  else{
    originalFD <-model$FinalDemand
  }

  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
    
    #disaggregation can be of types "Predefined" or "UserDefined". 

    if(disaggType == "Predefined"){
      
      
      disaggTable <- disaggregateCols(originalFD, disagg, duplicate = FALSE, notUniform = FALSE)
      
      
    } else if(disaggType == "Userdefined"){
      
      #Column names in Final Demand
      fdColNames <- colnames(model$FinalDemand)
      #Allocation for FD demand sectors
      FDPercentages <- subset(disagg$UseFileDF, IndustryCode %in% fdColNames)
      #Assigning allocations for FD
      AllocFDDF <- disaggAllocations(model, disagg, FDPercentages, "FinalDemand", domestic)
      
      #Determine number of commodities and industries in originalFD
      nCommodities <- nrow(originalFD)
      nIndustries <- ncol(originalFD) 
      
      #Deterine number of commodities and industries in DisaggSpecs
      numNewSectors <- length(disagg$DisaggregatedSectorCodes) 
      
      #Determine commodity and industry indeces corresponding to the original sector code
      originalRowIndex <- which(rownames(originalFD)==disagg$OriginalSectorCode)
      #originalColIndex <- which(colnames(originalFD)==disagg$OriginalSectorCode)
      
      #Determine end index of disaggregated sectors
      endRowIndex <- originalRowIndex + numNewSectors
      #endColIndex <- originalColIndex + numNewSectors
      

      disaggTable <- rbind(originalFD[1:originalRowIndex-1,], #above diagg rows, all columns
                           AllocFDDF,                        #insert disaggregated rows
                           originalFD[-(1:originalRowIndex),]) #include all rows except from 1st row to disaggregated row
      
      
    } else {
      
      logging::logwarn("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
  
}

#' Disaggregate Value Added based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A standardized Vale Added table with old sectors removed and new sectors with manual and default allocations added.
disaggregateVA <- function(model)
{
  
  
  for (disagg in model$DisaggregationSpecs$Disaggregation){
    
    #specify type of disaggregation
    disaggType = disagg$DisaggregationType
    
    #disaggregation can be of types "Predefined" or "UserDefined". 
    
    if(disaggType == "Predefined"){
      
      disaggTable <- disaggregateRows(model$ValueAdded, disagg, duplicate = FALSE, notUniform = FALSE)

      
    } else if(disaggType == "Userdefined"){
      
      #Row names in value added
      VARowNames <- rownames(model$ValueAdded)
      #Allocation for FD demand sectors
      VAPercentages <- subset(disagg$UseFileDF, CommodityCode %in% VARowNames)#if VA codenames are in the CommodityCode Column of the csv.
      #Assigning allocations for FD
      AllocVADF <- disaggAllocations(model, disagg, VAPercentages, "ValueAdded", domestic)#need to edit disaggAllocations to handle value added.

      ####assembling disaggregated VA

      #Determine number of commodities and industries in originalFD
      nCommodities <- nrow(model$ValueAdded)
      nIndustries <- ncol(model$ValueAdded)

      #Deterine number of commodities and industries in DisaggSpecs
      numNewSectors <- length(disagg$DisaggregatedSectorCodes)

      #Determine commodity and industry indeces corresponding to the original sector code
      #originalRowIndex <- which(rownames(model$ValueAdded)==disagg$OriginalSectorCode)
      originalColIndex <- which(colnames(model$ValueAdded)==disagg$OriginalSectorCode)

      #Determine end index of disaggregated sectors
      #endRowIndex <- originalRowIndex + numNewSectors
      endColIndex <- originalColIndex + numNewSectors

      
      tablePartOne <- model$ValueAdded[, 1:originalColIndex-1]#all rows, columns to the left of diagg col
      tablePartTwo <- model$ValueAdded[,-(1:originalColIndex)]#all rows, all columns except cols to left of disagg col
      
      disaggTable <- cbind(tablePartOne, AllocVADF, tablePartTwo)

      
    } else {
      
      logging::logwarn("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
  
}

#' Disaggregate make table uniformly based on the number of new sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return A standardized make table with old sectors removed and new, uniformly disaggregated sectors added.
uniformMakeDisagg <- function (model, disagg){
  
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
  
  return(disaggTable)
  
}


#' Disaggregate use table uniformly based on the number of new sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param domestic boolean, indicates whether use disagg is for domestic model or full model; FALSE is default
#' @return A standardized use table with old sectors removed and new, uniformly disaggregated sectors added.
uniformUseDisagg <- function(model, disagg, domestic = FALSE){
  
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
      
    }else {
      
      logging::logwarn("Disaggregation not performed, type not defined")
      break
    }
  }
  
  return(disaggTable)
}

#' Disaggregate multiple rows from a table.
#' @param RowVectors A dataframe containing the rows to disaggregate
#' @param disagg_specs Specifications for disaggregating the current Table
#' @param duplicate A flag that indicates whether the disaggregated rows are to be duplicated or not (e.g. for CPI values)
#' @param notUniform A flag that indicates whether the disaggregated rows are to be disaggregated in uniform manner or not
#' @return A dataframe with disaggregated rows.
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

#' Disaggregate multiple columns from a table.
#' @param ColVectors A dataframe containing the columns to disaggregate
#' @param disagg_specs Specifications for disaggregating the current Table
#' @param duplicate A flag that indicates whether the disaggregated columns are to be duplicated or not (e.g. for CPI values)
#' @param notUniform A flag that indicates whether the disaggregated columns are to be disaggregated in uniform manner or not
#' @return A dataframe with disaggregated columns.
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


#' Disaggregate a single row from a table.
#' @param originalRowVector A dataframe containing the row to disaggregate
#' @param disagg_specs Specifications for disaggregating the current Table
#' @param duplicate A flag that indicates whether the disaggregated row is to be duplicated or not (e.g. for CPI values)
#' @param notUniform A flag that indicates whether the disaggregated row is to be disaggregated in uniform manner or not
#' @return A dataframe with the original row disaggregated.
disaggregateRow <- function (originalRowVector, disagg_specs, duplicate = FALSE, notUniform = FALSE){
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (duplicate){
    
    #For handling CPI. Just copy the CPI values of the original sector to for all the disaggregated sectors.
    disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
      
  }else if(notUniform){
    
    percentages <- getDisaggCommodityPercentages(disagg_specs)#get default disaggregated commodity percentages
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


#' Disaggregate a single column from a table.
#' @param originalColVector A dataframe containing the column to disaggregate
#' @param disagg_specs Specifications for disaggregating the current Table
#' @param duplicate A flag that indicates whether the disaggregated columns are to be duplicated or not (e.g. for CPI values)
#' @param notUniform A flag that indicates whether the disaggregated columns are to be disaggregated in uniform manner or not
#' @return A dataframe with the original column disaggregated.
disaggregateCol <- function (originalColVector, disagg_specs, duplicate = FALSE, notUniform = FALSE){
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (duplicate){
      #For handling CPI. Just copy the CPI values of the original sector to for all the disaggregated sectors.
      disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
  }
  
  else if(notUniform){
    
    percentages <- getDisaggIndustryPercentages(disagg_specs)#get defaul disaggregated industry percentages
    percentageOrder <- percentages[match(disagg_specs$DisaggregatedSectorCodes, percentages$CommodityCode),]
    disaggCols <- originalColVector[, rep(seq_len(ncol(originalColVector)), numNewSectors)]#repeat the original vector numNewSector times
    disaggCols <- data.frame(t(t(disaggCols)*percentageOrder[,3]))
    
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
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return crosswalk with new sectors added.
disaggregateMasterCrosswalk <- function (model, disagg){

  
  crosswalk <- model$crosswalk#temp variable for storing intermediate changes
  new_cw <- crosswalk#variable to return with complete changes to crosswalk#temp
  
  #model$specs$BaseIOlevel # contains string describing model level
  
  #deterime which rows and columns to modify
  cwColIndex <- match(paste0("BEA_", model$specs$BaseIOLevel), colnames(crosswalk)) #search for concatenation of "BEA" and model$specs$BaseIOlevel object in crosswalk column names
  OriginalCodeLength <- regexpr(pattern ='/',disagg$OriginalSectorCode) - 1 #used to determine the length of the sector codes. E.g., detail would be 6, while summary would generally be 3 though variable, and sector would be variable
  DisaggCodeLength <- regexpr(pattern ='/',disagg$DisaggregatedSectorCodes[[1]]) - 1 #used to determine length of disaggregated sector codes.
  
  #Update original sector codes with disaggregated sector codes in the relevant column (i.e. cwColIndex) where rows have an exact match for the disaggregated codes in the NAICS column
  
  disaggNAICSIndex <- which(new_cw[,cwColIndex] == substr(disagg$OriginalSectorCode,1,OriginalCodeLength))#Get the indeces of the BEA_Detail column that are mapped to the originalSector
  disaggNAICSRows <- new_cw[disaggNAICSIndex,]#create a dataframe with only the relevant rows
  codeMatches <- match(disagg$NAICSSectorCW$NAICS_2012_Code,disaggNAICSRows$NAICS)#find matches between the crosswalk included in the disagg specs and NAICS column of the master crosswalk
  disaggNAICSRows[codeMatches,cwColIndex] <- substr(disagg$NAICSSectorCW$USEEIO_Code,1,DisaggCodeLength)#in the new crosswalk, replace codes in the new column (i.e. USEEIOv2.0) with the disaggregated codes for the rows where there is a match in between the NAICS crosswalk and the disaggregated Crosswalk
  new_cw[disaggNAICSIndex,] <- disaggNAICSRows#replace the rows that have a match for 6-digit naics for the disaggregated sectors in the crosswalk
  crosswalk <- new_cw #update crosswalk with the new sectors as a static index refernce for the rows still containing the originalSectorCode in the last column.
  
  #Update remaining rows where the original sector is present in cwColIndex but there is no exact match in the NAICS column for the disaggregated sector codes (e.g. 2-5 level NAICS codes)
  #get the indeces of the remaining rows that have 56200 in the new column
  remainingDisaggNAICSIndex <- which(new_cw[,cwColIndex] == substr(disagg$OriginalSectorCode,1,OriginalCodeLength))
  
  for (i in 1:length(remainingDisaggNAICSIndex)){
    #get the indeces of the remaining rows that have 56200 in the new column
    disaggNAICSIndex <- which(new_cw[,cwColIndex] == substr(disagg$OriginalSectorCode,1,OriginalCodeLength))
    crosswalkRow <- new_cw[disaggNAICSIndex[1],] #extract current row where code in last column needs to be updated
    
    rowComparisons <- grepl(crosswalkRow$NAICS[1], disagg$NAICSSectorCW$NAICS_2012_Code)#compare the value in the first column (NAICS) to the NAICS values in the disaggCrosswalk. Result is a string with TRUE where first column is a substring of values in disaggCrosswalk
    
    rowReplacements <- disagg$NAICSSectorCW$NAICS_2012_Code[rowComparisons]#Get the NAICS sector codes in the disagg crosswalk that are a match for the NAICS substring in the master crosswalk 
    rowReplacements <- substr(disagg$NAICSSectorCW$USEEIO_Code[rowComparisons],1,DisaggCodeLength)#Get the disaggregated sector codes that are mapped to the matches of the NAICS substring
    rowReplacements <- unique(rowReplacements)#reduce the list to the unique number of disaggregated sectors that the row comparisons map to
    
    crosswalkRow <- crosswalkRow[rep(seq_len(nrow(crosswalkRow)), length(rowReplacements)),, drop=FALSE]#replicate the crosswalk row as many times as there were matches in the substring search
    crosswalkRow[,cwColIndex] <- rowReplacements#replace the values in the last column (e.g. originalSectorCode) with the newSectorCodes that matched the substring search
    new_cw <- rbind(new_cw[1:disaggNAICSIndex[1]-1,],crosswalkRow, new_cw[-(1:disaggNAICSIndex[1]),])#include the expanded rows in the crosswalk
    
    
  }
  
  #renaming rows of crosswalk
  rownames(new_cw) <- 1:nrow(new_cw)
  
  return(new_cw)
  
}

#' Disaggregate make table based on the allocations specified in the files referenced in the diaggregation specs.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return A standardized make table with old sectors removed and new disaggregated sectors added based on the allocations in the disaggregation specs.
specifiedMakeDisagg <- function (model, disagg){
  
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
    
    ###Disaggregate Make Rows, Columns, and Intersection while using the allocation data extracted from the Disaggregationcsv. 
    
    #Allocations for column (commodity) disaggregation. 
    #Get rows of the DF which do not contain the original sector code or the new sector codes in the industry column (e.g., get only non 562 sector codes when doing waste disaggregation),
    #and where only the new sector codes are present in the commodity column.
    colPercentages <- subset(makeAllocations, !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes) & CommodityCode %in% newSectorCodes)
    #Assignning allocation for disaggregated columns
    AllocColDF <- disaggAllocations(model,disagg,colPercentages,"MakeCol") 
    
    #Allocations for make intersection. Get rows of DF where only new sector codes are present in both the industryCode and commodityCode columns. 
    intersectionPercentages <-subset(makeAllocations, IndustryCode %in% newSectorCodes & CommodityCode %in% newSectorCodes)
    #Assigning allocations for disaggregated intersection
    AllocIntersectionDF <- disaggAllocations(model,disagg,intersectionPercentages,"MakeIntersection")
    
    #Allocations for the row (industry) disaggregation. Get all rows of the DF where new sector codes are in the industryCode column, and neither the original nor new sector codes are in the commodityColumn. 
    rowsPercentages <- subset(makeAllocations, IndustryCode %in% newSectorCodes & !(CommodityCode %in% originalSectorCode) & !(CommodityCode %in% newSectorCodes))
    #Assigning allocations for disaggregated rows
    allocRowDF  <- disaggAllocations(model,disagg,rowsPercentages,"MakeRow")
 
    
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
    
    DisaggMake <- assembleTable(originalMake, originalRowIndex, originalColIndex, AllocColDF, allocRowDF, AllocIntersectionDF)
    
  }else{
    
    #No csv was read in, terminate execution
    DisaggMake <- NULL;
  }
  #End of if(!is.null(disagg$MakeFileDF)) loop
  
  return(DisaggMake)
  
}#End of specifiedMakeDisagg Function


#' Disaggregate use table based on the allocations specified in the files referenced in the diaggregation specs.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param domestic Flag that indicates where to use the Domestic Use or UseTrasanctions table
#' @return A standardized make table with old sectors removed and new disaggregated sectors added based on the allocations in the disaggregation specs.
specifiedUseDisagg <- function (model, disagg, domestic = FALSE){
  
  if(!is.null(disagg$UseFileDF)){
    
    #Local variable for original sector code
    originalSectorCode <- disagg$OriginalSectorCode
    #Local variable for new sector codes
    newSectorCodes <- disagg$DisaggregatedSectorCodes
    #Local variable for Make table allocations
    UseAllocations <- disagg$UseFileDF
    #Column names in Final Demand
    fdColNames <- colnames(model$FinalDemand)
    VARowNames <- rownames(model$ValueAdded)

    ###Disaggregate Make Rows, Columns, and Intersection while using the allocation data extracted from the Disaggregation.csv
    
    ##Extract data from Disaggregation csv
    #Commodity allocation totals (i.e. disaggregated row percentages) of new sectors (e.g. 100% of 562000 split into to 50% 562HAZ and 50% 562OTH; not actual splits).
    defaultPercentages <- getDisaggCommodityPercentages(disagg)
 
    #Allocations for intersection. Get rows of DF where only new sector codes are present in both the industryCode and commodityCode columns. 
    intersectionPercentages <-subset(UseAllocations, IndustryCode %in% newSectorCodes & CommodityCode %in% newSectorCodes)
    
    #Assigning allocations for disaggregated intersection
    AllocIntersectionDF <- disaggAllocations(model,disagg,intersectionPercentages,"UseIntersection", domestic)
    
    
    #Allocations for column (industry) disaggregation. 
    #Get rows of the DF which do not contain the original sector code or the new sector codes in the commodity column (e.g., get only non 562 sector codes when doing waste disaggregation),
    #where no VA row names are present in the commodity Column, and only the new sector codes are present in the industry column
    colPercentages <- subset(UseAllocations, !(CommodityCode %in% originalSectorCode) & !(CommodityCode %in% newSectorCodes) & !(CommodityCode %in% VARowNames) & IndustryCode %in% newSectorCodes)
    
    #Assignning allocation for disaggregated columns
    AllocColDF <- disaggAllocations(model,disagg,colPercentages,"UseCol", domestic) 
    
    
    #Allocations for the row (commodity) disaggregation. Get all rows of the DF where:
    #new sector codes are in the CommodityCode column; the FD column codes are not in the IndustryCode; 
    #and neither the original nor new sector codes are in the IndustryCode column. 
    #rowsPercentages <- subset(UseAllocations, CommodityCode %in% newSectorCodes & !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes))
    rowsPercentages <- subset(UseAllocations, CommodityCode %in% newSectorCodes & !(IndustryCode %in% fdColNames) & !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes))
    

    #Assigning allocations for disaggregated rows
    allocRowDF  <- disaggAllocations(model,disagg,rowsPercentages,"UseRow", domestic)
    
    #-----------------------Assembling table (code shared with uniformUseDisagg)
    
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
    
    DisaggUse <- assembleTable(originalUse, originalRowIndex, originalColIndex, AllocColDF, allocRowDF, AllocIntersectionDF)

  }else{
    
    #No csv was read in, terminate execution
    DisaggUse <- NULL
  }
  #End of if(!is.null(disagg$MakeFileDF)) loop
  
  return(DisaggUse)
  
}#End of specifiedUseDisagg Function


#' Assemble Table from the various disaggregated components.
#' @param originalTable Dataframe. The original table before disaggregation
#' @param originalRowIndex Integer. The row index, in the original table, of the sector to be disaggregated
#' @param originalColIndex Integer. The column index, in the original table, of the sector to be disaggregated
#' @param disaggCols Dataframe. Previously disaggregated columns of the table.
#' @param disaggRows Dataframe. Previously disaggregated rows of the table.
#' @param disaggIntersection Dataframe. Previously disaggregated intersection of the table.
#' @return The Disaggregated table as a dataframe with the disaggregated rows, columns, and intersection included
assembleTable <- function (originalTable, originalRowIndex, originalColIndex, disaggCols, disaggRows, disaggIntersection){
  
  
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
#' @param allocPercentages Dataframe. A subset of the dataframe that contains the percentages to allocate to specific industry and commodity combinations in the disaggregated vector. Parameter use coordinated with @param vectorToDisagg
#' @param vectorToDisagg String. A parameter to indicate what table and what part of that table is being disaggregated (e.g. "MakeCol" or "Intersection") 
#' @param domestic Boolean. Flag to indicate where to use the DomesticUse or UseTransactions table
#' @return A dataframe with the values specified in the disaggSpecs assigned to the correct Make or Use table indeces.
disaggAllocations <- function (model, disagg, allocPercentages, vectorToDisagg, domestic = FALSE){
  
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
    #Make the default Percentages match the disaggregated sector order
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$CommodityCode),]
    
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
    
  }
  else if(vectorToDisagg == "MakeCol")
  {
    
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
    #Make the default Percentages match the disaggregated sector order
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$CommodityCode),]
    
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
    
  }
  else if(vectorToDisagg == "MakeIntersection")
  {
    
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
    #Make the default Percentages match the disaggregated sector order
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$CommodityCode),]
    
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
    
  }
  else if(vectorToDisagg == "UseRow" || vectorToDisagg == "FinalDemand" )
  {
    
    #get original table
    if(domestic == TRUE){
      
      if(vectorToDisagg == "UseRow")
      {
        originalTable <- model$DomesticUseTransactions
      }
      else
      {
        originalTable <- model$DomesticFinalDemand
      }
    
    }else{
      
      if(vectorToDisagg == "UseRow")
      {
        originalTable <- model$UseTransactions
      }
      else
      {
        originalTable <- model$FinalDemand
      }
      
    
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
    defaultPercentages <- subset(disagg$UseFileDF, IndustryCode %in% originalSectorCode)
    
    #Make the default Percentages match the disaggregated sector order
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$CommodityCode),]
    
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
    
  }
  else if (vectorToDisagg == "UseCol" || vectorToDisagg == "ValueAdded")
  {
    
    #get original table
    
    if(vectorToDisagg == "UseCol")
    {
      if(domestic == TRUE)
      {
        
        originalTable <- model$DomesticUseTransactions
        
      }
      else
      {
        
        originalTable <- model$UseTransactions
        
      }
      
    }
    else
    {
      originalTable <- model$ValueAdded
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
    #Make the default Percentages match the disaggregated sector order
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$IndustryCode),]
    
    
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
    
  }
  else if(vectorToDisagg == "UseIntersection")
  {
    
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
    #Make the default Percentages match the disaggregated sector order
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$IndustryCode),]
    
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
    
  }
  else
  {
    #todo
    #error handling
  }
  
  
  if(nrow(allocPercentages)>0)#Check that there are manual allocations to perform
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
        logging::logdebug(paste("rowAlloc not found, no allocation made for row", rowAlloc, sep=" ", "in table."))
        
      }
      
      if(length(colAllocIndex)==0L){
        logging::logdebug(paste("colAlloc not found, no allocation made for column", colAlloc, sep=" ", "in table."))
        
      }
      
      
      #Calculate value based on allocation percent
      if(vectorToDisagg == "MakeRow" || vectorToDisagg == "UseRow" || vectorToDisagg == "FinalDemand"){
        
        value <- originalVector[colAllocIndex]*allocationValue
        
      }else if(vectorToDisagg=="MakeCol" || vectorToDisagg=="UseCol" || vectorToDisagg == "ValueAdded"){
        
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
    logging::logdebug(paste("rowAlloc not found, no allocation made for", vectorToDisagg, sep=" "))
    
  }

  
  #replace all NAs with 0
  manualAllocVector[is.na(manualAllocVector)] <-0
  
  #Replace values in the default allocation vector with values from the Manual allocation vector to finalize the vector disaggregation.

  if(vectorToDisagg == "MakeRow"|| vectorToDisagg == "MakeIntersection" || vectorToDisagg=="UseRow" || vectorToDisagg =="UseIntersection" || vectorToDisagg == "FinalDemand")
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

  }else if(vectorToDisagg == "MakeCol" || vectorToDisagg == "UseCol" || vectorToDisagg == "ValueAdded"){
    
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
  
}#end of disaggAllocations function


#' Obtain default disaggregation percentages for industries from the disaggregation input files. 
#' @param disagg Specifications for disaggregating the current Model
#' @return A dataframe with the default disaggregation percentges for the Industries of the current model
getDisaggIndustryPercentages <-function(disagg){
  
  defaultPercentages <- subset(disagg$MakeFileDF, IndustryCode %in% disagg$OriginalSectorCode)#get all rows in MakefileDF that have the OriginalSectorCode in the IndustryCode column
  
  return(defaultPercentages)
}


#' Obtain default disaggregation percentages for commodities from the disaggregation input files. 
#' @param disagg Specifications for disaggregating the current Model
#' @return A dataframe with the default disaggregation percentges for the Commodities of the current model
getDisaggCommodityPercentages <- function(disagg){
  
  defaultPercentages <- subset(disagg$UseFileDF, CommodityCode %in% disagg$OriginalSectorCode) #get all rows in UseAllocations that have the OriginalSectorCode in the CommodityCode column
 
   return(defaultPercentages)
  
}

#' Balance the Make and Use tables after disaggregation.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return model object with RAS-balanced disaggregation sectors 
balanceDisagg <- function(model, disagg){
  #Build full use table
  disaggFullUse <-buildDisaggFullUse(model, disagg)
  
  #Get commodity and/or industry indeces corresponding to the original sector code
  disaggIndustryIndex <- which(rownames(model$MakeTransactions)==disagg$DisaggregatedSectorCode[1])
  disaggCommodityIndex <- which(colnames(model$MakeTransactions)==disagg$DisaggregatedSectorCode[1])
  disaggIndustryEndIndex <- disaggIndustryIndex + length(disagg$DisaggregatedSectorCodes)-1
  disaggCommodityEndIndex <- disaggCommodityIndex + length(disagg$DisaggregatedSectorCodes)-1
  
  #Get Disaggregated Industry totals from both Make and Use tables
  disaggMakeIndTotals <- data.frame(rowSums(model$MakeTransactions[disaggIndustryIndex:disaggIndustryEndIndex,]))
  disaggUseIndTotals <- data.frame(colSums(disaggFullUse[, disaggIndustryIndex:disaggIndustryEndIndex]))

  #Get Disaggregated Commodity totals from both Make and Use Tables
  disaggMakeComTotals <- data.frame(colSums(model$MakeTransactions[, disaggCommodityIndex:disaggCommodityEndIndex]))
  disaggUseComTotals <- data.frame(rowSums(disaggFullUse[disaggCommodityIndex:disaggCommodityEndIndex, ]))
  
  #Calculate final disaggregation allocation percentages for commodities and industries in each table
  makeIndAllocPercentages <- disaggMakeIndTotals/sum(disaggMakeIndTotals)
  useIndAllocPercentages <- disaggUseIndTotals/sum(disaggUseIndTotals)
  
  makeComAllocPercentages <- disaggMakeComTotals / sum(disaggMakeComTotals)
  useComAllocPercentages <- disaggUseComTotals / sum(disaggUseComTotals)
  
  
  #Check Balance of industry totals across tables
  ones <- data.frame(matrix(1, nrow = length(disagg$DisaggregatedSectorCodes)))#initialized to number of disaggregated sectors by 1 col; all elements == 1
  tolerance <- ones*0.05#set all elements to 0.05 (ie 5% for all sectors)
  
  if(any(abs(useIndAllocPercentages - makeIndAllocPercentages) > tolerance) || any(abs(useComAllocPercentages - makeComAllocPercentages > tolerance))){
    
    #Balance. Create FullUse from UseTransanctions, UseValueAdded, and Final Demand, then call ApplyRAS
    targetIndTotals <- data.frame(rowSums(model$MakeTransactions))
    FDIndTotals <- data.frame(colSums(model$FinalDemand))
    colnames(FDIndTotals) <- colnames(targetIndTotals) #needed for rbind step
    targetIndTotals <- rbind(targetIndTotals, FDIndTotals)
    
    targetComTotals <- data.frame(colSums(model$MakeTransactions))
    VAComTotals <- data.frame(rowSums(model$ValueAdded))
    colnames(VAComTotals) <- colnames(targetComTotals) #needed for rbind step
    targetComTotals <- rbind(targetComTotals, VAComTotals)
    
    balancedDisaggFulluse <- applyRAS(data.matrix(disaggFullUse), targetComTotals[,1], targetIndTotals[,1], relative_diff = NULL, absolute_diff = 1E8, max_itr = 1E5)
    
    
  }
  else
  {
    #no change in fulluse
    balancedDisaggFullUse <- disaggFullUse
  }
  
  #break balancedDisaggFullUse back into model components
  
  domesticTables <- calculateBalancedDomesticTables(model, disagg, balancedDisaggFullUse)
  
  model$DomesticUseTransactions <- domesticTables$DomesticUseTransactions
  model$DomesticFinalDemand <- domesticTables$DomesticFinalDemand
  

  model$UseTransactions <- balancedDisaggFullUse[1:nrow(model$UseTransactions), 1:ncol(model$UseTransactions)]
  model$FinalDemand <- balancedDisaggFullUse[1:nrow(model$UseTransactions),-(1:ncol(model$UseTransactions))]
  model$ValueAdded <- balancedDisaggFullUse[-(1:nrow(model$UseTransactions)),1:ncol(model$UseTransactions)]
  

 
  return(model)
  
}

#' Build a Full Use table using the Use transactions, Use value added, and final demand model objects
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return dataframe representing a use table that includes the Use transactions, Use value added, and final demand sectors 
buildDisaggFullUse <- function(model, disagg){
  
  disaggFullUse <- rbind(model$UseTransactions, model$ValueAdded)
  
  tempVA <- matrix(0, nrow(model$ValueAdded), ncol(model$FinalDemand))
  colnames(tempVA) <- colnames(model$FinalDemand)
  rownames(tempVA) <- rownames(model$ValueAdded)
  fullFD <- rbind(model$FinalDemand, tempVA)
  
  disaggFullUse <- cbind(disaggFullUse, fullFD)
  
  return(disaggFullUse)
  
}


#' Calculate the domestic use transactions and final demand tables after RAS balancing
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param balancedFullUse A fullUse table (including UseTransactions, UseValueAdded, and FinalDemand), created to determine whether RAS balancing is needed
#' @return list containing balanced domesticFinalDemand and domesticUseTransactions dataframes. 
calculateBalancedDomesticTables <- function(model, disagg, balancedFullUse) 
{
  #Calculate domestic use transactions and domestic final demand based on balancedfullUse
  #Idea is to obtain the DomesticUse/UseTransaction ratio before balancing, and apply that to the balanced Use Transactions. 
  #Same for Domestic Final Demand
  domesticUseRatios <-  model$DomesticUseTransactions /model$UseTransactions 
  domesticUseRatios[is.na(domesticUseRatios)] <- 0# means numerator, denominator, or both are 0
  domesticUseRatios[domesticUseRatios == Inf] <-0 #inf means denominator is 0
  
  domesticFDRatios <- model$DomesticFinalDemand / model$FinalDemand
  domesticFDRatios[is.na(domesticFDRatios)] <-0
  domesticFDRatios[domesticFDRatios == Inf] <- 0

  balancedDomesticUseTransactions <- balancedFullUse[1:nrow(model$UseTransactions), 1:ncol(model$UseTransactions)]
  balancedDomesticUseTransactions <- balancedDomesticUseTransactions * domesticUseRatios
  
  balancedDomesticFD <- balancedFullUse[1:nrow(model$UseTransactions),-(1:ncol(model$UseTransactions))]
  balancedDomesticFD <- balancedDomesticFD * domesticFDRatios
  
  newDomesticTables <- list("DomesticUseTransactions" = balancedDomesticUseTransactions, "DomesticFinalDemand" = balancedDomesticFD)
  
  return(newDomesticTables)
}

