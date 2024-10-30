#TODO: add functionality to adjust disaggregation from Summary to detail based on summary model IO year. 
#by "bringing the detail model to summary year" for the specific disagg sector.

## NEXT STEPS: 
## Combined disaggregations of 22, GFE, and GSLE.
## Aggregate the disaggregated 221100, s00101, and s00202 sectors in an otherwise summary level model.
## Include disaggregated 221100 sectors in the combined disaggregation of 22, GFE, and GSLE.

#' Disaggregate a specific sector in a summary level model to detail level
#' @param modelname String indicating which model to generate. Must be a detail level model.
#' @param detailModel Completed build of detail model. If NULL, must pass modelname.
#' @param sectorToDisaggregate String with the summary level code of the sector to be disaggregated from Summary to Detail Level,
#' @param specificDetailLevelSector String to denote whether to disaggregate only the specific summary level sector to all related detail level sectors, or only one related detail level sector (if value is TRUE).
#' @param disagg Specifications for disaggregating the current Table. Pass to append outputs to the disagg object.
#' @param writePath String that specifies a path to write allocation csv files to.
#' @return A list object containing dataframes with the economic allocations for the Use and Make tables; environmental allocations for the TbS object; and the Sector CSV file output required for disaggregation.  
disaggregateSummaryModel <- function (modelname = "USEEIOv2.0", detailModel = NULL,
                                      sectorToDisaggregate = NULL, specificDetailLevelSector = NULL,
                                      disagg = NULL, writePath = NULL){
  # Check for appropriate input in sectorToDisaggregate and make sure format matches BEA_Summary column in model$crosswalk.
  if(is.null(sectorToDisaggregate)){
    stop("No summary level sector provided for disaggregation to detail level")
  }else{
    # Get index of '/' within the string if it exists to indicate where location code begins
    locIndex <- grep('/', strsplit(sectorToDisaggregate, '')[[1]])
    
    # Check for location code, or if there is none assume 'US'. 
    if(length(locIndex)!=0){
      summaryCode <- substr(sectorToDisaggregate, 1, locIndex-1)
      summaryLoc_Code <- substr(sectorToDisaggregate, locIndex + 1, nchar(sectorToDisaggregate))

    }else{
      summaryCode <- sectorToDisaggregate
      summaryLoc_Code <- 'US'
    }
  }
 
  if(is.null(detailModel)){
    # Read in a detail level model
    # todo: check if this line needs to  be replaced by a "load summary model from repo" line if this script is to be used outside the package, e.g. USEEIO teams. 
    detailModel <- buildModel(modelname)
  }

  # Get the detail sector codes that correspond to the summary code to be disaggregated
  summaryCodeCw <- subset(detailModel$crosswalk, BEA_Summary %in% summaryCode)
  summaryCodeCw <- as.list(unique(summaryCodeCw$BEA_Detail))
  
  # Consolidate disaggregation parameters in a list. 
  disaggParams <- list()
  disaggParams$specificDetailLevelSector <- specificDetailLevelSector
  disaggParams$detailModel <- detailModel
  disaggParams$summaryCode <- summaryCode
  disaggParams$summaryCodeCw <- summaryCodeCw
  disaggParams$summaryLoc_Code <-summaryLoc_Code
  # This is required to add a value to the NAICS code column of the model crosswalk object. 
  # TODO: ASK WHETHER THIS IS THE BEST WAY OF HANDLING THIS
  disaggParams$sectorsWithoutNAICS <- list("S00101","S00202") 
  
  # Get economic allocations
  fullUseTableColAlloc <- generateEconomicAllocations(disaggParams, "Use", "Column")
  makeTableColAlloc <- generateEconomicAllocations(disaggParams, "Make", "Column")
  fullUseTableRowAlloc <- generateEconomicAllocations(disaggParams, "Use", "Row")
  makeTableRowAlloc <- generateEconomicAllocations(disaggParams, "Make", "Row")
  fullUseIntersection <- generateEconomicAllocations(disaggParams, "Use", "Intersection")
  makeIntersection <-  generateEconomicAllocations(disaggParams, "Make", "Intersection")
  
  # Get environmental allocations
  envAllocationsDF <- generateEnvironmentalAllocations(disaggParams)

  # Create output DFs
  useAllocationsDF <- rbind(fullUseIntersection, fullUseTableColAlloc, fullUseTableRowAlloc)
  makeAllocationsDF <- rbind(makeIntersection, makeTableColAlloc, makeTableRowAlloc)
  #sectorsDF <- createSectorsCSV(detailModel, summaryCode, summaryCodeCw)
  sectorsDF <- createSectorsCSV(disaggParams)
  
  if(!is.null(disagg)){
    disagg$UseFileDF <- useAllocationsDF
    disagg$MakeFileDF <- makeAllocationsDF
    disagg$EnvFileDF <- envAllocationsDF
    disagg$NAICSSectorCW <- sectorsDF
    return(disagg)
  }
  else {
  outputDF <- list()
  outputDF$UseFileDF <- useAllocationsDF
  outputDF$MakeFileDF <- makeAllocationsDF
  outputDF$EnvFileDF <- envAllocationsDF
  outputDF$NAICSSectorCW <- sectorsDF
  outputDF$originalSector <- sectorToDisaggregate # Needed for the case where we want to combine multiple allocations later.
  
  #Write DFs to correct folder
  writeAllocationsToCSV(outputDF, disaggParams, writePath)
  return(outputDF)
  }
  
}

#' Create a mapping object that contains summary-to-detail-to-allocation specific (e.g. 22X) mappings
#' @param detailModel Completed build of detail model. If NULL, must pass modelname.
#' @param currentList List that contains the disaggregation percentages for Summary-to-Detail level disaggregation of a specific summary sector
#' @return A dataframe with mappings of summary, detail, and allocation specific sectors as defined by the currentList input 
combineAllocAndDetailCW <- function(detailModel = NULL, listOfAllocations){
  
  
  listOfCrosswalks <- vector("list", length(listOfAllocations))
  # For each list, get crosswalks of original summary sectors to disaggregated summary sectors and detail sectors 
  for(listNumber in 1:length(listOfAllocations)){
    temp <- 2
    
    currentList <- listOfAllocations[[listNumber]]
#    otherListsOriginalSectors <- sapply(listOfAllocations[-(listNumber)], function(i)i[["originalSector"]])
    
    locIndex <- grep('/', strsplit(currentList$originalSector, '')[[1]])
    # Separate sector and location code
    summaryCode <- substr(currentList$originalSector, 1, locIndex-1)
    summaryLoc_Code <- substr(currentList$originalSector, locIndex + 1, nchar(currentList$originalSector))
    
    # Create a crosswalk that contains detail and summary BEA as well as the modified crosswalk of the allocated list.
    cw <- currentList$NAICSSectorCW[,1:2]
    cw <- cbind(cw, cw[,2])
    cw[,3] <- gsub("/[A-z]*",'',cw[,3]) # Create a copy the USEEIO codes but with locations removed (i.e., gsub removes everything after '/')
    cwColnames <- names(cw)
    cwColnames[2:3] <- c("USEEIO_Code_Loc", "USEEIO_Code")
    colnames(cw) <- cwColnames
    naicsIndeces <- which(detailModel$crosswalk$NAICS %in% cw$NAICS_2012_Code)    
    
    cwNaicsInMainCW <- detailModel$crosswalk[naicsIndeces,]
    sharedNaics <- which(cwNaicsInMainCW$BEA_Summary %in% summaryCode)
    allocToDetCW <- cbind(cw, cwNaicsInMainCW[sharedNaics,3:4])
    
    allocToDetCWUnique <- allocToDetCW[!duplicated(allocToDetCW[,c("BEA_Detail")]),] # Get unique mappings of modified summary model codes to BEA detail 
    allocToDetCWUnique <- cbind(allocToDetCWUnique, paste0(allocToDetCWUnique[,5],"/",summaryLoc_Code)) # Adding a version of the detail code with location for use later when modifying the allocation percentages
    names(allocToDetCWUnique)[6] <- "BEA_Detail_Loc"
    
    listOfCrosswalks[[listNumber]] <- allocToDetCWUnique 
    
    temp <- 2
  }
  

  
  return(listOfCrosswalks)
}


#' Create combined allocation percentages by Table based on a list of several allocations
#' @param detailModel A complete USEEIO model at the detail level.  
#' @param table A String indiciate which table to use from the model, can be Use or Make. 
#' @param listOfCrosswalks A list containing dataframes with mappings of summary, detail, and allocation specific sectors
#' @param tableParams List of paramater used to created the combined percentages from the indicated table  
#' @return Combined allocation percentages for the specific table
createCombinedPercentagesByTable <- function(detailModel, table, listOfCrosswalks, tableParams){
  # Note: due to the way the Make and Use tables are structured (i.e., their commodity and industry orientation is transposed),
  # the sequence of operations in this function are carried out row wise for the Use allocations and columwise for the Make allocations
  
  # Unpack tableParams
  currentComDetailIndeces <- tableParams$currentComDetailIndeces
  currentIndDetailIndeces <- tableParams$currentIndDetailIndeces
  commodityList <- tableParams$commodityList 
  industryList <- tableParams$industryList 
  otherListComDetailIndeces <- tableParams$otherListComDetailIndeces 
  otherListIndDetailIndeces <- tableParams$otherListIndDetailIndeces 
  
  if(table == "Use"){
    # Get Use table intersection of currentIndeces and otherList indeces
    currentTable <- detailModel$U[currentComDetailIndeces,otherListIndDetailIndeces, drop = FALSE]
    # Need to find the rows and columns in currentTable that match the X sectors (i.e. allocated sectors) in crosswalks
    
    # Combine rows that match row X sector
    # Row X sectors are referenced by commodityList; column X sectors referenced by industryList
    RowsDetailMatches <- rownames(currentTable)[which(rownames(currentTable) %in% listOfCrosswalks[[commodityList]]$BEA_Detail_Loc)] # Sector codes of currentTable rows which match the BEA detail of the crosswalk
    if(length(RowsDetailMatches) == 0){
      stop("Error in combining allocation percentages: mismatch in detail to allocated sectors mapping")
    }
    
    XSectorsInRows <- which(!(RowsDetailMatches %in% listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc))
    XSectorsCombinedRowName <- listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc[XSectorsInRows[1]]
    
    #Combine Columns that match col X sector 
    ColsDetailMatches <- colnames(currentTable)[which(colnames(currentTable) %in% listOfCrosswalks[[industryList]]$BEA_Detail_Loc)]
    if(length(ColsDetailMatches) == 0){
      stop("Error in combining allocation percentages: mismatch in detail to allocated sectors mapping")
    }
    
    XSectorsInCols <- which(!(ColsDetailMatches %in% listOfCrosswalks[[industryList]]$USEEIO_Code_Loc))
    XSectorsCombinedColName <- listOfCrosswalks[[industryList]]$USEEIO_Code_Loc[XSectorsInCols[1]]
    
    # Get sectors that are not aggregated to summary level (e.g. 221100 in 22X aggregation)
    RowSectorsNotInXSectors <- which(RowsDetailMatches %in% listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc) # Unaggregated row sector index
    ColSectorsNotInXSectors <- which(ColsDetailMatches %in% listOfCrosswalks[[industryList]]$USEEIO_Code_Loc) # Unaggregated col sector index
    
  }else if(table == "Make"){
    currentTable <- detailModel$V[otherListIndDetailIndeces, currentComDetailIndeces, drop = FALSE]
    RowsDetailMatches <- rownames(currentTable)[which(rownames(currentTable) %in% listOfCrosswalks[[industryList]]$BEA_Detail_Loc)] # Sector codes of currentTable rows which match the BEA detail of the crosswalk
    if(length(RowsDetailMatches) == 0){
      stop("Error in combining allocation percentages: mismatch in detail to allocated sectors mapping")
    }
    XSectorsInRows <- which(!(RowsDetailMatches %in% listOfCrosswalks[[industryList]]$USEEIO_Code_Loc))
    XSectorsCombinedRowName <- listOfCrosswalks[[industryList]]$USEEIO_Code_Loc[XSectorsInRows[1]]
    
    #Combine Columns that match col X sector 
    ColsDetailMatches <- colnames(currentTable)[which(colnames(currentTable) %in% listOfCrosswalks[[commodityList]]$BEA_Detail_Loc)]
    if(length(ColsDetailMatches) == 0){
      stop("Error in combining allocation percentages: mismatch in detail to allocated sectors mapping")
    }
    
    XSectorsInCols <- which(!(ColsDetailMatches %in% listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc))
    XSectorsCombinedColName <- listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc[XSectorsInCols[1]]
    
    # Get sectors that are not aggregated to summary level (e.g. 221100 in 22X aggregation)
    RowSectorsNotInXSectors <- which(RowsDetailMatches %in% listOfCrosswalks[[industryList]]$USEEIO_Code_Loc) # Unaggregated row sector index
    ColSectorsNotInXSectors <- which(ColsDetailMatches %in% listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc) # Unaggregated col sector index
  }
  
  if(dim(currentTable)[1] == 1){ 
    # For the case the are different number of commodities and industries, due to a sector being industry-only in Use or commodity only in Make
    # No need to sum along column dimension as there is only one row 
    XSectorsCombinedRow <- currentTable 
    
    XSectorsCombinedCol <- as.matrix(sum(currentTable[XSectorsInCols]))
    rownames(XSectorsCombinedCol) <- rownames(currentTable)
  }else if(dim(currentTable)[2] == 1){ 
    # For the case the are different number of commodities and industries, due to a sector being commodity-only in Use or industry only in Make
    XSectorsCombinedRow <- as.matrix(sum(currentTable[XSectorsInRows]))
    colnames(XSectorsCombinedRow) <- colnames(currentTable)
    
    # No need to sum along row dimension as there is only one column 
    XSectorsCombinedCol <- currentTable 
  }
  else{ 
    # For the case where there are an equal number of commodities and industries
    XSectorsCombinedRow <- t(colSums(currentTable[XSectorsInRows,]))
    XSectorsCombinedCol <- as.matrix(rowSums(currentTable[,XSectorsInCols])) # To match the format of XSectorsCombinedRow
  }
  
  rownames(XSectorsCombinedRow) <- XSectorsCombinedRowName
  colnames(XSectorsCombinedCol) <- XSectorsCombinedColName
  

  # Create temporary matrix to store the correct mix of aggregate and detail level values  
  if(length(RowSectorsNotInXSectors) != 0 && length(ColSectorsNotInXSectors) != 0){ # The case when, after aggregation, there are still 2 rows and columns left
    aggTable <- data.frame(matrix(0, nrow =2, ncol = 2)) 
    
    aggTable[1,1] <- currentTable[RowSectorsNotInXSectors,ColSectorsNotInXSectors] # Store unaggregated row and aggregarted column value at 1,1
    aggTable[2,1] <- XSectorsCombinedRow[ColSectorsNotInXSectors] # Store aggregated row and unaggregated column value at 2,1
    aggTable[1,2] <- XSectorsCombinedCol[RowSectorsNotInXSectors] # Store aggregated col and unaggregated row at value 1,2
    aggTable[2,2] <- sum(XSectorsCombinedRow) - XSectorsCombinedRow[ColSectorsNotInXSectors] # Store aggregated row and aggregated col values at 2,2
    
    aggRowNames <- c(rownames(currentTable)[RowSectorsNotInXSectors],XSectorsCombinedRowName)
    rownames(aggTable) <- aggRowNames
    
    aggColNames <- c(colnames(currentTable)[ColSectorsNotInXSectors],XSectorsCombinedColName)
    colnames(aggTable) <- aggColNames
    
    aggPercent <- aggTable/sum(sum(aggTable))
  }else if(length(RowSectorsNotInXSectors) == 0 && length(ColSectorsNotInXSectors) != 0){ 
    # The case when, after aggregation, there is only one row left because the detail level sector did not exist as a commodity, only an industry
    aggTable <- data.frame(matrix(0, nrow = 1, ncol = 2))
    
    aggTable[1,1] <- XSectorsCombinedRow[ColSectorsNotInXSectors] # Store aggregated row and unaggregated column value at 1,1
    aggTable[1,2] <- sum(XSectorsCombinedRow) - XSectorsCombinedRow[ColSectorsNotInXSectors] # Store aggregated row and aggregated col values at 2,2
    
    rownames(aggTable) <- XSectorsCombinedRowName
    aggColNames <- c(colnames(currentTable)[ColSectorsNotInXSectors],XSectorsCombinedColName)
    colnames(aggTable) <- aggColNames
    
    aggPercent <- aggTable/sum(aggTable)
  }
  else if(length(RowSectorsNotInXSectors) != 0 && length(ColSectorsNotInXSectors) == 0){

    # The case when, after aggregation, there is only one col left because the detail level sector did not exist as an industry, only a commodity
    aggTable <- data.frame(matrix(0, nrow = 2, ncol = 1))
    
    aggTable[1,1] <- XSectorsCombinedCol[RowSectorsNotInXSectors] # Store unaggregated row and aggregated column value at 1,1
    aggTable[2,1] <- sum(XSectorsCombinedCol) - XSectorsCombinedCol[RowSectorsNotInXSectors] # Store aggregated row and aggregated col values at 2,2
    
    aggRowNames <- c(rownames(currentTable)[RowSectorsNotInXSectors], XSectorsCombinedRowName)
    rownames(aggTable) <- aggRowNames
    colnames(aggTable) <- XSectorsCombinedColName
    
    aggPercent <- aggTable/sum(aggTable)
  }
  
  aggPercent[is.na(aggPercent)] <-0 # Replace all NAs with 0s
  
  
  
  
  
  temp <- 2
  return(aggPercent) 
  
}


#' Remove original sector summary codes (e.g., 22/US) from the listOfAllocations after including combined allocation percentages in this list.
#' @param combinedLists A list containing the combined disaggregation allocations for several summary to detail disaggregations 
#' @return Combined allocation percentages for the specific table without original summary sector codes
removeOriginalCodesFromAllocTable <- function(combinedLists, listOfCrosswalks, tableParams){
  
  # For UseFileDF 
  # Remove original sector codes from commodityCode column in combinedLists
  useFileDFComAllocIndex <- which(combinedLists$UseFileDF$CommodityCode %in% combinedLists$originalSector)
  if(length(useFileDFComAllocIndex) != 0){
    combinedLists$UseFileDF <- combinedLists$UseFileDF[-(useFileDFComAllocIndex),]
    
  }
  
  # Remove original Sector codes for IndustryCode column in combinedLists
  useFileDFIndAllocIndex <- which(combinedLists$UseFileDF$IndustryCode %in% combinedLists$originalSector)
  if(length(useFileDFIndAllocIndex) != 0){
    combinedLists$UseFileDF <- combinedLists$UseFileDF[-(useFileDFComAllocIndex),]
    
  }
  
  # For MakeFileDF
  # Remove original sector codes from commodityCode column in combinedLists
  makeFileDFComAllocIndex <- which(combinedLists$MakeFileDF$CommodityCode %in% combinedLists$originalSector)
  if(length(makeFileDFComAllocIndex) != 0){
    combinedLists$MakeFileDF <- combinedLists$MakeFileDF[-(makeFileDFComAllocIndex),]
    
  }
  
  # Remove original Sector codes for IndustryCode column in combinedLists
  makeFileDFIndAllocIndex <- which(combinedLists$MakeFileDF$IndustryCode %in% combinedLists$originalSector)
  if(length(makeFileDFIndAllocIndex) != 0){
    combinedLists$MakeFileDF <- combinedLists$MakeFileDF[-(makeFileDFIndAllocIndex),]
  }
  
  return(combinedLists)
  
}


#' Assign combined allocation percentages by Table based on a list of several allocations
#' @param listOfAllocations A list of lists containing the disaggregation allocations for several summary to detail disaggregations 
#' @param listOfCrosswalks A list containing dataframes with mappings of summary, detail, and allocation specific sectors
#' @param tableParams List of paramater used to created the combined percentages from the specific table  
#' @return Combined allocation percentages for the specific table
assignCombinedPercentagesByTable <- function(listOfAllocations, listOfCrosswalks, tableParams){

  # Unpack tableParams
#  currentComDetailIndeces <- tableParams$currentComDetailIndeces
#  currentIndDetailIndeces <- tableParams$currentIndDetailIndeces
  commodityList <- tableParams$commodityList 
  industryList <- tableParams$industryList 
#  otherListComDetailIndeces <- tableParams$otherListComDetailIndeces 
#  otherListIndDetailIndeces <- tableParams$otherListIndDetailIndeces 
  listOfUseTables <- tableParams$listOfUseTables 
  listOfMakeTables <- tableParams$listOfMakeTables 
    
  # For UseFileDF
  # Finding useFileDF indeces that contain values to replace
  useFileDFComAllocIndex <- which(listOfAllocations[[commodityList]]$UseFileDF$CommodityCode %in% unique(listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc))
  useFileDFIndAllocIndex <- which(listOfAllocations[[commodityList]]$UseFileDF$IndustryCode %in% listOfAllocations[[industryList]]$originalSector)
  allocUseIndeces <- intersect(useFileDFComAllocIndex, useFileDFIndAllocIndex)
  
  # Formatting percentages to include in UseFileDF
  allocDFUse <- reshape2::melt(t(t(listOfUseTables[[length(listOfUseTables)]])), id.vars=1) # Need double transpose to get the right melt shape and row ordering
  allocDFUse <- allocDFUse[,c(2,1,3)] #reorder columns
  
  colnames(allocDFUse) <- c("IndustryCode", "CommodityCode", "PercentUse")
  noteDF <-  data.frame(Note = I(rep("CommodityDisagg", nrow(allocDFUse))))
  allocDFUse <- cbind(allocDFUse, noteDF)
  
  if(length(allocUseIndeces) != 0){ # error checking required in case allocUseIndeces does not have any values 
    
    listOfAllocations[[commodityList]]$UseFileDF <- rbind(listOfAllocations[[commodityList]]$UseFileDF[1:(allocUseIndeces[1]-1),],
                                                          allocDFUse,
                                                          listOfAllocations[[commodityList]]$UseFileDF[-(1:(allocUseIndeces[length(allocUseIndeces)])),]) 
  }
  
  rownames(listOfAllocations[[commodityList]]$UseFileDF) <- 1:nrow(listOfAllocations[[commodityList]]$UseFileDF)

  
  # For MakeFileDF
  # Finding useFileDF indeces that contain values to replace
  makeFileDFComAllocIndex <- which(listOfAllocations[[commodityList]]$MakeFileDF$CommodityCode %in% unique(listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc))
  makeFileDFIndAllocIndex <- which(listOfAllocations[[commodityList]]$MakeFileDF$IndustryCode %in% listOfAllocations[[industryList]]$originalSector)
  allocMakeIndeces <- intersect(makeFileDFComAllocIndex, makeFileDFIndAllocIndex )
  
  #TODO: Insert allocDF Use and Make in their repestive places, e.g., in allocUseIndeces' place in listOfAllocations[[commodityList]]$UseFileDF
  allocDFMake <- reshape2::melt(t(t(listOfMakeTables[[length(listOfMakeTables)]])), id.vars=1)
  colnames(allocDFMake) <- c("IndustryCode", "CommodityCode", "PercentMake")
  noteDF <-  data.frame(Note = I(rep("IndustryDisagg", nrow(allocDFMake))))
  allocDFMake <- cbind(allocDFMake, noteDF)
  
  if(length(allocMakeIndeces) != 0){
    listOfAllocations[[commodityList]]$MakeFileDF <- rbind(listOfAllocations[[commodityList]]$MakeFileDF[1:(allocMakeIndeces[1]-1),],
                                                           allocDFMake,
                                                           listOfAllocations[[commodityList]]$MakeFileDF[-(1:(allocMakeIndeces[length(allocMakeIndeces)])),])
  }
   
  rownames(listOfAllocations[[commodityList]]$MakeFileDF) <- 1:nrow(listOfAllocations[[commodityList]]$MakeFileDF)
  
#   # Finding locations of current industries to replace in the original list of allocations
#   if(commodityList > industryList){
#     # Use
#     useFileDFIndAllocIndex <- which(listOfAllocations[[industryList]]$UseFileDF$IndustryCode %in% unique(listOfCrosswalks[[industryList]]$USEEIO_Code_Loc))
#     useFileDFComAllocIndex <- which(listOfAllocations[[industryList]]$UseFileDF$CommodityCode %in% listOfAllocations[[commodityList]]$originalSector)
#     allocUseIndeces <- intersect(useFileDFComAllocIndex, useFileDFIndAllocIndex)
#     
#     #Make
#     makeFileDFIndAllocIndex <- which(listOfAllocations[[industryList]]$MakeFileDF$IndustryCode %in% unique(listOfCrosswalks[[industryList]]$USEEIO_Code_Loc))
#     makeFileDFComAllocIndex <- which(listOfAllocations[[industryList]]$MakeFileDF$CommodityCode %in% listOfAllocations[[commodityList]]$originalSector)
#     allocMakeIndeces <- intersect(makeFileDFComAllocIndex, makeFileDFIndAllocIndex )
#     
# #    replacementList <- industryList
#     temp <- 1.5
#   }
  
  temp <- 2
  return(listOfAllocations)
}

#' Combine allocation factors of several Summary-to-Detail disaggregations to produce allocation factors will disaggregate several sectors in only one function call to disaggregateModel()
#' @param modelname String indicating which model to generate. Must be a detail level model.
#' @param detailModel Completed build of detail model. If NULL, must pass modelname.
#' @param listOfAllocations List that contains various, separate lists of disaggregation percentages for Summary-to-Detail level disaggregations. 
#' @return Allocation percentages for disaggregating multiple Summary sectors into Detail level sectors simultaneously. 
combineAllocationPercentages <- function(modelname = "USEEIOv2.0", detailModel = NULL, listOfAllocations){
  temp <- 1
  
  if(is.null(detailModel)){
    # Read in a detail level model
    # todo: check if this line needs to  be replaced by a "load summary model from repo" line if this script is to be used outside the package, e.g. USEEIO teams. 
    detailModel <- buildModel(modelname)
  }
  
  # For each list, get crosswalks of original summary sectors to disaggregated summary sectors and detail sectors 
  listOfCrosswalks <- combineAllocAndDetailCW(detailModel, listOfAllocations)
  # Get other list items that will be needed later in the function
##  combinedLists <- do.call(Map, c(f = rbind, listOfAllocations))
##  listOrder <- as.data.frame(combinedLists$originalSector)
  
  # Lists used for storing disagg values used to replace non-disagg values in the input lists. 
  listOfUseTables <- list()
  listOfMakeTables <- list()
  
  for(commodityList in 1:length(listOfAllocations)){
    temp <- 2
    
    otherListsOriginalSectors <- sapply(listOfAllocations[-(commodityList)], function(i)i[["originalSector"]])
   
    # Get indeces for original detail sectors of currentList
    currentComDetailIndeces <- which(detailModel$Commodities$Code %in% listOfCrosswalks[[commodityList]]$BEA_Detail)
    currentIndDetailIndeces <- which(detailModel$Industries$Code %in% listOfCrosswalks[[commodityList]]$BEA_Detail)
    
    # Get indeces for the original detail sectors
    detailSectorsList <- sapply(listOfCrosswalks, function(i)i[["BEA_Detail"]], simplify = FALSE) # Don't want to simplify in case we get lists of different lengths
    
    #Transform lists into a matrix for easier handling
    n_obs <- sapply(detailSectorsList, length) # Get number of detail sectors for each summary sector
    seq_max <- seq_len(max(n_obs)) # Get max number of detail sectors included in the lists
    detailSectorsList <- sapply(detailSectorsList, "[", i = seq_max) # Convert to matrix
    
    colnames(detailSectorsList) <- sapply(listOfAllocations, function(i)i[["originalSector"]])
    
    tableParams <- list() #Initialize list of paramters needed for calculating the combined percentages for each possible combination of sectors in listOfAllocations 
    
    for(industryList in 1:length(listOfAllocations)){
      if(industryList == commodityList){
        next # Skip intersection of current number with itself as that the allocations for that are already well defined
      }
      
      otherListComDetailIndeces <- which(detailModel$Commodities$Code %in% detailSectorsList[,industryList])
      otherListIndDetailIndeces <- which(detailModel$Industries$Code %in% detailSectorsList[,industryList])
      
      
      # Include all relevant parameters in tableParams list
      tableParams$currentComDetailIndeces <- currentComDetailIndeces
      tableParams$currentIndDetailIndeces <- currentIndDetailIndeces
      tableParams$commodityList <- commodityList
      tableParams$industryList <- industryList
      tableParams$otherListComDetailIndeces <- otherListComDetailIndeces
      tableParams$otherListIndDetailIndeces <- otherListIndDetailIndeces
      
      # Create Combined percentafes for each table for the current commodityList
      listOfUseTables[[length(listOfUseTables)+1]] <- createCombinedPercentagesByTable(detailModel, "Use", listOfCrosswalks, tableParams)
      listOfMakeTables[[length(listOfMakeTables)+1]] <- createCombinedPercentagesByTable(detailModel, "Make", listOfCrosswalks, tableParams)
      
      tableParams$listOfUseTables <- listOfUseTables
      tableParams$listOfMakeTables <- listOfMakeTables
      
      
      listOfAllocations <- assignCombinedPercentagesByTable(listOfAllocations, listOfCrosswalks, tableParams)
      #Finding locations of current commodities to replace in the original listOfallocations
      
      # # For UseFileDF
      # # Finding useFileDF indeces that contain values to replace
      # useFileDFComAllocIndex <- which(listOfAllocations[[commodityList]]$UseFileDF$CommodityCode %in% unique(listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc))
      # useFileDFIndAllocIndex <- which(listOfAllocations[[commodityList]]$UseFileDF$IndustryCode %in% listOfAllocations[[industryList]]$originalSector)
      # allocUseIndeces <- intersect(useFileDFComAllocIndex, useFileDFIndAllocIndex)
      # 
      # # Formatting percentages to include in UseFileDF
      # allocDFUse <- reshape2::melt(t(t(listOfUseTables[[length(listOfUseTables)]])), id.vars=1) # Need double transpose to get the right melt shape and row ordering
      # allocDFUse <- allocDFUse[,c(2,1,3)] #reorder columns
      # 
      # colnames(allocDFUse) <- c("IndustryCode", "CommodityCode", "PercentUse")
      # noteDF <-  data.frame(Note = I(rep("CommodityDisagg", nrow(allocDFUse))))
      # allocDFUse <- cbind(allocDFUse, noteDF)
      # 
      # # listOfAllocations[[commodityList]]$UseFileDF <- rbind(listOfAllocations[[commodityList]]$UseFileDF[1:(allocUseIndeces[1]-1),],
      # #                                                 allocDFUse,
      # #                                                 listOfAllocations[[commodityList]]$UseFileDF[-(1:(allocUseIndeces[length(allocUseIndeces)]+1)),]
      # # ) # TODO: Need to make sure to add both commodity and industry disagg (as per note column) to the same list. 
      # 
      # # For MakeFileDF
      # # Finding useFileDF indeces that contain values to replace
      # makeFileDFComAllocIndex <- which(listOfAllocations[[commodityList]]$MakeFileDF$CommodityCode %in% unique(listOfCrosswalks[[commodityList]]$USEEIO_Code_Loc))
      # makeFileDFIndAllocIndex <- which(listOfAllocations[[commodityList]]$MakeFileDF$IndustryCode %in% listOfAllocations[[industryList]]$originalSector)
      # allocMakeIndeces <- intersect(makeFileDFComAllocIndex, makeFileDFIndAllocIndex )
      # 
      # #TODO: Insert allocDF Use and Make in their repestive places, e.g., in allocUseIndeces' place in listOfAllocations[[commodityList]]$UseFileDF
      # allocDFMake <- reshape2::melt(t(t(listOfMakeTables[[length(listOfMakeTables)]])), id.vars=1)
      # colnames(allocDFMake) <- c("IndustryCode", "CommodityCode", "PercentMake")
      # noteDF <-  data.frame(Note = I(rep("IndustryDisagg", nrow(allocDFMake))))
      # allocDFMake <- cbind(allocDFMake, noteDF)
      # 
      # # Finding locations of current industries to replace in the original list of allocations
      # if(commodityList > industryList){
      #   
      #   useFileDFIndAllocIndex <- which(listOfAllocations[[industryList]]$UseFileDF$IndustryCode %in% unique(listOfCrosswalks[[industryList]]$USEEIO_Code_Loc))
      #   useFileDFComAllocIndex <- which(listOfAllocations[[industryList]]$UseFileDF$CommodityCode %in% listOfAllocations[[commodityList]]$originalSector)
      #   allocUseIndeces <- intersect(useFileDFComAllocIndex, useFileDFIndAllocIndex)
      #   
      #   
      #   temp <- 1.5
      # }
      # 

      temp <- 2
    }# End of for industryList loop
    
    temp <- 2
  } # End of for commodityList loop
  
  # Combine all lists into one list
  combinedLists <- do.call(Map, c(f = rbind, listOfAllocations))
  # Remove unneeded rows in combinedLists that still contain original summary Sector codes. 
  combinedLists <- removeOriginalCodesFromAllocTable(combinedLists)

  temp <- 3 
  return(combinedLists)
  
}


#' Generate the allocation percentages for non-intersection portions of the Use and Make tables
#' @param disaggParams List of disaggregation paramaters
#' @param sector String, name of sector being disaggregated
#' @param outputDF Dataframe containing the allocation factors for the current sector
#' @param vectorToDisagg String indicating whether a "Column" or "Row" is being disaggregated. These are the only two permitted values for this paramter.
#' @return Allocation percentages for disaggregating the non-intersection portion of the summary level model into the detail level for the current vectorToDisagg.
nonIntersectionAllocation <- function (disaggParams, sector, outputDF, vectorToDisagg){
  # Build current section of outputDF with current industry/commodity allocations
  
  # If there are more than 1 detail sectors to the summary sector, need to change dataframe from wide to long format.
  if(length(disaggParams$currentDetailIndeces) > 1){
    allocDF <- disaggParams$currentDetailVector/sum(disaggParams$summarySectorVectorSums)
    
    if(vectorToDisagg == "Column"){
      if(is.null(dim(allocDF))){
        allocDF <- data.frame(sum(allocDF)) 
      }else{
        allocDF <- data.frame(colSums(allocDF)) 
      }
      
    }else if(vectorToDisagg == "Row"){
      if(is.null(dim(allocDF))){
        allocDF <- data.frame(sum(allocDF)) 
      }else{
        allocDF <- data.frame(rowSums(allocDF)) 
      }
    }
    
  }else{
    allocDF <- data.frame(Percent = disaggParams$currentDetailVector/sum(disaggParams$summarySectorVectorSums))
  }
  
  specificDetailIndex <- which(rownames(allocDF) == disaggParams$specificDetailLevelSector)
  # Determine whether we want to create allocation values for all detail level sectors or just one
  if(!(is.null(disaggParams$specificDetailLevelSector))){# If a particular detail level sector is provided, re-calculate the allocation factors for other sectors
    
    # Create new DF to house modified allocation values.
    # The allocations are split between the specific detail sector provided and a new sector containing
    # all other sectors part of the summary level sector except for the specific detail sector
    
    if(length(specificDetailIndex) == 0){ 
      # If the specific sector does not exist for this Table/Vector combination 
      # (i.e., commodity does not exist, only industry, so it is only present as a Make table row rather than a column)
      newAlloc <- data.frame(matrix(ncol = 1, nrow = 1))
      newAlloc[1,1] <- colSums(allocDF)
      
      colnames(newAlloc) <- disaggParams$allocName
      # Code for the new, "all other" sector is the same as the summary level sector, but with an "X" appended at the end of the summary code
      rownames(newAlloc) <- c(paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      
      allocDF <- newAlloc
      # detailCodeOutputIndex determines whether the summaryCode goes under the Industry or Commodity column in the output file. 
      if(disaggParams$detailCodeOutputIndex == 1){
        industryDF <- data.frame(IndustryCode =I(rownames(newAlloc)))
        commodityDF <- data.frame(CommodityCode = I(rep(paste(sector, disaggParams$summaryLoc_Code, sep = "/"), nrow(industryDF))))
        noteDF <- data.frame(Note = I(rep("IndustryDisagg", nrow(industryDF))))
        
      }else{
        commodityDF <- data.frame(CommodityCode =I(rownames(newAlloc)))
        industryDF <- data.frame(IndustryCode = I(rep(paste(sector, disaggParams$summaryLoc_Code, sep = "/"), nrow(commodityDF))))
        noteDF <- data.frame(Note = I(rep("CommodityDisagg", nrow(commodityDF))))
      }
      currentDF <- cbind(industryDF, commodityDF, allocDF[1], noteDF)
      
      outputDF <- rbind(outputDF, currentDF)
      
      
    }else{ #If the specific sector exists as both a commodity and industry for this Table
      newAlloc <- data.frame(matrix(ncol = 1, nrow = 2))
      newAlloc[1,1] <- allocDF[specificDetailIndex,1]
      newAlloc[2,1] <- colSums(allocDF) - allocDF[specificDetailIndex,1]
      
      colnames(newAlloc) <- disaggParams$allocName
      # Code for the new, "all other" sector is the same as the summary level sector, but with an "X" appended at the end of the summary code
      rownames(newAlloc) <- c(disaggParams$specificDetailLevelSector, paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      
      allocDF <- newAlloc
      # detailCodeOutputIndex determines whether the summaryCode goes under the Industry or Commodity column in the output file. 
      if(disaggParams$detailCodeOutputIndex == 1){
        industryDF <- data.frame(IndustryCode =I(rownames(newAlloc)))
        commodityDF <- data.frame(CommodityCode = I(rep(paste(sector, disaggParams$summaryLoc_Code, sep = "/"), nrow(industryDF))))
        noteDF <- data.frame(Note = I(rep("IndustryDisagg", nrow(industryDF))))
        
      }else{
        commodityDF <- data.frame(CommodityCode =I(rownames(newAlloc)))
        industryDF <- data.frame(IndustryCode = I(rep(paste(sector, disaggParams$summaryLoc_Code, sep = "/"), nrow(commodityDF))))
        noteDF <- data.frame(Note = I(rep("CommodityDisagg", nrow(commodityDF))))
      }
      currentDF <- cbind(industryDF, commodityDF, allocDF[1], noteDF)
      
      outputDF <- rbind(outputDF, currentDF)
      
    }

  }else{ 
    # If a particular detail level is not provided, just carry on with current allocation factors 
    # and assign them to the correct places in the DFs
    colnames(allocDF)[1] <- disaggParams$allocName
    
    # detailCodeOutputIndex determines whether the summaryCode goes under the Industry or Commodity column in the output file. 
    if(disaggParams$detailCodeOutputIndex == 1){
      industryDF <- data.frame(IndustryCode =I(paste(disaggParams$summaryCodeCw, disaggParams$summaryLoc_Code, sep = "/")))
      commodityDF <- data.frame(CommodityCode = I(rep(paste(sector, disaggParams$summaryLoc_Code, sep = "/"), nrow(industryDF))))
      noteDF <- data.frame(Note = I(rep("IndustryDisagg", nrow(industryDF))))
      
    }else{
      commodityDF <- data.frame(CommodityCode =I(paste(disaggParams$summaryCodeCw, disaggParams$summaryLoc_Code, sep="/")))
      industryDF <- data.frame(IndustryCode = I(rep(paste(sector, disaggParams$summaryLoc_Code, sep = "/"), nrow(commodityDF))))
      noteDF <- data.frame(Note = I(rep("CommodityDisagg", nrow(commodityDF))))
    }
    
    currentDF <- cbind(industryDF, commodityDF, allocDF[1], noteDF)
    
    outputDF <- rbind(outputDF, currentDF)
  }
  
  return(outputDF)
  
}

#' Generate the allocation percentages for intersection portions of the Use and Make tables
#' @param disaggParams List of disaggregation paramaters
#' @param Table String that denotes which table the allocation values refer to. Can be either "Make" or "Use"
#' @param vectorToDisagg String indicating whether a "Column" or "Row" is being disaggregated. These are the only two permitted values for this paramter.
#' @param specificDetailLevelSector String that denotes which table the allocation values refer to. Can be either "Make" or "Use"
#' @return Allocation percentages for disaggregating the non-intersection portion of the summary level model into the detail level for the current vectorToDisagg.
intersectionAllocation <- function (disaggParams, Table, outputDF, vectorToDisagg){
  
  originalVector <- disaggParams$originalTable[disaggParams$detailRowIndeces, disaggParams$detailColIndeces, drop = FALSE]# Get detail intersection
  
  originalVectorSum <- sum(sum(originalVector))# Get sum of detail intersection
  allocationVector <- originalVector/originalVectorSum # Divide each element in intersection by intersection sum to get allocation value
  
  # Create new DF to house modified allocation values.
  # The allocations are split between the specific detail sectors and a new sector containing
  # all other sectors part of the summary level sector except for the specific detail sector
  specificDetailRowIndex <- which(rownames(allocationVector) == disaggParams$specificDetailLevelSector)
  specificDetailColIndex <- which(colnames(allocationVector) == disaggParams$specificDetailLevelSector)
  
  
  if(!(is.null(disaggParams$specificDetailLevelSector))){
    # If a particular detail level sector is provided, re-calculate the allocation factors for other sectors
    

    if(length(specificDetailColIndex)==0 && length(specificDetailRowIndex)==0){
      # Should never happen
      stop("No sector selected for calculating the allocation of its intersection")
    }
    
    if(length(specificDetailColIndex) ==  0){ # If specific column index does not exist for this Table/Vector combination (e.g., industry doesn't exist for Use table)

      newAlloc <- data.frame(matrix(ncol =1, nrow = 1)) # Create DF for new intersection; as it is missing one index, then we only need to allocate to the other
      # Assign proper allocation to intersection
      newAlloc [1,1] <- sum(allocationVector[-(specificDetailRowIndex),]) # Should add up to 1
      # Rename cols and rows appropriately
      colnames(newAlloc) <- c(paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      rownames(newAlloc) <- c(paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      allocationVector <- newAlloc
      
    }else if(length(specificDetailRowIndex) == 0){ # If specific row index does not exist for this Table/Vector combination (e.g., commodity doesn't exist for Use table)

      newAlloc <- data.frame(matrix(ncol =1, nrow = 1)) # Create DF for new intersection; as it is missing one index, then we only need to allocate to the other
      # Assign proper allocation to intersection
      newAlloc[1,1] <- sum(allocationVector[,-(specificDetailColIndex)]) # Should add up to 1
      # Rename cols and rows appropriately
      colnames(newAlloc) <- c(paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      rownames(newAlloc) <- c(paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      allocationVector <- newAlloc
      
    }else{ # If both are speciied (as should be the case most of time)
      
      # Create new DF to house modified allocation values.
      # The allocations are split between the specific detail sectors and a new sector containing
      # all other sectors part of the summary level sector except for the specific detail sector
      newAlloc <- data.frame(matrix(ncol = 2, nrow = 2))
      
      # Need to add values in the originalVector that correspond various parts of the intersection:
      # Assigning of intersection of specificDetailLevelSector with itself
      newAlloc[1,1] <- allocationVector[specificDetailRowIndex,specificDetailColIndex]
      
      
      # Addition of all rows (except specificDetailRowIndex) under  (bottom left section of newAlloc)
      summaryRowAndDetailColValue <- colSums(data.frame(allocationVector[-(specificDetailRowIndex),specificDetailColIndex]))
      newAlloc[2,1] <- summaryRowAndDetailColValue
      
      # Addition of all columns (except specificDetailColIndex) along  (top right section of newAlloc)
      summaryColAndDetailRowValue <- colSums(data.frame(allocationVector[specificDetailRowIndex, -(specificDetailColIndex)]))
      newAlloc[1,2] <- summaryColAndDetailRowValue
      
      # Addition of all columns and rows excluding specificDetailColIndex and specificDetailRowIndex (bottom right section of newAlloc)
      summaryRowAndSummaryColValue <- sum(sum((data.frame(allocationVector[-(specificDetailRowIndex), -(specificDetailColIndex)]))))
      newAlloc[2,2] <- summaryRowAndSummaryColValue
      
      colnames(newAlloc) <- c(disaggParams$specificDetailLevelSector, paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      rownames(newAlloc) <- c(disaggParams$specificDetailLevelSector, paste(paste(disaggParams$summaryCode,"X", sep=""), disaggParams$summaryLoc_Code, sep = "/"))
      
      allocationVector <- newAlloc
    }
    
  } # End of if(!is.null) statement
  
  if(Table == "Use"){
    # Transpose vector to get allocations in correct order when reshaping
    allocationVector <- t(allocationVector)
    allocDF <- reshape2::melt(allocationVector, id.vars=1)# Reshape from wide to long DF format
    # Need to change order of columns after reshaping to ensure Industry is in column 1.
    allocDF <- allocDF[,c(2,1,3)]
  }else{
    if(!(is.null(disaggParams$specificDetailLevelSector))){# This if statement is necessary for the specific disagg case because otherwise the matrix will not be generated appropriately
      allocDF <- reshape2::melt(as.matrix(allocationVector), id.vars=1)
    }else{
      allocDF <- reshape2::melt(allocationVector, id.vars=1)# Reshape from wide to long DF format
      
    }
  }
  
  colnames(allocDF) <- c("IndustryCode", "CommodityCode", disaggParams$allocName)# Rename DF columns
  noteDF <-  data.frame(Note = I(rep("IntersectionDisagg", nrow(allocDF))))# Add a note for output DF to indicate intersectiond disaggregation
  outputDF <- cbind(allocDF, noteDF)
  
  return(outputDF)
  
}


#' Generate the environmental allocation percentages required to disaggregate environmental to detail level. 
#' @param detailModel Model file loaded with IO tables
#' @param summaryCode String containing summary level code to be disaggregated
#' @param summaryCodeCw List of detail sectors that map to the summary level sector to be disaggregated
#' @return Allocation percentages for disagggregating the summary level model into the detail level model for the specific sector using the disaggregation fuctions.
generateEnvironmentalAllocations <- function (disaggParams){
  
  # Initialize dataframe that contains allocation values
  outputDF <- data.frame(Flowable = character(), Context = character(), FlowUUID = character(), Sector = character(), FlowAmount = double())
  
  TbSRowIndeces <- which(disaggParams$detailModel$TbS$Sector %in% disaggParams$summaryCodeCw) # Row indeces that contain one of detail sector codes in the Sector field.
  TbSColIndeces <- c(1,2,3,5,7) # Column indeces that correspond to the Flowable, Context,	FlowUUID,	Sector, and	FlowAmount columns, needed to create the Env disagg file.
  
  TbSFlows <- disaggParams$detailModel$TbS[TbSRowIndeces, TbSColIndeces] # Subset of TbS with only the rows for the relevant detail sectors.
  uniqueFlows <- unique(TbSFlows[,c('Flowable','Context')]) # Subset of unique flow/context combinations for the relevant detail sectors, needed to be able to sum the correct flows.
  
  for(curFlow in 1:nrow(uniqueFlows)){
    flow <- uniqueFlows[curFlow, ]
    # Get indeces that match flowable, context, and releavant detail sectors in TbS dataframe
    currentTbsIndeces <- which(disaggParams$detailModel$TbS$Flowable %in% flow$Flowable & 
                                 disaggParams$detailModel$TbS$Context %in% flow$Context & 
                                 disaggParams$detailModel$TbS$Sector %in% disaggParams$summaryCodeCw)
    
    # Get current subset from TbS dataframe
    currentTbSFlows <- disaggParams$detailModel$TbS[currentTbsIndeces, TbSColIndeces]
    
    # If we are disaggregating only one detail level sector  
    if(!is.null(disaggParams$specificDetailLevelSector)){
      
      specificCode <- strsplit(disaggParams$specificDetailLevelSector, "/")[[1]]
      specificCode <- specificCode[1]
      specificDetailRowIndex <- which(currentTbSFlows$Sector == specificCode) # Get index for code we want to keep
      
      if(length(specificDetailRowIndex!=0)){ # If specific sector does contain an amount for this specific flow
        specificTBSFlow <- currentTbSFlows[specificDetailRowIndex,] # Get row containing specific index, if it exists
        specificTBSFlow$FlowAmount <- specificTBSFlow$FlowAmount/sum(currentTbSFlows$FlowAmount)
        outputDF <- rbind(outputDF, specificTBSFlow)
        
      }
      remainingTBSFlows <- which(currentTbSFlows$Sector != specificCode)
      
      if(length(remainingTBSFlows)!=0){ # If there are other sectors with this flow besides the specific sector
        newTBSFlow <- currentTbSFlows[1,] # DF to store allocation of new summary level sector (e.g., 22X)
        newTBSFlow$Sector <- paste0(disaggParams$summaryCode,"X") # Add new sector code to DF 
        newTBSFlow$FlowAmount <-  sum(currentTbSFlows$FlowAmount[remainingTBSFlows])/sum(currentTbSFlows$FlowAmount)
        outputDF <- rbind(outputDF, newTBSFlow)
      }
      

    } else{ # If we are disaggregating all detail level sectors mapped to this summary sector
      # Calculate amount ratios for current subset
      currentRatios <- currentTbSFlows$FlowAmount/sum(currentTbSFlows$FlowAmount)
      # Replace amounts with ratios in current subset
      currentTbSFlows$FlowAmount <- currentRatios
      # Bind current subset to output DF
      outputDF <- rbind(outputDF, currentTbSFlows)
      
    }# End of If-Else for !is.null(disaggParams$specificDetailLevelSector)
    
  } # End of for current flow loop
  
  outputDF$FlowAmount[is.na(outputDF$FlowAmount)] <- 0 # Remove all NAs, product of division by 0 in some flows.  
  names(outputDF)[names(outputDF) == 'FlowAmount'] <- 'FlowRatio'
  
  return(outputDF)
  
}


#' Generate the _Sectors.csv file that contains a list of sectors to disaggregate
#' @param disaggParams List of disaggregation parameters
#' @return Allocation percentages for disagggregating the summary level model into the detail level model for the specific sector using the disaggregation fuctions.
createSectorsCSV <- function (disaggParams){
 
  # TODO: Check to see if the sectorsCSV file is already present

  # Add values in the NAICS columns for the sectors without NAICS marked in disaggParams
  
  noNAICSIndeces <- which(disaggParams$detailModel$crosswalk$BEA_Detail %in% disaggParams$sectorsWithoutNAICS)
  disaggParams$detailModel$crosswalk$NAICS[noNAICSIndeces] <- unlist(disaggParams$sectorsWithoutNAICS)
  
  
  # Initialize dataframe that contains allocation values
  sectorIndeces <- which(disaggParams$detailModel$crosswalk$BEA_Summary %in% disaggParams$summaryCode) # Find indeces in crosswalk that match summary level sector to be disaggregated
  sectorDF <- disaggParams$detailModel$crosswalk[sectorIndeces,] # Get part of crosswalk corresponding to the summary level code to be disaggregated
  sectorDF <- sectorDF[nchar(sectorDF$NAICS) == 6, ]   # Get only rows that have 6 digit NAICS codes
  outputDF <- data.frame(sectorDF[-c(2:4)]) # Keep only the NAICS and USEEIO 6-digit codes
  outputDF[,2] <- data.frame(paste0(outputDF[,2], "/", disaggParams$summaryLoc_Code), stringsAsFactors = FALSE) # Add location code to USEEIO code column
  
    # Get index of detail sectors in detailModel$Commodity to copy descriptions in that model object    
    detailIndeces <- which(disaggParams$detailModel$Commodities$Code %in% disaggParams$summaryCodeCw)
    
    # Get descriptions from the commodity
    descriptionsDF <- data.frame(matrix(nrow = nrow(outputDF), ncol = 4))
    for(dfRow in 1:nrow(outputDF)){
      outputRow <- outputDF[dfRow,]
      
      # If we are disaggregating only the specificDetailLevelSector, and not the other detail level sectors mapped to the current summary level sector 

      if(!is.null(disaggParams$specificDetailLevelSector)){ #new block
        
        # If outputRow currently refers to a detail level sector other than the specificDetailLevelSector
        if((outputRow[1,2] != disaggParams$specificDetailLevelSector)){
          # Specify the details for Sector description
          newSectorCode <- paste0(disaggParams$summaryCode, "X/", disaggParams$summaryLoc_Code)
          sectorName <- paste0(disaggParams$summaryCode, "/", disaggParams$summaryLoc_Code, " without ", disaggParams$specificDetailLevelSector)
          sectorCategory <- newSectorCode
          sectorSubcategory <- newSectorCode
          sectorDescription <-paste0(disaggParams$summaryCode, "/", disaggParams$summaryLoc_Code, " after separating ", disaggParams$specificDetailLevelSector)
          descriptionsDF[dfRow, ] <- c(sectorName, sectorCategory, sectorSubcategory, sectorDescription)
          
          # Make sure to mark this NAICS code with the new, modified summary level sector that does not contain the specific detail level sector (e.g. 22X)
          outputDF[dfRow,2] <- newSectorCode
          
          next
        }
        

      } # End of for dfRow loop
      
      commodityIndex <- which(disaggParams$detailModel$Commodities$Code_Loc %in% outputRow[,2])
      
      # If the description is available in the model$Commodities object
      if(length(commodityIndex != 0)){
        descriptionsDF[dfRow, ] <- disaggParams$detailModel$Commodities[commodityIndex, 2:5]
        
      }else{ # If the description is not available in the model$Commodities object, use default descriptions
        sectorName <- paste0("Unspecific name for code ",outputRow[,2])
        sectorCategory <- disaggParams$summaryCode
        sectorSubcategory <- paste0("unspecific subcategory for code ", outputRow[,2])
        sectorDescription <- paste0("Unspecific description for code ", outputRow[,2])
        
        descriptionsDF[dfRow, ] <- c(sectorName, sectorCategory, sectorSubcategory, sectorDescription)
        
      }
      
    }# end of for dfRow loop
    
    colnames(descriptionsDF) <- colnames(disaggParams$detailModel$Commodities[commodityIndex, 2:5])
    colnames(descriptionsDF)[1] <- c("USEEIO_Name")
    colnames(outputDF) <- c("NAICS_2012_Code","USEEIO_Code")
    outputDF <- cbind(outputDF, descriptionsDF)


  return(outputDF)
  
  
}


#' Write allocation dataframes to csv files at the specific directory
#' @param outputDF A dataframe containing a list of outputDFs to write to CSV: Use table, Make table, and environmental allocations for TbS object, as well as the Sectors CSV. 
#' @param disaggParams List of disaggregation parameters
#' @param writePath String that specifies a path to write allocation csv files to
#' @description Write allocation dataframes to csv files at the specific directory
writeAllocationsToCSV <- function(outputDF, disaggParams, writePath = NULL){
  
  # Path pointing to write directory
  if(is.null(writePath)){
    writePath <- "inst/extdata/disaggspecs/"
  }else if(toupper(writePath) == "NOCSV"){
    # If writePath = "NoCSV", skip the rest of this function (don't print to csv)
    return()
  }else{
    # Check that path exists
    if(dir.exists(writePath) == FALSE){
      stop(paste0(writePath," does not exist."))
    }
  }
  
  # If we are creating csvs to disaggregate a specific detail level sector rather than all detail level sectors mapped to the summary level sector
  if(!is.null(disaggParams$specificDetailLevelSector)){
    detailCode <- strsplit(disaggParams$specificDetailLevelSector,"/")[[1]]
    detailCode <- detailCode[1]
    filePrefix <- paste0("S",disaggParams$summaryCode,"To",detailCode,"Disagg")
    
  }else{
    filePrefix <- paste0("S",disaggParams$summaryCode,"ToBEADetDisagg")# S for summary
    
  }
 
  #filePrepend <- "UtilityDisaggregationSummary"
  useAllocFileName <- paste0(writePath, filePrefix, "_Use.csv")
  makeAllocFileName <- paste0(writePath, filePrefix, "_Make.csv")
  envAllocFileName <- paste0(writePath, filePrefix, "_Env.csv")
  sectorsFileName <- paste0(writePath, filePrefix,"_Sectors.csv")
  
  write.csv(outputDF$UseFileDF, useAllocFileName, row.names = FALSE)
  write.csv(outputDF$MakeFileDF, makeAllocFileName, row.names = FALSE)
  write.csv(outputDF$EnvFileDF, envAllocFileName, row.names = FALSE)
  write.csv(outputDF$NAICSSectorCW, sectorsFileName, row.names = FALSE)
  
}

#' Generate the economic allocation percentages required to disaggregate the columns of the make and use tables. 
#' Note that this function is desgined to work with model$V and model$U objects, rather the the intermediary model$MakeTransactions and UseTransactions objects.
#' @param disaggParams List of disaggregation paramaters
#' @param Table String that denotes which table the allocation values refer to. Can be either "Make" or "Use"
#' @param vectorToDisagg String that denotes whether to disagg rows or columns. Only acceptable string values are "Row", "Column", or "Intersection"
#' @return Allocation percentages for disagggregating the summary level model into the detail level model for the specific sector using the disaggregation fuctions.
generateEconomicAllocations <- function (disaggParams, Table, vectorToDisagg){
  
  # Initialize dataframe that contains allocation values
  outputDF <- data.frame(IndustryCode = character(), CommodityCode = character(), PercentUse = double(), Note = character())
  # Get a list of all summary sectors
  summarySectorList <- as.list(unique(disaggParams$detailModel$crosswalk$BEA_Summary))
  
  # Obtain the correct table and disaggregation parameters
  if(Table == "Use"){
    originalTable <- disaggParams$detailModel$U
    # Bind ValueAdded codes for Commodities and FinalDemand codes for Industries to match dimensions of as model$U object, 
    # which includes VA and FD values. 
    originalRowCodes <- rbind(disaggParams$detailModel$Commodities[c("Code", "Name", "Code_Loc")], 
                              disaggParams$detailModel$ValueAddedMeta)
    originalColCodes <- rbind(disaggParams$detailModel$Industries[c("Code", "Name", "Code_Loc")], 
                              disaggParams$detailModel$FinalDemandMeta[c("Code", "Name", "Code_Loc")])
    
    allocName <- "PercentUse"
    
    # DetailCodeOutput index indicates which column in the output data to assign the detail (disaggregated) codes. 
    # For Use column disagg (industries) the index is 1. 
    # For Use row disagg (commodities) the index is 2.
    if(vectorToDisagg == "Column"){
      detailCodeOutputIndex <- 1
      summaryCodeOutputIndex <- 2
      
      # Get indeces of the detail level columns that match the the summary level code
      detailDisaggIndeces <- which(originalColCodes$Code %in% disaggParams$summaryCodeCw)
      detailOutputNames <- originalColCodes$Code_Loc[detailDisaggIndeces]
      
    }else if(vectorToDisagg == "Row"){
      detailCodeOutputIndex <- 2
      summaryCodeOutputIndex <- 1
      
      detailDisaggIndeces <- which(originalRowCodes$Code %in% disaggParams$summaryCodeCw)
      detailOutputNames <- originalRowCodes$Code_Loc[detailDisaggIndeces]
    }else{
      # For Use intersection
      detailRowIndeces <- which(originalRowCodes$Code %in% disaggParams$summaryCodeCw)
      detailColIndeces <- which(originalColCodes$Code %in% disaggParams$summaryCodeCw)
      detailOutputNames <- disaggParams$summaryCodeCw
    }
    
  }else{
    originalTable <- disaggParams$detailModel$V
    originalRowCodes <- disaggParams$detailModel$Industries
    # Limit colCodes object to three columns as in the "Use" case for consistency
    originalColCodes <- disaggParams$detailModel$Commodities[c("Code", "Name", "Code_Loc")]
    allocName <- "PercentMake"
    
    # Detail code output index for Make column disagg (commodities) is 2.
    # For make row disagg (industries) the code index is 1.
    if(vectorToDisagg == "Column"){
      detailCodeOutputIndex <- 2
      summaryCodeOutputIndex <- 1  
      
      detailDisaggIndeces <- which(originalColCodes$Code %in% disaggParams$summaryCodeCw)
      detailOutputNames <- originalColCodes$Code_Loc[detailDisaggIndeces]
      
    }else if (vectorToDisagg == "Row"){
      detailCodeOutputIndex <- 1
      summaryCodeOutputIndex <- 2
      
      detailDisaggIndeces <- which(originalRowCodes$Code %in% disaggParams$summaryCodeCw)
      detailOutputNames <- originalRowCodes$Code_Loc[detailDisaggIndeces]
    }else{
      # For Make intersection
      detailRowIndeces <- which(originalRowCodes$Code %in% disaggParams$summaryCodeCw)
      detailColIndeces <- which(originalColCodes$Code %in% disaggParams$summaryCodeCw)
      detailOutputNames <- disaggParams$summaryCodeCw
    }
    
  }
  
  if(vectorToDisagg == "Intersection"){
    # Generate allocation percentates for the intersection disaggregation
    disaggParams$originalTable <- originalTable
    disaggParams$detailRowIndeces <- detailRowIndeces
    disaggParams$detailColIndeces <- detailColIndeces
    disaggParams$allocName <- allocName
    
    outputDF <- intersectionAllocation(disaggParams, Table, outputDF, vectorToDisagg)

  }else{
    # Calculate allocation percentages for each summary level commodity
    for (sector in summarySectorList){

      # Get summary to detail mapping of the current sector (row) 
      currentDetailSectors <- subset(disaggParams$detailModel$crosswalk, BEA_Summary %in% sector)
      currentDetailSectors <- as.list(unique(currentDetailSectors$BEA_Detail))
      
      # Get list of currentDetailIndeces. These are the detail indeces that map to the vector not being disaggregated
      # I.e., if vectorToDisagg == "Column", then currentDetailIndeces refer to row indeces. 
      if(vectorToDisagg == "Column"){
        currentDetailIndeces <- which(originalRowCodes$Code %in% currentDetailSectors)
        currentDetailVector <- originalTable[currentDetailIndeces, detailDisaggIndeces]
        
      }else if(vectorToDisagg == "Row"){
        currentDetailIndeces <- which(originalColCodes$Code %in% currentDetailSectors)
        currentDetailVector <- originalTable[detailDisaggIndeces, currentDetailIndeces]
      }
      
      # Find vector sums. If statements necessary to avoid error in case currentDetailIndeces (i.e., summary to detail level mapping is 1:1) or detailDisaggIndeces are of is of length 1 (i.e., disaggregating a summary level sector into different numbers of commodities and industries at the detail level)
      # Also this if statement is necessary prior to calculating allocDF below to check whether it is necessary to calculate allocation factors or if there are no values in the current vector.
      if(length(currentDetailIndeces) > 1){
        if(vectorToDisagg == "Column"){
          if(length(detailDisaggIndeces) == 1){
           summarySectorVectorSums <- sum(currentDetailVector)
          }else{
           summarySectorVectorSums <- colSums(currentDetailVector)
          }

        }else if(vectorToDisagg == "Row"){
          if(length(detailDisaggIndeces) == 1){
            summarySectorVectorSums <- sum(currentDetailVector)
          }else{
            summarySectorVectorSums <- rowSums(currentDetailVector)
          }
        }
        
      }else{
        summarySectorVectorSums <- currentDetailVector
        
      }
      
      # If the current set of detail sectors are not all 0, then we need to perform an allocation to disaggregate.
      if(sum(summarySectorVectorSums) !=0){ 
        
        # Initialize paramters for function non-intersection allocation function call
        disaggParams$currentDetailIndeces <- currentDetailIndeces
        disaggParams$currentDetailVector <- currentDetailVector
        disaggParams$summarySectorVectorSums <- summarySectorVectorSums
        disaggParams$detailCodeOutputIndex <- detailCodeOutputIndex
        disaggParams$allocName <- allocName
        
        # The allocation values of the intersection of the summary sector with itself are calculated differently from the allocation values of the rest of the column
        if(sector != disaggParams$summaryCode){
          
          outputDF <- nonIntersectionAllocation(disaggParams, sector, outputDF, vectorToDisagg)

        }
        
      }
      else{
        # If sum of detail level colums for the current row = 0, don't need to add allocation of the current detail rows to the allocation dataframe.
        next
        
        
      }# End of if(sum(summarySectorVectorSums)) !=0 statement
      
      
    }# End of for sector loop
    
  }# End of else statement for disaaggregating non-intersection vectors
  
  
  
  rownames(outputDF) <- 1:nrow(outputDF)
  
  return(outputDF)
  
}

#' For target disaggregation sectors, check if they are IndustryOnly or CommodityOnly by comparing to detailModel
#' @param disagg 
#' @param detailModel
#' @return disagg with IndustryOnly or CommodityOnly lists added if necessary
checkAsymmetricalDisaggregation <- function(disagg, detailModel){
  disagg$IndustryOnly <- list()
  disagg$CommodityOnly <- list()
  for (sector in disagg$DisaggregatedSectorCodes){
    if((sector %in% detailModel$Commodities$Code_Loc) &
       (!sector %in% detailModel$Industries$Code_Loc)){
      disagg$CommodityOnly <- append(disagg$CommodityOnly, sector)
    }
    else if((sector %in% detailModel$Industries$Code_Loc) &
         (!sector %in% detailModel$Commodities$Code_Loc)){
      disagg$IndustryOnly <- append(disagg$IndustryOnly, sector)
    }
  }
  
  if(length(disagg$IndustryOnly) == 0) {
    disagg$IndustryOnly <- NULL
  }
  if(length(disagg$CommodityOnly) == 0) {
    disagg$CommodityOnly <- NULL
  }
  return(disagg)
}
