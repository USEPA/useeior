#' Disaggregate a specific sector in a summary level model to detail level
#' @param modelname String indicating which model to generate. Must be a detail level model.
#' @param sectorToDisaggregate String with the summary level code of the sector to be disaggregated from Summary to Detail Level
#' @return A summary level model with the specified sectors disaggregated at the Detail level.
disaggregateSummaryModel <- function (modelname = "USEEIO2.0_nodisagg", sectorToDisaggregate = NULL){
  # Check for appropriate input in sectorToDisaggregate and make sure format matches BEA_Summary column in model$crosswalk.
  if(is.null(sectorToDisaggregate)){
    stop("No summary level sector specified for disaggregation to detail level")
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
  
  # Read in a detail level model
  # todo: check if this line needs to  be replaced by a "load summary model from repo" line if this script is to be used outside the package, e.g. USEEIO teams. 
  detailModel <- buildModel(modelname)#build summary model
  
  # Get the detail sector codes that correspond to the summary code to be disaggregated
  summaryCodeCw <- subset(detailModel$crosswalk, BEA_Summary %in% summaryCode)
  summaryCodeCW <- as.list(unique(summaryCodeCw$BEA_Detail))
  
  useTableColAlloc <- generateColumnAllocations(detailModel, summaryCode, summaryCodeCW, summaryLoc_Code, "Use")
  #useTableRowAlloc <- generatreRowAllocations
  #makeTableColAlloc 
  #makeTableRowAlloc
  temp <-1
  
  return(detailModel)#temporay return statement
}


#' Generate the allocation percentages required to disaggregate the columns of the make and use tables. 
#' Note that this function is desgined to work with model$V and model$U objects, rather the the intermediary model$MakeTransactions and UseTransactions objects.
#' @param detailModel Model file loaded with IO tables
#' @param summaryCode String containing summary level code to be disaggregated
#' @param summaryCodeCW List of detail sectors that map to the summary level sector to be disaggregated
#' @param summaryLoc_Code String containing location code of the summary level sector to be disaggregated 
#' @param Table String that denotes which table the allocation values refer to. Can be either "Make" or "Use"
#' @return Allocation percentages for disagggregating the summary level model into the detail level model for the specified sector using the disaggregation fuctions.
generateColumnAllocations <- function (detailModel, summaryCode, summaryCodeCw, summaryLoc_Code, Table){
  # Obtain the correct table
  if(Table == "Use"){
    originalTable <- detailModel$U
    # Bind ValeAdded codes for Commodities and FinalDemand codes for Industries to match dimensions of as model$U object, which includes VA and FD values. 
    originalRowCodes <- rbind(detailModel$Commodities[c("Code", "Name", "Code_Loc")], detailModel$ValueAddedMeta)
    originalColCodes <- rbind(detailModel$Industries, detailModel$FinalDemandMeta[c("Code", "Name", "Code_Loc")])

    # DetailCodeOutput index indicates which column in the output data to assign the detail (disaggregated) codes. 
    # For Use column disagg (industries) the index is 1. 
    detailCodeOutputIndex <- 1
    summaryCodeOutputIndex <- 2
    allocName <- "PercentUse"
  }else{
    originalTable <- detailMode$V
    originalRowCodes <- detailModel$Industries
    # Limit colCodes object to three columns as in the "Use" case for consistency
    originalColCodes <- detailModel$Commodities[c("Code", "Name", "Code_Loc")]
    # Detail code output index for Make column disagg (commodities) is 2. 
    detailCodeOutputIndex <- 2
    summaryCodeOutputIndex <- 1
    allocName <- "PercentMake"
  }
  
  # Initialize dataframe that contains allocation values
  outputColNames <- c("IndustryCode", "CommodityCode", "PercentUse", "Note")
  #outputDF <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), outputColNames)
  outputDF <- data.frame(IndustryCode = character(), CommodityCode = character(), PercentUse = double(), Note = character())
  
  # Get indeces of the detail level columns that match the the summary level code
  detailColIndeces <- which(originalColCodes$Code %in% summaryCodeCw)
  detailColOutputNames <- originalColCodes$Code_Loc[detailColIndeces]
  
  # Get a list of all summary sectors
  summarySectorList <- as.list(unique(detailModel$crosswalk$BEA_Summary))
  # Calculate allocation percentages for each summary level commodity
  for (sector in summarySectorList){
    # Get summary to detail mapping of the current sector (row) 
    currentDetailSectors <- subset(detailModel$crosswalk, BEA_Summary %in% sector)
    currentDetailSectors <- as.list(unique(currentDetailSectors$BEA_Detail))
    currentDetailRowIndeces <- which(originalRowCodes$Code %in% currentDetailSectors)

    # The allocation values of the intersection of the summary sector with itself are calculated differently from the allocation values of the rest of the column
    if(sector != summaryCode){
      # Calculation allocation values for disaggregation of the intersection

      currentRows <- originalTable[currentDetailRowIndeces, detailColIndeces]      
      # Find colsums. If statement necessary to avoid error in case currentDetailROwIndeces is of length 1, i.e., summary to detail level mapping is 1:1
      if(length(currentDetailRowIndeces) > 1 ){
        summarySectorColSums <- colSums(currentRows)
      }else{
        summarySectorColSums <- currentRows
      }

      # If sum of detail level colums for the current row = 0, don't need to add allocation of the current detail rows to the allocation data.
      if(sum(summarySectorColSums) != 0){
        # Build current section of outputDF with current industry/commodity allocations
        ##allocDF <- data.frame(Percent = currentRows/sum(summarySectorColSums))
        
        # If there are more than 1 detail sectors to the summary sector, need to change dataframe from wide to long format.
        ##if(length(allocDF) >1 ){
        if(length(currentDetailRowIndeces) > 1){  
          allocDF <- currentRows/sum(summarySectorColSums)
          allocDF <- data.frame(colSums(allocDF))
          ##allocDF <- stack(allocDF)
        }else{
          allocDF <- data.frame(Percent = currentRows/sum(summarySectorColSums))
        }
        
        colnames(allocDF)[1] <- allocName
        
        # detailCodeOutputIndex determines whether the summaryCode goes under the Industry or Commodity column in the output file. 
        if(detailCodeOutputIndex == 1){
          industryDF <- data.frame(IndustryCode =I(paste(summaryCodeCw,summaryLoc_Code,sep = "/")))
          commodityDF <- data.frame(CommodityCode = I(rep(paste(sector, summaryLoc_Code, sep = "/"), nrow(industryDF))))
          #commodityDF <- data.frame(paste(currentDetailSectors, summaryLoc_Code, sep = "/")) #adding location to current detail sectors
          #commodityDF <- data.frame(commodityDF[rep(seq_len(nrow(commodityDF)), each = length(summaryCodeCw)),])#replicating the detail commodities a number of times equal to the number of 
          
          
          noteDF <- data.frame(Note = I(rep("IndustryDisagg", nrow(industryDF))))
        }else{
          commodityDF <- data.frame(CommodityCode =I(paste(summaryCodeCw,summaryLoc_Code,sep="/")))
          IndustryDF <- data.frame(IndustryCode = I(rep(paste(sector, summaryLoc_Code, sep = "/"), nrow(industryDF))))
          noteDF <- data.frame(Note = I(rep("CommodityDisagg", nrow(industryDF))))
        }

        currentDF <- cbind(industryDF, commodityDF, allocDF[1], noteDF)
        
        
        temp <-1
        
        outputDF <- rbind(outputDF, currentDF)
        
      }else{
        
        next
      }
      
    }else{
      # Calculate disaggregation values for the rest of the column
      
    }
    

    
  }#end of for sector loop
  
  temp <-2
  return(outputDF)
  
}