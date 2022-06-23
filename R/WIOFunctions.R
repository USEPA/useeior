#' Obtain WIO specs from input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified WIO specs.
getWIOSpecs <- function (model, configpaths = NULL){
  
  model$WIOSpecs <- vector(mode='list')
  
  for (configFile in model$specs$WIOSpecs){
    logging::loginfo(paste0("Loading WIO specification file for ", configFile, "..."))
    config <- getConfiguration(configFile, "WIO", configpaths)
    
    if('WIO' %in% names(config)){
      model$WIOSpecs <- append(model$WIOSpecs, config$WIO)
    }
  }
  
  return(model)
}

#' Setup the WIO specs based on the input files. 
#' This function is essentially a wrapper for disaggregateSetup() function, but included for clarifying the different code flows between the two model types.
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model object with the correct WIO specs.
getWIOFiles <- function (model, configpaths = NULL){ 
  
  model <- disaggregateSetup(model)

  return(model)
}

#' Prepare make and use input files from FlowBySector file.
#' @param fbs FlowBySector dataframe.
#' @return list of two dataframes: UseTableDF and MakeTableDF
prepareWIODFfromFBS <- function(fbs) {
  
  # Temp
  sectorlist <- c('562920')
  
  # Update FBS column names
  old_names <- c('SectorProducedBy', 'SectorConsumedBy', 'FlowAmount', 'Unit', 'MetaSources')
  new_names <- c('IndustryCode', 'CommodityCode', 'Amount', 'Unit', 'Note')
  fbs <- dplyr::rename_with(fbs, ~ new_names,
                            all_of(old_names))
  fbs <- fbs[,(names(fbs) %in% new_names)]
  
  use1 <- fbs[fbs$CommodityCode %in% sectorlist, ]
  use1$WIOSection <- 'Waste Generation Mass'
  use2 <- fbs[fbs$IndustryCode %in% sectorlist, ]
  use2[, c("IndustryCode", "CommodityCode")] <- use2[, c("CommodityCode", "IndustryCode")]
  use2$WIOSection <- 'Waste Treatment Commodities Mass'
  use <- rbind(use1, use2)
  
  make <- fbs[fbs$IndustryCode %in% sectorlist, ]  
  make_agg <- dplyr::group_by(make, IndustryCode, Unit) 
  make_agg <- dplyr::summarize(
    make_agg,
    AmountAgg = sum(Amount),
    Note = dplyr::nth(Note, 1),
    .groups = 'drop'
  )
  colnames(make_agg)[colnames(make_agg)=="AmountAgg"] <- "Amount"
  make_agg$CommodityCode <- make_agg$IndustryCode
  make_agg <- make_agg[,new_names]
  
  x <- list()
  x$UseFileDF <- use
  x$MakeFileDF <- data.frame(make_agg)
  return(x)
}


#' Initialize all the WIO model objects simultaneously. 
#' @param model An EEIO model object with model sepcs and IO tables loaded
#' @return A model object with the WIO lists initialized but empty 
initializeWIOObjects <- function(model){
  
  model$WasteTreatmentIndustries <- data.frame()
  model$WasteTreatmentCommodities <- data.frame()
  model$WasteGenTreat <- data.frame()
  model$WasteGenMass <- data.frame()
  model$RecyclingTreat <- data.frame()
  model$RecyclingnMass <- data.frame()
  
  return(model)
}


#' Build a WIO-style model by including data read from wiospecs folder
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the UseTransactions matrix modified with WIO specs.
assembleWIOModel <- function (model){
  temp <- 1

  for(WIO in model$WIOSpecs){
    
    if(is.null(model$WasteTreatmentIndustries)){
      model <- initializeWIOObjects(model) # Initialize all WIO objects
    }
    
    model <- subsetWIOSectors(model, WIO, "Waste Treatment Industries")
    model <- subsetWIOSectors(model, WIO, "Waste Treatment Commodities")
    model <- subsetWIOSectors(model, WIO, "Waste Generation by Treatment")
    model <- subsetWIOSectors(model, WIO, "Waste Generation by Mass")
    model <- subsetWIOSectors(model, WIO, "Recycling by Treatment")
    model <- subsetWIOSectors(model, WIO, "Recycling by Mass")
 
    model <- includeUseWIO(model, WIO)
    model <- includeMakeWIO(model, WIO)
    temp <- 1.5
    
  }
    

  temp <- 2
  return(model)
}

#' Get subset of sectors corresponding to WasteTreatmentIndustries, WasteTreatmentCommodities, WasteGentTreatment, WasteGenMass, RecyclingTreatment, or RecyclingMass
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param WIO A list with WIO specifications and data
#' @param WIOSection A string that indicates which section of the WIO model the sectors belong to
#' @return A model with the model WIO model objects with the correct subset of sectors and format
subsetWIOSectors <- function (model, WIO, WIOSection){
  

  sectorsSubset <- subset(WIO$NAICSSectorCW, WIO$NAICSSectorCW$Category == WIOSection) # Get rows from WIO$NAICSSectorCW that only have the value "Waste Treatment Industries" under the category column
  colsToRemove <- c("Sector Type", "NAICS_2012_Code")
  sectorsSubset   <- sectorsSubset  [, !colnames(sectorsSubset  ) %in% colsToRemove] # Remove unneeded columns
  colnames(sectorsSubset) <- colnames(model$Commodities) # Rename columns to match model$Industries
  sectorsSubset$Code_Loc <- paste0(sectorsSubset$Code,"/",sectorsSubset$Code_Loc) # Create Code_Loc by combining Code and Location columns
  
   if(WIOSection == "Waste Treatment Industries"){
    model$WasteTreatmentIndustries <- rbind(model$WasteTreatIndustries, sectorsSubset)
    
  }else if(WIOSection == "Waste Treatment Commodities"){
    model$WasteTreatmentCommodities <- rbind(model$WasteTreatmentCommodities, sectorsSubset)
    
  }else if(WIOSection == "Waste Generation by Treatment"){
    model$WasteGenTreat  <- rbind(model$model$WasteGenTreat, sectorsSubset)
    
  }else if(WIOSection == "Waste Generation by Mass"){
    model$WasteGenMass <- rbind(model$WasteGenMass, sectorsSubset)

  }else if(WIOSection == "Recycling by Treatment"){
    model$RecyclingTreat <- rbind(model$RecyclingTreat, sectorsSubset) 

  }else if(WIOSection == "Recycling by Mass"){
    model$RecyclingnMass <- rbind(model$RecyclingnMass, sectorsSubset) 
    
  }else{
    stop("Not a valid sector for a WIO model.")
  }
  
  return(model)
}

#' Include the WIO elements of the Use table in the correct configuration
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param WIO A list with WIO specifications and data
#' @return A model with the UseTransactions matrix modified with WIO specs.
includeUseWIO <- function (model, WIO){
  temp <- 1
  
  
  # Get list of all WIO commodities and industries in a structural sense (i.e., WIO rows and columns for Make/Use)
  WIOUseRows <- do.call("rbind",list(model$WasteTreatmentCommodities, model$WasteGenMass, model$RecyclingnMass))
  WIOUseColumns <- do.call("rbind", list(model$WasteTreatmentIndustries, model$WasteGenTreat, model$RecyclingTreat))
  
  # Find commodity an industry lengths for the expanded use table
  WIOComLength <- dim(model$UseTransactions)[1] + dim(WIOUseRows)[1] # Find the total commodity and industry length of the use table with the WIO elements
  WIOIndLength <- dim(model$UseTransactions)[2] + dim(WIOUseColumns)[1]
  
  fullWIOComList <- rbind(model$Commodities, WIOUseRows) # Create commodity and industry lists of the full WIO Use table
  rownames(fullWIOComList) <- 1:nrow(fullWIOComList)
  
  fullWIOIndList <- rbind(model$Industries, WIOUseColumns[,-(3:5)])
  rownames(fullWIOIndList) <- 1:nrow(fullWIOIndList)

  # Create WIO tables and fill with correct values  
  # For Use Transactions
  WIOUseTransactions <- data.frame(matrix(0, nrow = WIOComLength, ncol = WIOIndLength)) # Create an empty dataframe of the appropriate dimensions
  rownames(WIOUseTransactions) <- fullWIOComList$Code_Loc # Name the rows and columns of the dataframe with the appropriate commodity and industry names respectively
  colnames(WIOUseTransactions) <- fullWIOIndList$Code_Loc
  WIOUseTransactions[1:dim(model$UseTransactions)[1], 1:dim(model$UseTransactions)[2]] <- model$UseTransactions # Populate the IO only portion with the model$UseTransactions values
  
  # For FinalDemand
  WIOFinalDemand <- data.frame(matrix(0, nrow = WIOComLength, ncol = dim(model$FinalDemand)[2]))
  rownames(WIOFinalDemand) <- fullWIOComList$Code_Loc
  colnames(WIOFinalDemand) <- model$FinalDemandMeta$Code_Loc
  WIOFinalDemand[1:dim(model$UseTransactions)[1],] <- model$FinalDemand
  
  # For UseValueAdded
  WIOUseValueAdded <- data.frame(matrix(0, nrow = dim(model$UseValueAdded)[1], ncol = WIOIndLength))
  rownames(WIOUseValueAdded) <- model$ValueAddedMeta$Code_Loc
  colnames(WIOUseValueAdded) <- fullWIOIndList$Code_Loc
  WIOUseValueAdded[,1:dim(model$UseTransactions)[2]] <- model$UseValueAdded
  
  #Split WIO$UseFile into UseTransactions, FinalDemand, and UseValueAdded segments
  useTransactionsDF <-  subset(WIO$UseFileDF, WIO$UseFileDF$CommodityCode %in% fullWIOComList$Code_Loc & WIO$UseFileDF$IndustryCode %in% fullWIOIndList$Code_Loc)
  finalDemandDF <- subset(WIO$UseFileDF, WIO$UseFileDF$CommodityCode %in% fullWIOComList$Code_Loc & WIO$UseFileDF$IndustryCode %in% model$FinalDemandMeta$Code_Loc)
  VADF <- subset(WIO$UseFileDF, WIO$UseFileDF$CommodityCode %in% model$ValueAddedMeta$Code_Loc & WIO$UseFileDF$IndustryCode %in% fullWIOIndList$Code_Loc)
  
  # Assign WIO specific values in the correct location of the WIOfullUse
  # For UseTransactions
  for(r in 1:nrow(useTransactionsDF)){
    comIndex <- which(fullWIOComList$Code_Loc %in% useTransactionsDF[r,]$CommodityCode)
    indIndex <- which(fullWIOIndList$Code_Loc %in% useTransactionsDF[r,]$IndustryCode)

    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOUseTransactions[comIndex, indIndex] <- useTransactionsDF[r,]$Amount
    }
  
    temp <- 3
  }
  
  model$UseTransactions <- WIOUseTransactions
  
  # For FinalDemand
  for(r in 1:nrow(finalDemandDF)){
    comIndex <- which(fullWIOComList$Code_Loc %in% finalDemandDF[r,]$CommodityCode)
    indIndex <- which(fullWIOIndList$Code_Loc %in% finalDemandDF[r,]$IndustryCode)
    
    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOFinalDemand[comIndex, indIndex] <- finalDemandDF[r,]$Amount
    }
    
    temp <- 3
  }
  
  model$FinalDemand <- WIOFinalDemand

  # For UseValueAdded
  for(r in 1:nrow(VADF)){
    comIndex <- which(fullWIOComList$Code_Loc %in% VADF[r,]$CommodityCode)
    indIndex <- which(fullWIOIndList$Code_Loc %in% VADF[r,]$IndustryCode)
    
    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOUseValueAdded[comIndex, indIndex] <- VADF[r,]$Amount
    }
    
    temp <- 3
  }
  
  model$UseValueAdded <- WIOUseValueAdded
  
  temp <- 2
  return(model)
}

#' Include the WIO elements of the Make table in the correct configuration
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the MakeTransactions matrix modified with WIO specs.
includeMakeWIO <- function (model, WIO){
  temp <- 1
  
  # Get list of all WIO commodities and industries in a structural sense (i.e., WIO rows and columns for Make/Use)
  WIOMakeRows <- do.call("rbind", list(model$WasteTreatmentIndustries, model$WasteGenTreat, model$RecyclingTreat))
  WIOMakeColumns <- do.call("rbind",list(model$WasteTreatmentCommodities, model$WasteGenMass, model$RecyclingnMass)) 

  #Create WIO matrices with the correct dimensions to append to use table
  WIOIndLength <- dim(model$MakeTransactions)[1] + dim(WIOMakeRows)[1] # Find the total commodity and industry length of the use table with the WIO elements
  WIOComLength <- dim(model$MakeTransactions)[2] + dim(WIOMakeColumns)[1]
  
  fullWIOIndList <- rbind(model$Industries, WIOMakeRows[,-(3:5)]) # Create commodity and industry lists of the full WIO Make table
  rownames(fullWIOIndList) <- 1:nrow(fullWIOIndList)
  
  fullWIOComList <- rbind(model$Commodities, WIOMakeColumns) 
  rownames(fullWIOComList) <- 1:nrow(fullWIOComList)
  
  # For MakeTransactions
  # Create WIO Make table and fill with correct values
  WIOMakeTransactions <- data.frame(matrix(0, nrow = WIOIndLength, ncol = WIOComLength)) # Create an empty dataframe of the appropriate dimensions
  rownames(WIOMakeTransactions) <- fullWIOIndList$Code_Loc # Name the rows and columns of the dataframe with the appropriate commodity and industry names respectively
  colnames(WIOMakeTransactions) <- fullWIOComList$Code_Loc
  
  WIOMakeTransactions[1:dim(model$MakeTransactions)[1], 1:dim(model$MakeTransactions)[2]] <- model$MakeTransactions # Populate the IO only portion with the model$MakeTransactions values
  
  # Assign WIO specific values in the correct location of the full WIOUseTransactions
  for(r in 1:nrow(WIO$MakeFileDF)){
    comIndex <- which(fullWIOComList$Code_Loc %in% WIO$MakeFileDF[r,]$CommodityCode)
    indIndex <- which(fullWIOIndList$Code_Loc %in% WIO$MakeFileDF[r,]$IndustryCode)
    
    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOMakeTransactions[indIndex, comIndex] <- WIO$MakeFileDF[r,]$Amount
    }
    
    temp <- 3
  }
  
  model$MakeTransactions <- WIOMakeTransactions
  
  temp <- 2
  
  return(model)
}

