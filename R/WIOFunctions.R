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
#' This function is essentially a wrapper for disaggregateSetup() function, but included 
#' for clarifying the different code flows between the two model types.
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model object with the correct WIO specs.
getWIOFiles <- function (model, configpaths = NULL){ 
  
  model <- disaggregateSetup(model, configpaths)

  return(model)
}

#' Prepare make and use input files from FlowBySector file.
#' @param fbs FlowBySector dataframe.
#' @param spec WIO model spec
#' @return list of two dataframes: UseTableDF and MakeTableDF
prepareWIODFfromFBS <- function(fbs, spec, model) {
  sectorlist <- spec$NAICSSectorCW$USEEIO_Code[spec$NAICSSectorCW$Type != "Flowable"]
  year <- fbs$Year[[1]]

# Map Sectors and flows to new WIO codes
lookup <- spec$NAICSSectorCW[c("Type", "Tag", "USEEIO_Code")]
for (col in unique(lookup$Type)){
  fbs[col] <- dplyr::recode(
    fbs[[col]],
    !!!setNames(subset(lookup, Type==col)$USEEIO_Code,
                subset(lookup, Type==col)$Tag))
}
## full dplyr implementation runs into 'promise already under evaluation' error:
# fbs <- dplyr::mutate(
#   fbs,
#   dplyr::across(
#     dplyr::all_of(unique(lookup$Type)),
#     ~ dplyr::recode(.x,
#       !!!setNames(subset(lookup, Type==dplyr::cur_column())$USEEIO_Code,
#                   subset(lookup, Type==dplyr::cur_column())$Tag))))
   
  
  # Consolidate master crosswalk on model level and rename
  NAICStoBEA <- unique(model$crosswalk[, c("NAICS","USEEIO")])
  colnames(NAICStoBEA) <- c("NAICS","BEA")
  # Generate allocation_factor data frame containing allocation factors between NAICS and BEA sectors
  allocation_factor <- getNAICStoBEAAllocation(year, model)
  colnames(allocation_factor) <- c("NAICS", "BEA", "allocation_factor")
  
  for(col in c("SectorProducedBy", "SectorConsumedBy")) {
    colnames(fbs)[colnames(fbs)==col] <- "NAICS"
    # Merge fbs table with NAICStoBEA mapping
    fbs <- merge(fbs, NAICStoBEA, by = "NAICS", all.x = TRUE)
    # Merge the BEA-coded satellite table with allocation_factor dataframe
    fbs <- merge(fbs, allocation_factor, by = c("NAICS", "BEA"), all.x = TRUE)
    # Replace NA in allocation_factor with 1
    fbs[is.na(fbs$allocation_factor), "allocation_factor"] <- 1
    # Where no mapping exists (e.g. new waste sectors), maintain the original sector code
    fbs$BEA <- ifelse(is.na(fbs$BEA), fbs$NAICS, fbs$BEA)
    # Calculate FlowAmount for BEA-coded sectors using allocation factors
    fbs$FlowAmount <- fbs$FlowAmount*fbs$allocation_factor
    colnames(fbs)[colnames(fbs)=="BEA"] <- col
    fbs[, c("NAICS", "allocation_factor")] <- list(NULL)
        
  }

  # Update FBS column names
  old_names <- c('FlowAmount', 'Unit', 'MetaSources')
  new_names <- c('Amount', 'Unit', 'Note')
  cols <- c("CommodityCode", "IndustryCode", new_names, "WIOSection")
  code_cols <- c("IndustryCode", "CommodityCode")
  fbs <- dplyr::rename_with(fbs, ~ new_names,
                            all_of(old_names))
  
  # Separate out use data
  use1 <- fbs[fbs$SectorConsumedBy %in% sectorlist, ]
  use1$WIOSection <- 'Waste Generation Mass'
  use1 <- dplyr::rename_with(use1, ~c('CommodityCode', 'IndustryCode'),
                             all_of(c('Flowable', 'SectorProducedBy')))
  use1$SectorConsumedBy <- NULL
  use2 <- fbs[fbs$SectorProducedBy %in% sectorlist, ]
  use2$WIOSection <- 'Waste Treatment Commodities Mass'  
  use2 <- dplyr::rename_with(use2, ~c('CommodityCode', 'IndustryCode'),
                             all_of(c('Flowable', 'SectorConsumedBy')))
  use2$SectorProducedBy <- NULL
  use <- rbind(use1, use2)
  # Add loc to all sectors
  use[code_cols] <- lapply(use[code_cols], function(x) paste0(x,"/",use$Location))
  use <- use[,(names(use) %in% cols)]
  use <- aggregate(Amount ~ IndustryCode + CommodityCode + Unit + Note + WIOSection,
                   data = use, FUN = sum)
  
  # Separate out make data
  make <- fbs[fbs$SectorConsumedBy %in% sectorlist, ]  
  make_agg <- dplyr::group_by(make, Flowable, SectorConsumedBy, Location, Unit) 
  make_agg <- dplyr::summarize(
    make_agg,
    Amount = sum(Amount),
    Note = dplyr::nth(Note, 1),
    .groups = 'drop'
  )
  make_agg <- dplyr::rename_with(make_agg, ~c('CommodityCode', 'IndustryCode'),
                                 all_of(c('Flowable', 'SectorConsumedBy')))
  make_agg$WIOSection <- 'Waste Generation by Treatment'

  make2 <- fbs[fbs$SectorProducedBy %in% sectorlist, ]
  make_agg2 <- dplyr::group_by(make2, Flowable, SectorProducedBy, Location, Unit) 
  make_agg2 <- dplyr::summarize(
    make_agg2,
    Amount = sum(Amount),
    Note = dplyr::nth(Note, 1),
    .groups = 'drop'
  )
  make_agg2 <- dplyr::rename_with(make_agg2, ~c('CommodityCode', 'IndustryCode'),
                                 all_of(c('Flowable', 'SectorProducedBy')))
  make_agg2$WIOSection <- 'Waste Treatment Commodities'  
  make_agg <- rbind(make_agg, make_agg2)
  make_agg[code_cols] <- lapply(make_agg[code_cols], function(x) paste0(x,"/",make_agg$Location))
  make_agg <- make_agg[,cols]
  
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
  model$RecyclingMass <- data.frame()
  
  return(model)
}


#' Build a WIO-style model by including data read from wiospecs folder
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the UseTransactions matrix modified with WIO specs.
assembleWIOModel <- function (model){
  for(WIO in model$WIOSpecs){
    
    if(is.null(model$WasteTreatmentIndustries)){
      model <- initializeWIOObjects(model) # Initialize all WIO objects
    }
    
    # Create WIO sector lists
    model <- subsetWIOSectors(model, WIO, "Waste Treatment Industries")
    model <- subsetWIOSectors(model, WIO, "Waste Treatment Commodities")
    model <- subsetWIOSectors(model, WIO, "Waste Generation by Treatment")
    model <- subsetWIOSectors(model, WIO, "Waste Generation by Mass")
    model <- subsetWIOSectors(model, WIO, "Recycling by Treatment")
    model <- subsetWIOSectors(model, WIO, "Recycling by Mass")
 
    # Add WIO elements to the useeior model objects
    model <- includeWIOSectorDFs(model, WIO)
    model <- includeFullUseWIO(model, WIO)
    model <- includeMakeWIO(model, WIO)
    model <- calculateWIOOutputs(model, WIO)
    model <- adjustITAwithWIOSectors(model)

    
    checkWIOBalance(model, "Waste")
    checkWIOBalance(model, "Recycling")
    
    temp <- 1.5

  }

  return(model)
}

#' Get subset of sectors corresponding to WasteTreatmentIndustries, WasteTreatmentCommodities,
#' WasteGentTreatment, WasteGenMass, RecyclingTreatment, or RecyclingMass
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param WIO A list with WIO specifications and data
#' @param WIOSection A string that indicates which section of the WIO model the sectors belong to
#' @return A model with the model WIO model objects with the correct subset of sectors and format
subsetWIOSectors <- function (model, WIO, WIOSection){
  
  # Get rows from WIO$NAICSSectorCW that only have the value "Waste Treatment Industries" under the category column
  sectorsSubset <- subset(WIO$NAICSSectorCW, WIO$NAICSSectorCW$Category == WIOSection)
  colsToRemove <- c("Sector Type", "NAICS_2012_Code", "Tag", "Type")
  
  sectorsSubset   <- sectorsSubset [, !colnames(sectorsSubset) %in% colsToRemove] # Remove unneeded columns
  if(nrow(sectorsSubset) == 0){
    return(model)
  }
  colnames(sectorsSubset) <- colnames(model$Commodities) # Rename columns to match model$Industries
  sectorsSubset$Code_Loc <- paste0(sectorsSubset$Code,"/",sectorsSubset$Code_Loc)
  
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
#' @return A model object which contain the model$Commodity or model$Industry objects with WIO sectors
includeWIOSectorDFs <- function (model, WIO){
  # Get list of all WIO commodities and industries in a structural sense (i.e., WIO rows and columns for Make/Use)
  WIOUseRows <- do.call("rbind",list(model$WasteTreatmentCommodities, model$WasteGenMass, model$RecyclingnMass))
  WIOUseColumns <- do.call("rbind", list(model$WasteTreatmentIndustries, model$WasteGenTreat, model$RecyclingTreat))
  
  fullWIOComList <- rbind(model$Commodities, WIOUseRows) # Create commodity and industry lists of the full WIO Use table
  rownames(fullWIOComList) <- 1:nrow(fullWIOComList)
  
  fullWIOIndList <- rbind(model$Industries, WIOUseColumns[,-(3:5)])
  rownames(fullWIOIndList) <- 1:nrow(fullWIOIndList)
  
  model$Commodities <- fullWIOComList
  model$Industries <- fullWIOIndList
  
  return(model)
  
}

#' Include the WIO elements of the Use table in the correct configuration
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param WIO A list with WIO specifications and data
#' @return A model with the UseTransactions matrix modified with WIO specs.
includeFullUseWIO <- function (model, WIO){
  # Find commodity an industry lengths for the expanded use table
  WIOComLength <- dim(model$Commodities)[1]
  WIOIndLength <- dim(model$Industries)[1] 
  
  # Create WIO tables and fill with correct values  
  # For Use Transactions
  # Create an empty dataframe of the appropriate dimensions
  WIOUseTransactions <- data.frame(matrix(0, nrow = WIOComLength, ncol = WIOIndLength)) 
  # Name the rows and columns of the dataframe with the appropriate commodity and industry names respectively
  rownames(WIOUseTransactions) <- model$Commodities$Code_Loc
  colnames(WIOUseTransactions) <- model$Industries$Code_Loc
  # Populate the IO only portion with the model$UseTransactions values
  WIOUseTransactions[1:dim(model$UseTransactions)[1], 1:dim(model$UseTransactions)[2]] <- model$UseTransactions
  
  # For DomesticUseTransactions
  # Create an empty dataframe of the appropriate dimensions
  WIODomesticUseTransactions <- data.frame(matrix(0, nrow = WIOComLength, ncol = WIOIndLength))
  # Name the rows and columns of the dataframe with the appropriate commodity and industry names respectively
  rownames(WIODomesticUseTransactions) <- model$Commodities$Code_Loc
  colnames(WIODomesticUseTransactions) <- model$Industries$Code_Loc
  # Populate the IO only portion with the model$UseTransactions values
  WIODomesticUseTransactions[1:dim(model$DomesticUseTransactions)[1],
                             1:dim(model$DomesticUseTransactions)[2]] <- model$DomesticUseTransactions
  
  # For FinalDemand
  WIOFinalDemand <- data.frame(matrix(0, nrow = WIOComLength, ncol = dim(model$FinalDemand)[2]))
  rownames(WIOFinalDemand) <- model$Commodities$Code_Loc
  colnames(WIOFinalDemand) <- model$FinalDemandMeta$Code_Loc
  WIOFinalDemand[1:dim(model$UseTransactions)[1],] <- model$FinalDemand
  
  # For FinalDemand
  WIODomesticFinalDemand <- data.frame(matrix(0, nrow = WIOComLength, ncol = dim(model$DomesticFinalDemand)[2]))
  rownames(WIODomesticFinalDemand) <- model$Commodities$Code_Loc
  colnames(WIODomesticFinalDemand) <- model$FinalDemandMeta$Code_Loc
  WIODomesticFinalDemand[1:dim(model$UseTransactions)[1],] <- model$DomesticFinalDemand
  
  # For UseValueAdded
  WIOUseValueAdded <- data.frame(matrix(0, nrow = dim(model$UseValueAdded)[1], ncol = WIOIndLength))
  rownames(WIOUseValueAdded) <- model$ValueAddedMeta$Code_Loc
  colnames(WIOUseValueAdded) <- model$Industries$Code_Loc
  WIOUseValueAdded[,1:dim(model$UseTransactions)[2]] <- model$UseValueAdded
  
  #Split WIO$UseFile into UseTransactions, FinalDemand, and UseValueAdded segments
  useTransactionsDF <-  subset(WIO$UseFileDF, (WIO$UseFileDF$CommodityCode %in% model$Commodities$Code_Loc) 
                               & (WIO$UseFileDF$IndustryCode %in% model$Industries$Code_Loc))
  finalDemandDF <- subset(WIO$UseFileDF, (WIO$UseFileDF$CommodityCode %in% model$Commodities$Code_Loc) & 
                            (WIO$UseFileDF$IndustryCode %in% model$FinalDemandMeta$Code_Loc))
  VADF <- subset(WIO$UseFileDF, (WIO$UseFileDF$CommodityCode %in% model$ValueAddedMeta$Code_Loc) & 
                   (WIO$UseFileDF$IndustryCode %in% model$Industries$Code_Loc))
  
  # Assign WIO specific values in the correct location of the WIOfullUse
  # For UseTransactions and DomesticUseTransactions
  for(r in 1:nrow(useTransactionsDF)){
    comIndex <- which(model$Commodities$Code_Loc %in% useTransactionsDF[r,]$CommodityCode)
    indIndex <- which(model$Industries$Code_Loc %in% useTransactionsDF[r,]$IndustryCode)

    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOUseTransactions[comIndex, indIndex] <- useTransactionsDF[r,]$Amount
      WIODomesticUseTransactions[comIndex, indIndex] <- useTransactionsDF[r,]$Amount
    }
  }
  
  model$UseTransactions <- WIOUseTransactions
  model$DomesticUseTransactions <- WIODomesticUseTransactions

  # For FinalDemand and DomesticFinalDemand
  for(r in 1:nrow(finalDemandDF)){
    comIndex <- which(model$Commodities$Code_Loc %in% finalDemandDF[r,]$CommodityCode)
    indIndex <- which(model$FinalDemandMeta$Code_Loc %in% finalDemandDF[r,]$IndustryCode)
    
    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOFinalDemand[comIndex, indIndex] <- finalDemandDF[r,]$Amount
      WIODomesticFinalDemand[comIndex, indIndex] <- finalDemandDF[r,]$Amount
    }
  }
  
  model$FinalDemand <- WIOFinalDemand
  model$DomesticFinalDemand <- WIODomesticFinalDemand

  # For UseValueAdded
  for(r in 1:nrow(VADF)){
    comIndex <- which(model$ValueAddedMeta$Code_Loc %in% VADF[r,]$CommodityCode)
    indIndex <- which(model$Industries$Code_Loc %in% VADF[r,]$IndustryCode)
    
    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOUseValueAdded[comIndex, indIndex] <- VADF[r,]$Amount
    }
  }
  
  model$UseValueAdded <- WIOUseValueAdded
  return(model)
}

#' Include the WIO elements of the Make table in the correct configuration
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model with the MakeTransactions matrix modified with WIO specs.
includeMakeWIO <- function (model, WIO){
  
  #Create WIO matrices with the correct dimensions to append to use table
  WIOComLength <- dim(model$Commodities)[1]
  WIOIndLength <- dim(model$Industries)[1] 
  
  # For MakeTransactions
  # Create WIO Make table and fill with correct values
  # Create an empty dataframe of the appropriate dimensions
  WIOMakeTransactions <- data.frame(matrix(0, nrow = WIOIndLength, ncol = WIOComLength))
  # Name the rows and columns of the dataframe with the appropriate commodity and industry names respectively
  rownames(WIOMakeTransactions) <- model$Industries$Code_Loc
  colnames(WIOMakeTransactions) <- model$Commodities$Code_Loc
  
  # Populate the IO only portion with the model$MakeTransactions values
  WIOMakeTransactions[1:dim(model$MakeTransactions)[1], 1:dim(model$MakeTransactions)[2]] <- model$MakeTransactions
  
  # Assign WIO specific values in the correct location of the full WIOUseTransactions
  for(r in 1:nrow(WIO$MakeFileDF)){
    comIndex <- which(model$Commodities$Code_Loc %in% WIO$MakeFileDF[r,]$CommodityCode)
    indIndex <- which(model$Industries$Code_Loc %in% WIO$MakeFileDF[r,]$IndustryCode)
    
    if(length(comIndex)!=0 & length(indIndex) !=0){
      WIOMakeTransactions[indIndex, comIndex] <- WIO$MakeFileDF[r,]$Amount
    }
  }
  
  model$MakeTransactions <- WIOMakeTransactions
  
  return(model)
}


#' Calculate the total industry and commodity outputs for the expanded WIO tables
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param WIO A list with WIO specifications and data
#' @return A model object which contain the model$Commodity or model$Industry objects with WIO sectors
calculateWIOOutputs<- function (model, WIO){
  # The outputs need to be calculated using the row sums for each table to avoid mixing units

  model$IndustryOutput <- rowSums(model$MakeTransactions)
  model$CommodityOutput <- rowSums(model$UseTransactions)+rowSums(model$FinalDemand)
  
  return(model)
}

#' Adjust InternationalTradeAdjustment to the correct dimensions based on WIO sectors
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A model object with a modified international trade adjustment object based on WIO sectors
adjustITAwithWIOSectors <- function (model){
  # Get list of all WIO commodities and industries in a structural sense (i.e., WIO rows and columns for Make/Use)
  WIOUseRows <- do.call("rbind",list(model$WasteTreatmentCommodities, model$WasteGenMass, model$RecyclingnMass))
  
  WIOITA <- double(dim(WIOUseRows)[1])
  names(WIOITA) <- WIOUseRows$Code_Loc
  model$InternationalTradeAdjustment <- append(model$InternationalTradeAdjustment, WIOITA)
  
  
  return(model)
}

#' Check balance of WIO waste and recycling sectors
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param sectorType A string that indicates to compare the balance of waste or recycling generation and treatment sectors.
#' @return A model with the UseTransactions matrix modified with WIO specs.
checkWIOBalance <- function (model, sectorType = "Waste"){
  # Get all waste generation and recycling by mass and treatment from use table
  if(sectorType == "Waste"){
    useGen <- model$UseTransactions[which(rownames(model$UseTransactions) %in% model$WasteGenMass$Code_Loc),]
    makeTreatment <- model$MakeTransactions[which(rownames(model$MakeTransactions) %in% model$WasteGenTreat$Code_Loc),]
  }else{
    useGen <- model$UseTransactions[which(rownames(model$UseTransactions) %in% model$RecyclingMass$Code_Loc),]
    makeTreatment <- model$MakeTransactions[which(rownames(model$MakeTransactions) %in% model$RecyclingTreat$Code_Loc),]
  }

  if(dim(useGen)[1] != 0 & dim(makeTreatment)[1] != 0){

    
    genSum <- sum(useGen)
    treatSum <- sum(makeTreatment)
    
    # if the ratio of the sum of generation over the sum of treatment is greater than 1% then the generation and treatment is not balanced and we need to stop execution
    if(abs(1 - genSum/treatSum) > 0.01){
      stop(paste0(sectorType, " not balanced"))
    }else{
      logging::loginfo(paste0(sectorType, " adequately balanced within 1%."))
    }
  } else if(dim(useGen)[1] != 0 | dim(makeTreatment)[1] != 0) { # for the case where some, but not all, WIO sectors are properly defined 
    stop(paste0(sectorType, " sector(s) missing from table."))
  }
  
  
}
