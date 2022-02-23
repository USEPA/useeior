#' Disaggregate a model based on specified source file
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return A disaggregated model.
disaggregateModel <- function (model){
 
  logging::loginfo("Initializing Disaggregation of IO tables...")
  
  for (disagg in model$DisaggregationSpecs){
    
    #Disaggregating sector lists 
    model$Commodities <- disaggregateSectorDFs(model, disagg, "Commodity")
    model$Industries <- disaggregateSectorDFs(model, disagg, "Industry")

    #Disaggregating main model components
    model$UseTransactions <- disaggregateUseTable(model, disagg)
    model$MakeTransactions <- disaggregateMakeTable(model, disagg)
    model$UseValueAdded <- disaggregateVA(model, disagg)
    model$DomesticUseTransactions <- disaggregateUseTable(model, disagg, domestic = TRUE)
    
    if(model$specs$CommodityorIndustryType=="Commodity") {
      model$FinalDemand <- disaggregateFinalDemand(model, disagg, domestic = FALSE)
      model$DomesticFinalDemand <- disaggregateFinalDemand(model, disagg, domestic = TRUE)
    } else {
      model$FinalDemandbyCommodity <- disaggregateFinalDemand(model, disagg, domestic = FALSE)
      model$DomesticFinalDemandbyCommodity <- disaggregateFinalDemand(model, disagg, domestic = TRUE)
      model$InternationalTradeAdjustmentbyCommodity <- disaggregateInternationalTradeAdjustment(model, disagg, NULL, adjustmentByCommodity = TRUE)
    }
    
    #Balancing model
    if(disagg$DisaggregationType == "Userdefined"){
      model <- balanceDisagg(model, disagg)
    }

    #Recalculate model$CommodityOutput and model$IndustryOutput objects. This if else has to be separate from the one above because 
    #the calculateIndustryCommodityOutput function is used prior to the creation of model$FinalDemandbyCommodity object, 
    #and we can't recalculate the commodity and industry totals before balancing. 
    if(model$specs$CommodityorIndustryType=="Commodity") {
      model <- calculateIndustryCommodityOutput(model)
      
    } else{
      model$IndustryOutput <- colSums(model$UseTransactions) + colSums(model$UseValueAdded)
      model$CommodityOutput <- rowSums(model$UseTransactions) + rowSums(model$FinalDemandbyCommodity)
    }
    
    #Disaggregating MultiyearIndustryOutput and MultiYearCommodityOutput 
    model$MultiYearCommodityOutput <- disaggregateMultiYearOutput(model, disagg, output_type = "Commodity")
    model$MultiYearIndustryOutput <- disaggregateMultiYearOutput(model, disagg, output_type = "Industry")

    #Disaggregating CPI model objects. Assumption is that the disaggregated sectors have the same CPI values as the original sector. 
    model$MultiYearCommodityCPI <- disaggregateCols(model$MultiYearCommodityCPI, disagg, duplicate = TRUE)
    model$MultiYearIndustryCPI <- disaggregateCols(model$MultiYearIndustryCPI, disagg, duplicate = TRUE)

    #Disaggregating Crosswalk
    model$crosswalk <- disaggregateMasterCrosswalk(model, disagg)
    
    #Disaggregate Margins
    model$Margins <- disaggregateMargins(model, disagg)
    model$InternationalTradeAdjustment <- disaggregateInternationalTradeAdjustment(model, disagg)
    
    # Transform model FinalDemand, DomesticFinalDemand, and InternationalTradeAdjustment to by-industry form
    if (model$specs$CommodityorIndustryType=="Industry") {
      # Keep the orignal FinalDemand (in by-commodity form)
      model$FinalDemand <- transformFinalDemandwithMarketShares(model$FinalDemandbyCommodity, model)
      model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(model$DomesticFinalDemandbyCommodity, model)
      model$InternationalTradeAdjustment <- unlist(transformFinalDemandwithMarketShares(model$InternationalTradeAdjustmentbyCommodity, model))
    }
  }
  
  return(model)
  
}

#' Obtain aggregation and disaggregation specs from input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model with the specified aggregation and disaggregation specs.
getDisaggregationSpecs <- function (model, configpaths = NULL){

  model$DisaggregationSpecs <- vector(mode='list')
  for (configFile in model$specs$DisaggregationSpecs){
    logging::loginfo(paste0("Loading disaggregation specification file for ", configFile, "..."))
    config <- getConfiguration(configFile, "disagg", configpaths)

    if('Disaggregation' %in% names(config)){
      model$DisaggregationSpecs <- append(model$DisaggregationSpecs, config$Disaggregation)
    }
  }
  
  model <- disaggregateSetup(model, configpaths)
  
  return(model)
}

#' Setup the configuration specs based on the input files
#' @param model An EEIO model object with model specs and IO tables loaded
#' @param configpaths str vector, paths (including file name) of disagg configuration file(s).
#' If NULL, built-in config files are used.
#' @return A model object with the correct disaggregation specs.
disaggregateSetup <- function (model, configpaths = NULL){
  
  for (disagg in model$DisaggregationSpecs){  
    filename <- ifelse(is.null(configpaths),
                       system.file("extdata/disaggspecs", disagg$SectorFile, package = "useeior"),
                       file.path(dirname(configpaths)[1], disagg$SectorFile))
    disagg$NAICSSectorCW <- utils::read.table(filename,
                                              sep = ",", header = TRUE,
                                              stringsAsFactors = FALSE,
                                              check.names = FALSE)
    
    newNames <- unique(data.frame("SectorCode" = disagg$NAICSSectorCW$USEEIO_Code,
                                  "SectorName" = disagg$NAICSSectorCW$USEEIO_Name,
                                  "Category" = disagg$NAICSSectorCW$Category,
                                  "Subcategory" = disagg$NAICSSectorCW$Subcategory,
                                  "Description" = disagg$NAICSSectorCW$Description,
                                  stringsAsFactors = TRUE))
    disagg$DisaggregatedSectorNames <- as.list(levels(newNames[, 'SectorName']))
    disagg$DisaggregatedSectorCodes <- as.list(levels(newNames[, 'SectorCode']))
    disagg$Category <- lapply(newNames[, 'Category'], as.character)
    disagg$Subcategory <- lapply(newNames[, 'Subcategory'], as.character)
    disagg$Description <- lapply(newNames[, 'Description'], as.character)
    
    #reordering disaggSectorNames and DisaggSectorCodes to match the mapping in newNames
    disagg$DisaggregatedSectorNames <- as.list(disagg$DisaggregatedSectorNames[match(newNames$SectorName,disagg$DisaggregatedSectorNames)])
    disagg$DisaggregatedSectorCodes <- as.list(disagg$DisaggregatedSectorCodes[match(newNames$SectorCode,disagg$DisaggregatedSectorCodes)])
    
    # Load Make table disaggregation file
    if(!is.null(disagg$MakeFile)){
      filename <- ifelse(is.null(configpaths),
                         system.file("extdata/disaggspecs", disagg$MakeFile, package = "useeior"),
                         file.path(dirname(configpaths)[1], disagg$MakeFile))
      disagg$MakeFileDF <- utils::read.table(filename,
                                             sep = ",", header = TRUE,
                                             stringsAsFactors = FALSE,
                                             check.names = FALSE)
    }
    
    # Load Use table disaggregation file
    if(!is.null(disagg$UseFile)){
      filename <- ifelse(is.null(configpaths),
                         system.file("extdata/disaggspecs", disagg$UseFile, package = "useeior"),
                         file.path(dirname(configpaths)[1], disagg$UseFile))
      disagg$UseFileDF <- utils::read.table(filename,
                                            sep = ",", header = TRUE,
                                            stringsAsFactors = FALSE,
                                            check.names = FALSE)
    }
    
    # Load Environment flows table
    if(!is.null(disagg$EnvFile)){
      filename <- ifelse(is.null(configpaths),
                         system.file("extdata/disaggspecs", disagg$EnvFile, package = "useeior"),
                         file.path(dirname(configpaths)[1], disagg$EnvFile))
      disagg$EnvFileDF <- utils::read.table(filename,
                                            sep = ",", header = TRUE,
                                            stringsAsFactors = FALSE,
                                            check.names = FALSE)
    }
    
    if("FlowRatio" %in% colnames(disagg$EnvFileDF)) {
      disagg$EnvAllocRatio <- TRUE
    } else {
      disagg$EnvAllocRatio <- FALSE
    }

    # For Two-region model, develop two-region specs from national disaggregation files
    if (model$specs$IODataSource=="stateior" & stringr::str_sub(disagg$OriginalSectorCode, start=-3)=="/US") {
      for(region in model$specs$ModelRegionAcronyms){
        d2 <- prepareTwoRegionDisaggregation(disagg, region, model$specs$ModelRegionAcronyms)
        model$DisaggregationSpecs[[d2$OriginalSectorCode]] <- d2
      }
      # Remove original disaggregation spec
      model$DisaggregationSpecs[disagg$OriginalSectorCode] <- NULL

    } else {
      #Need to assign these DFs back to the modelspecs
      model$DisaggregationSpecs[[disagg$OriginalSectorCode]] <- disagg
    }
  }
  
  return(model)
}


#' Generate two-region disaggregation specs from a national spec
#' @param disagg Specifications for disaggregating the current Table
#' @param region Str, Location code for target disaggregation specs
#' @param regions list of location codes from ModelRegionAcronyms
#' @return modified disagg specs for target region
prepareTwoRegionDisaggregation <- function(disagg, region, regions) {

  d2 <- disagg
  OriginalSector <- gsub("/US", "", disagg$OriginalSectorCode)
  d2$OriginalSectorCode <- paste0(OriginalSector, "/", region)
  other_region <- regions[regions != region]
  
  # Update NAICSSectorCW
  d2$NAICSSectorCW$USEEIO_Code <- gsub("/US", paste0("/",region), d2$NAICSSectorCW$USEEIO_Code)
  d2$DisaggregatedSectorCodes <- lapply(d2$DisaggregatedSectorCodes, function(x) gsub("/US", paste0("/",region), x))

  # Duplicate national allocations
  cols <- c("IndustryCode","CommodityCode")
  d2$MakeFileDF[cols] <- lapply(d2$MakeFileDF[cols], function(x) gsub("/US", paste0("/",region), x))
  d2$UseFileDF[cols] <- lapply(d2$UseFileDF[cols], function(x) gsub("/US", paste0("/",region), x))  

  # For Use table, adjust use table intersections for sequential disaggregation
  rep <- subset(d2$UseFileDF, CommodityCode %in% d2$DisaggregatedSectorCodes &
                              IndustryCode %in% d2$DisaggregatedSectorCodes)
  
  rep1 <- rep
  rep2 <- rep
  
  # For the first pass (region 1), consolidate on the original sector code (not yet disaggregated)
  if(region == regions[1]) {
    rep1["CommodityCode"] <- paste0(OriginalSector, "/", other_region)
    rep1 <- aggregate(PercentUsed ~ IndustryCode + CommodityCode, rep1, sum)

    rep2["IndustryCode"] <- paste0(OriginalSector, "/", other_region)
    rep2 <- aggregate(PercentUsed ~ IndustryCode + CommodityCode, rep2, sum)
    
    # Invert columns for sequential disaggregation
    rep1[cols] <- rep1[rev(cols)]
    rep2[cols] <- rep2[rev(cols)]
    
    # Add back blank 'Note' column
    rep <- rbind(rep1, rep2)
    rep['Note'] <- NA

  } else {
  # On the second pass (region 2), apply to disaggregated sectors

    # Renormalize intersection columns
    total <- list()
    total[c("CommodityCode", "Total")] <- aggregate(PercentUsed ~ CommodityCode,
                                                    rep1, sum)
    rep1 <- merge(rep1, total)
    rep1["PercentUsed"] = rep1["PercentUsed"]/rep1["Total"]
    rep1["Total"] <- NULL
    rep1[cols] <- rep1[rev(cols)]
    rep1["IndustryCode"] <- lapply(rep1["IndustryCode"], function(x) gsub(paste0("/", region), 
                                                                          paste0("/", other_region), x))

    # Renormalize intersection columns
    total <- list()
    total[c("IndustryCode", "Total")] <- aggregate(PercentUsed ~ IndustryCode,
                                                    rep2, sum)
    rep2 <- merge(rep2, total)
    rep2["PercentUsed"] = rep2["PercentUsed"]/rep2["Total"]
    rep2["Total"] <- NULL
    rep2[cols] <- rep2[rev(cols)]

    rep2["CommodityCode"] <- lapply(rep2["CommodityCode"], function(x) gsub(paste0("/", region), 
                                                                            paste0("/", other_region), x))
    rep <- rbind(rep1, rep2)    
  }
  d2$UseFileDF <- rbind(d2$UseFileDF, rep)
  rownames(d2$UseFileDF) <- NULL

  ## Disaggregate Satellite Table
  ## TODO

  
  return(d2)
}


#' Disaggregate model$InternationalTradeAdjustments vector in the main model object
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param ratios Specific ratios to be used for the disaggregation of the InternationalTradeAdjusment object in place of using economic totals to derive the ratios.
#' @param adjustmentByCommodity Flag to denote whether to disaggregate the InternationalTradeAdjustmentbyCommodity object which is only present in industry models
#' @return newInternationalTradeAdjustment A vector which contains the InternationalTradeAdjustment for the disaggregated sectors
disaggregateInternationalTradeAdjustment <- function(model, disagg, ratios = NULL, adjustmentByCommodity = FALSE){
  if (adjustmentByCommodity == FALSE) {
    originalInternationalTradeAdjustment <- model$InternationalTradeAdjustment
  } else {
    originalInternationalTradeAdjustment <- model$InternationalTradeAdjustmentbyCommodity
  }
  originalNameList <- names(originalInternationalTradeAdjustment) # Get names from named vector
#  codeLength <- nchar(gsub("/.*", "", disagg$OriginalSectorCode)) # Calculate code length (needed for summary vs. detail level code lengths)
#  originalIndex <- which(originalNameList == substr(disagg$OriginalSectorCode, 1, codeLength)) # Get row index of the original aggregate sector in the object
  originalIndex <- which(originalNameList == disagg$OriginalSectorCode) # Get row index of the original aggregate sector in the object
  
  originalRow <- originalInternationalTradeAdjustment[originalIndex] # Copy row containing the Margins information for the original aggregate sector
  disaggInternationalTradeAdjustment <- rep(originalRow,length(disagg$DisaggregatedSectorCodes)) # Replicate the original a number of times equal to the number of disaggregate sectors
 
  if(is.null(ratios)){# Use default ratios, i.e., commodity output ratios
    disaggRatios <- unname(disaggregatedRatios(model, disagg, "Commodity"))#ratios needed to calculate the margins for the disaggregated sectors. Need to unname for compatibility with Rho matrix later in the model build process.
  }else{
    #todo: need to discuss if other ratios are relevant/needed and where they would come from.
    disaggRatios <- ratios
  }
  
  disaggInternationalTradeAdjustment <- disaggInternationalTradeAdjustment * disaggRatios
  
  # Rename the rows of the vector
  disaggRowNames <- unlist(disagg$DisaggregatedSectorCodes)
  disaggRowNames <- sapply(strsplit(disaggRowNames, split = "/"), "[",1)
  names(disaggInternationalTradeAdjustment) <- disaggRowNames
  
  # Combine elements in a new vector
  part1 <- originalInternationalTradeAdjustment[1:(originalIndex-1)]
  part3 <- originalInternationalTradeAdjustment[(originalIndex+1):length(originalInternationalTradeAdjustment)]
  
  newITA <- c(part1, disaggInternationalTradeAdjustment, part3)
 
  return(newITA)

}

#' Disaggregate model$Margins dataframe in the main model object
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return newMargins A dataframe which contain the margins for the disaggregated sectors
disaggregateMargins <- function(model, disagg) {
  originalMargins <- model$Margins
  originalIndex <-  grep(disagg$OriginalSectorCode, model$Margins$Code_Loc)#get row index of the original aggregate sector in the model$Margins object
  originalRow <- model$Margins[originalIndex,]#copy row containing the Margins information for the original aggregate sector
  disaggMargins <-originalRow[rep(seq_len(nrow(originalRow)), length(disagg$DisaggregatedSectorCodes)),,drop=FALSE]#replicate the original a number of times equal to the number of disaggregate sectors
  disaggRatios <- unname(disaggregatedRatios(model, disagg, "Commodity"))#ratios needed to calculate the margins for the disaggregated sectors. Need to unname for compatibility with Rho matrix later in the model build process.
  
  #variable to determine length of Code substring, i.e., code length minus geographic identifier and separator character (e.g. "/US")
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
disaggregatedRatios <- function(model, disagg, output_type = "Commodity") {

  if(output_type == "Industry") {
    #Get Index for Disaggregated Industries in the use table
    disaggUseStartIndex <- which(colnames(model$UseTransactions)==disagg$DisaggregatedSectorCodes[1])
    disaggUseEndIndex <- disaggUseStartIndex+length(disagg$DisaggregatedSectorCodes)-1
    
    #calculate industry ratios after disaggregation from Use table
    disaggRatios <- colSums(model$UseTransactions[,disaggUseStartIndex:disaggUseEndIndex]) + colSums(model$UseValueAdded[,disaggUseStartIndex:disaggUseEndIndex])
    disaggRatios <- disaggRatios / sum(disaggRatios)

  } else { 
    #assume commodity if industry is not specified
    #Get Index for Disaggregated Commodities in the use table
    disaggUseStartIndex <- which(rownames(model$UseTransactions)==disagg$DisaggregatedSectorCodes[1])
    disaggUseEndIndex <- disaggUseStartIndex+length(disagg$DisaggregatedSectorCodes)-1
    
    #calculate industry ratios after disaggregation from Use table
#    disaggRatios <- rowSums(model$UseTransactions[disaggUseStartIndex:disaggUseEndIndex,]) + rowSums(model$FinalDemand[disaggUseStartIndex:disaggUseEndIndex,])
    if(model$specs$CommodityorIndustryType == "Industry"){
      disaggRatios <- rowSums(model$UseTransactions[disaggUseStartIndex:disaggUseEndIndex,]) + rowSums(model$FinalDemandbyCommodity[disaggUseStartIndex:disaggUseEndIndex,])
      
    }else{
      disaggRatios <- rowSums(model$UseTransactions[disaggUseStartIndex:disaggUseEndIndex,]) + rowSums(model$FinalDemand[disaggUseStartIndex:disaggUseEndIndex,])
      
    }
    disaggRatios <- disaggRatios / sum(disaggRatios) 
    
  }
  
  return(disaggRatios)
}

#' Disaggregate MultiYear Output model objects
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param output_type A string that indicates whether the Commodity or Industry output should be disaggregated
#' @return model A dataframe with the disaggregated GDPGrossOutputIO by year
disaggregateMultiYearOutput <- function(model, disagg, output_type = "Commodity") {
 
  if(output_type == "Industry") {
    originalOutput = model$MultiYearIndustryOutput
  } else {
    #assume commodity if industry is not specified
    originalOutput = model$MultiYearCommodityOutput
  }

  disaggRatios <- disaggregatedRatios(model, disagg, output_type)
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
disaggregateSectorDFs <- function(model, disagg, list_type) {

  if(list_type == "Commodity") {
    originalList <- model$Commodities
  } else {
    #assume industry if not specified
    originalList <- model$Industries
  }
  originalIndex <- grep(disagg$OriginalSectorCode, originalList$Code_Loc)
  newSectors <- data.frame(matrix(ncol = ncol(originalList), nrow = length(disagg$DisaggregatedSectorCodes)))
  names(newSectors) <- names(originalList) #rename columns for the df

  if(list_type == "Commodity") {  
    newSectors$Category <- sapply(disagg$Category, paste0, collapse = "")
    newSectors$Subcategory <- sapply(disagg$Subcategory, paste0, collapse = "")
    newSectors$Description <- sapply(disagg$Description, paste0, collapse = "")
  }

  #variable to determine length of Code substring, i.e., code length minus geographic identifier and separator character (e.g. "/US")
  codeLength <- nchar(gsub("/.*", "", disagg$DisaggregatedSectorCodes[1]))
  newSectors$Code <- substr(disagg$DisaggregatedSectorCodes,1,codeLength)
  newSectors$Code_Loc <- sapply(disagg$DisaggregatedSectorCodes, paste0, collapse = "")#sapply needed to convert DisaggregatedSectorCodes from list to char vector
  newSectors$Name <- sapply(disagg$DisaggregatedSectorNames, paste0, collapse = "")
  newSectors <- rbind(originalList[1:originalIndex-1,],newSectors,originalList[-(1:originalIndex),])
  rownames(newSectors) <- 1:nrow(newSectors)
  
  return(newSectors)
}

#' Disaggregate a portion of a satellite table based on an allocation_vector
#' @param sattable A standardized satellite table to be disaggregated.
#' @param disagg Specifications for disaggregating the current Table
#' @param allocating_sectors vector of sectors to allocate to
#' @param allocation_vector named vector of allocation ratios
#' @return A satellite table with new sectors added.
disaggregateSatelliteSubsetByRatio <- function(sattable, disagg, allocating_sectors, allocation_vector = NULL) {
  
  if(is.null(allocation_vector) & !is.null(disagg$MakeFileDF)) {
    GrossOutputAlloc <- subset(disagg$MakeFileDF, IndustryCode == disagg$OriginalSectorCode)
    allocation_vector <- setNames(GrossOutputAlloc$PercentMake, gsub("/.*", "", GrossOutputAlloc$CommodityCode))
    allocation_vector <- allocation_vector[!duplicated(allocation_vector)]
  } else if(is.null(allocation_vector)) {
    allocation_vector <- setNames(rep(1/length(allocating_sectors),
                                      times = length(allocating_sectors)),
                                  gsub("/.*", "", allocating_sectors))
  }
  
  # only maintain the appropriate sectors in the allocation vector
  allocation_vector <- subset(allocation_vector, names(allocation_vector) %in% gsub("/.*", "", allocating_sectors))
  allocation_vector <- sapply(allocation_vector, function(x){x / sum(allocation_vector)})
  
  
  sattable_subset_disaggregated <- sattable
  i<-1
  for (new_sector in gsub("/.*", "", allocating_sectors)){
    new_sector_totals <- sattable
    # Update the sector and sector name
    new_sector_totals$Sector <- new_sector
    new_sector_totals$SectorName <- disagg$DisaggregatedSectorNames[[match(new_sector, gsub("/.*", "", disagg$DisaggregatedSectorCode))]]
    allocation <- 0
    if (new_sector %in% names(allocation_vector)){
      allocation <- allocation_vector[[new_sector]]
    }
    new_sector_totals$FlowAmount <- new_sector_totals$FlowAmount * allocation
    # Modify other metadata or DQI?

    # Append to the original satellite subset
    sattable_subset_disaggregated <- rbind(sattable_subset_disaggregated,new_sector_totals)
    i <- i+1
  }
  return(sattable_subset_disaggregated)
}


#' Disaggregate satellite tables from static file based on specs
#' @param disagg Specifications for disaggregating the current Table
#' @param tbs A standardized satellite table with resource and emission names from original sources.
#' @param sat_spec, a standard specification for a single satellite table.
#' @return A standardized satellite table with old sectors removed and new sectors added.
disaggregateSatelliteTable <- function (disagg, tbs, sat_spec) {
  sattable <- tbs
  # identify NAICS that require further disaggregation
  naics <- disagg$NAICSSectorCW[c('NAICS_2012_Code','USEEIO_Code')]
  codes <- unique(naics[duplicated(naics$NAICS_2012_Code),]$NAICS_2012_Code)
  naics <- naics[which(naics$NAICS_2012_Code %in% codes),]
  
  original_code <- gsub("/.*", "", disagg$OriginalSectorCode)
  codes <- c(original_code, codes)
  allocating_sectors <- disagg$DisaggregatedSectorCodes
  if(any(codes %in% sattable$Sector)) {
    if(!is.null(disagg$EnvFileDF) & disagg$EnvAllocRatio) {
      # If satellite table data is provided as flow by sector ratios, loop through each flow assigned to original sector
      sattable_to_disaggregate = subset(sattable, Sector %in% codes)
      # Check if allocating to full sector list from original code or just a subset based on duplicate NAICS
      if(!(original_code %in% sattable_to_disaggregate$Sector)){
        allocating_sectors <- naics$USEEIO_Code
      }
      sattable_to_disaggregate$FlowUUID[is.na(sattable_to_disaggregate$FlowUUID)] <- ""
      for(flow in unique(sattable_to_disaggregate$FlowUUID)) {
        allocation_df <- subset(disagg$EnvFileDF, (FlowUUID==flow & Sector %in% gsub("/.*","",allocating_sectors)))
        if(nrow(allocation_df)==0) {
          allocation_vector <- NULL
        } else {
          allocation_vector <- setNames(allocation_df$FlowRatio, allocation_df$Sector)
        }
        
        disaggregated_flows <- disaggregateSatelliteSubsetByRatio(subset(sattable_to_disaggregate, FlowUUID==flow, colnames(sattable)),
                                                                  disagg, allocating_sectors = allocating_sectors, allocation_vector)
        sattable <- rbind(sattable, disaggregated_flows)
      }
    } else if(!is.null(disagg$EnvFileDF)) {
      # If satellite table data is provided as new flow by sector totals file
      # Select only those rows from the disaggregation env file that apply for this satellite table
      new_sector_totals <- subset(disagg$EnvFileDF, SatelliteTable==sat_spec$Abbreviation)
      if(nrow(new_sector_totals)==0) {
        logging::logwarn(paste0("No data found for disaggregation of ",sat_spec$Abbreviation, " for ",
                                disagg$OriginalSectorCode, " - applying default allocation"))
        sattable <- rbind(sattable, disaggregateSatelliteSubsetByRatio(subset(sattable, Sector==original_code, colnames(sattable)),
                                                                       disagg, allocating_sectors = allocating_sectors))
      } else {
        # Check for errors in satellite table
        new_sector_totals <- conformTbStoStandardSatTable(new_sector_totals)
        included_sectors <- unique(new_sector_totals[,"Sector"])
        if (!identical(sort(included_sectors),sort(unlist(gsub("/.*","",disagg$DisaggregatedSectorCodes))))) {
          logging::logwarn("Satellite table does not include all disaggregated sectors")
        }
        # Append to the main dataframe
        sattable <- rbind(sattable,new_sector_totals)
        }
    } else {
      # No satellite table data provided, use default allocation
      sattable <- rbind(sattable, disaggregateSatelliteSubsetByRatio(subset(sattable, Sector==original_code, colnames(sattable)),
                                                                     disagg,  allocating_sectors = allocating_sectors))
    }
  } 
  # Remove data for the original sector
  sattable_disaggregated <- subset(sattable, !(Sector %in% codes))

  return(sattable_disaggregated)
}

#' Disaggregate make table based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateMakeTable <- function (model, disagg) {
  
  #specify type of disaggregation
  disaggType = disagg$DisaggregationType

  #disaggregation can be of types "Predefined" or "UserDefined". 
  if(disaggType == "Predefined" | is.null(disagg$MakeFileDF)) {
    disaggTable <- uniformDisagg(model, disagg, model$MakeTransactions)
  } else if(disaggType == "Userdefined") {
    disaggTable <- specifiedMakeDisagg(model, disagg)
  } else {
    stop("Disaggregation not performed, type not defined")
  }
  
  return(disaggTable)
}

#' Disaggregate Use table based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param domestic A logical value indicating whether to disaggregate domestic final demand.
#' @return A standardized make table with old sectors removed and new sectors added.
disaggregateUseTable <- function (model, disagg, domestic = FALSE) {

  #specify type of disaggregation
  disaggType = disagg$DisaggregationType
  
  #disaggregation can be of types "Predefined" or "UserDefined". 
  if(disaggType == "Predefined" | is.null(disagg$UseFileDF)) {
    if(domestic) {
      table <- model$DomesticUseTransactions
    } else {
      table <- model$UseTransactions
    }
    disaggTable <- uniformDisagg(model, disagg, table)
  } else if(disaggType == "Userdefined") {
    disaggTable <- specifiedUseDisagg(model, disagg, domestic)
  } else {
    stop("Disaggregation not performed, type not defined")
  }

  return(disaggTable)
}


#' Disaggregate Final Demand based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param domestic A logical value indicating whether to disaggregate domestic final demand.
#' @return A standardized final demand table with old sectors removed and new sectors with manual and default allocations added.
disaggregateFinalDemand <- function(model, disagg, domestic = FALSE) {

  if(domestic) {
    if (model$specs$CommodityorIndustryType=="Commodity") {
      originalFD <-model$DomesticFinalDemand
    } else {
      originalFD <- model$DomesticFinalDemandbyCommodity
    }
  } else {
    if (model$specs$CommodityorIndustryType=="Commodity") {
      originalFD <-model$FinalDemand
    } else {
      originalFD <- model$FinalDemandbyCommodity
    }
  }
  #specify type of disaggregation
  disaggType = disagg$DisaggregationType
  
  #disaggregation can be of types "Predefined" or "UserDefined". 
  if(disaggType == "Predefined") {
    disaggTable <- disaggregateCols(originalFD, disagg, duplicate = FALSE, notUniform = FALSE)

  } else if(disaggType == "Userdefined") {
    
    #Column names in Final Demand
    fdColNames <- colnames(model$FinalDemand)
    #Allocation for FD demand sectors
    FDPercentages <- subset(disagg$UseFileDF, IndustryCode %in% fdColNames)
    #Assigning allocations for FD
    AllocFDDF <- applyAllocation(disagg, FDPercentages, "FinalDemand", originalFD)

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
    stop("Disaggregation not performed, type not defined")
  }

  return(disaggTable)
  
}

#' Disaggregate Value Added based on specs
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return A standardized Vale Added table with old sectors removed and new sectors with manual and default allocations added.
disaggregateVA <- function(model, disagg) {

  #specify type of disaggregation
  disaggType = disagg$DisaggregationType
  
  #disaggregation can be of types "Predefined" or "UserDefined". 
  
  if(disaggType == "Predefined") {
    disaggTable <- disaggregateRows(model$UseValueAdded, disagg, duplicate = FALSE, notUniform = FALSE)

  } else if(disaggType == "Userdefined") {
    
    #Row names in value added
    VARowNames <- rownames(model$UseValueAdded)
    #Allocation for FD demand sectors
    VAPercentages <- subset(disagg$UseFileDF, CommodityCode %in% VARowNames)#if VA codenames are in the CommodityCode Column of the csv.
    #Assigning allocations for FD
    AllocVADF <- applyAllocation(disagg, VAPercentages, "ValueAdded", model$UseValueAdded)#need to edit applyAllocation to handle value added.

    ####assembling disaggregated VA
    #Determine number of commodities and industries in DisaggSpecs
    numNewSectors <- length(disagg$DisaggregatedSectorCodes)

    #Determine commodity and industry indeces corresponding to the original sector code
    originalColIndex <- which(colnames(model$UseValueAdded)==disagg$OriginalSectorCode)
    #Determine end index of disaggregated sectors
    endColIndex <- originalColIndex + numNewSectors

    tablePartOne <- model$UseValueAdded[, 1:originalColIndex-1]#all rows, columns to the left of diagg col
    tablePartTwo <- model$UseValueAdded[,-(1:originalColIndex)]#all rows, all columns except cols to left of disagg col
    
    disaggTable <- cbind(tablePartOne, AllocVADF, tablePartTwo)
  } else {
    stop("Disaggregation not performed, type not defined")
  }

  return(disaggTable)
  
}

#' Disaggregate make or use table uniformly based on the number of new sectors
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param table DataFrame of make or use table
#' @return A standardized make table with old sectors removed and new, uniformly disaggregated sectors added.
uniformDisagg <- function (model, disagg, table) {
  
  #Predefined disaggregation assumes 1 industry/commodity disaggregated uniformly into several, with  
  #values along the intersections disaggregated uniformly along the diagonal.

  #Determine number of commodities and industries in DisaggSpecs
  numNewSectors <- length(disagg$DisaggregatedSectorCodes) 
  
  #Determine commodity and industry indeces corresponding to the original sector code
  originalRowIndex <- which(rownames(table)==disagg$OriginalSectorCode)
  originalColIndex <- which(colnames(table)==disagg$OriginalSectorCode)

  ########Row disaggregation
  #Copy original row (ind) for disaggregation
  originalRowVector <- table[originalRowIndex,]
  
  disaggRows <- disaggregateRow(originalRowVector,disagg)
  
  ########Column disaggregation
  #Copy original Column (Com) for disaggregation
  originalColVector <-table[,originalColIndex, drop = FALSE]#drop = False needed to copy as dataframe
  
  disaggCols <- disaggregateCol(originalColVector,disagg)
  
  
  ########Intersection Disaggregation
  originalIntersection <- table[originalRowIndex, originalColIndex]
  
  #Divide intersection by number of new sectors
  originalIntersection <- originalIntersection/numNewSectors
  
  #Populate disaggregated intersection assuming equal values along the diagonal. Matrix variable. 
  disaggIntersection <- diag(originalIntersection,numNewSectors,numNewSectors)
  
  #Convert to data frame
  disaggIntersection = as.data.frame(t(disaggIntersection))
  
  #rename rows and columns
  colnames(disaggIntersection) <- disagg$DisaggregatedSectorCodes
  rownames(disaggIntersection) <- disagg$DisaggregatedSectorCodes
  
  
  disaggTable <- assembleTable(table, disagg, disaggCols, disaggRows, disaggIntersection)
  
  return(disaggTable)
  
}


#' Disaggregate multiple rows from a table.
#' @param RowVectors A dataframe containing the rows to disaggregate
#' @param disagg_specs Specifications for disaggregating the current Table
#' @param duplicate A flag that indicates whether the disaggregated rows are to be duplicated or not (e.g. for CPI values)
#' @param notUniform A flag that indicates whether the disaggregated rows are to be disaggregated in uniform manner or not
#' @return A dataframe with disaggregated rows.
disaggregateRows <- function (RowVectors, disagg_specs, duplicate=FALSE, notUniform = FALSE) {
  
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
disaggregateCols <- function (ColVectors, disagg_specs, duplicate=FALSE, notUniform = FALSE) {
  
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
disaggregateRow <- function (originalRowVector, disagg_specs, duplicate = FALSE, notUniform = FALSE) {
  
  numNewSectors <- length(disagg_specs$DisaggregatedSectorCodes)
  
  if (duplicate) {
    #For handling CPI. Just copy the CPI values of the original sector to for all the disaggregated sectors.
    disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
  } else if(notUniform) {
    percentages <- getDisaggCommodityPercentages(disagg_specs)#get default disaggregated commodity percentages
    disaggRows <- originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),, drop=FALSE]#repeat the original vector numNewSector times
    disaggRows <- disaggRows * percentages[,3]#multiply the values in the repeated vector by the default percentages to get values allocated by industry totals
  } else {
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
  
  if (duplicate) {
      #For handling CPI. Just copy the CPI values of the original sector to for all the disaggregated sectors.
      disaggRows <-originalRowVector[rep(seq_len(nrow(originalRowVector)), numNewSectors),,drop=FALSE]
  } else if(notUniform) {
    percentages <- getDisaggIndustryPercentages(disagg_specs)#get defaul disaggregated industry percentages
    percentageOrder <- percentages[match(disagg_specs$DisaggregatedSectorCodes, percentages$CommodityCode),]
    disaggCols <- originalColVector[, rep(seq_len(ncol(originalColVector)), numNewSectors)]#repeat the original vector numNewSector times
    disaggCols <- data.frame(t(t(disaggCols)*percentageOrder[,3]))
  } else {
    #Create new cols with the uniform values
    uniformColVector <- originalColVector/numNewSectors
    disaggCols <- uniformColVector[, rep(seq_len(ncol(uniformColVector)), numNewSectors)]
    
  }
  
  #Rename cols to use the disaggregated codes
  colnames(disaggCols) <- disagg_specs$DisaggregatedSectorCodes
  
  return(disaggCols)
}

#' Disaggregate the MasterCrosswalk to include the new sectors for disaggregation
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return crosswalk with new sectors added.
disaggregateMasterCrosswalk <- function (model, disagg){
  new_cw <- model$crosswalk #variable to return with complete changes to crosswalk#temp

  secLength <- regexpr(pattern ='/',disagg$OriginalSectorCode) - 1 #used to determine the length of the sector codes. E.g., detail would be 6, while summary would generally be 3 though variable, and sector would be variable
  cw <- disagg$NAICSSectorCW[, c('NAICS_2012_Code','USEEIO_Code')]
  cw$USEEIO_Code <- sub("/.*","",cw$USEEIO_Code) # For all rows in the USEEIO_Code column, remove all characters after (and including) "/"

  #Update original sector codes with disaggregated sector codes in the relevant column (i.e. cwColIndex) where rows have an exact match for the disaggregated codes in the NAICS column
  new_cw <-merge(new_cw, cw, by.x=c("NAICS"), by.y=c("NAICS_2012_Code"), all=T)
  
  new_cw$USEEIO <- ifelse(is.na(new_cw$USEEIO_Code), new_cw$USEEIO, new_cw$USEEIO_Code)
  
  #Update remaining rows where the original sector is present in cwColIndex but there is no exact match in the NAICS column for the disaggregated sector codes (e.g. 2-5 level NAICS codes)
  remainingDisaggNAICSIndex <- which(new_cw$USEEIO == substr(disagg$OriginalSectorCode,1,secLength))
  
  for (i in seq_along(remainingDisaggNAICSIndex)){
    disaggNAICSIndex <- which(new_cw$USEEIO == substr(disagg$OriginalSectorCode,1,secLength))
    crosswalkRow <- new_cw[disaggNAICSIndex[1],] #extract current row where code in last column needs to be updated
    
    # if NAICS is NA map the entire new list of sectors
    if(is.na(crosswalkRow$NAICS[1])) {
      rowComparisons[1:length(disagg$DisaggregatedSectorCodes)] <- TRUE
    } else {
      #compare the value in the first column (NAICS) to the NAICS values in the disaggCrosswalk. Result is a string with TRUE where first column is a substring of values in disaggCrosswalk
      rowComparisons <- grepl(crosswalkRow$NAICS[1], disagg$NAICSSectorCW$NAICS_2012_Code) 
    }
    
    rowReplacements <- disagg$NAICSSectorCW$NAICS_2012_Code[rowComparisons] #Get the NAICS sector codes in the disagg crosswalk that are a match for the NAICS substring in the master crosswalk 
    rowReplacements <- sub("/.*","",disagg$NAICSSectorCW$USEEIO_Code[rowComparisons]) #Get the disaggregated sector codes that are mapped to the matches of the NAICS substring
    rowReplacements <- unique(rowReplacements) #reduce the list to the unique number of disaggregated sectors that the row comparisons map to
    
    crosswalkRow <- crosswalkRow[rep(seq_len(nrow(crosswalkRow)), length(rowReplacements)),, drop=FALSE] #replicate the crosswalk row as many times as there were matches in the substring search
    crosswalkRow$USEEIO <- rowReplacements #replace the values in the last column (e.g. originalSectorCode) with the newSectorCodes that matched the substring search
    new_cw <- rbind(new_cw[1:disaggNAICSIndex[1]-1,],crosswalkRow, new_cw[-(1:disaggNAICSIndex[1]),]) #include the expanded rows in the crosswalk
  }
  
  #renaming rows of crosswalk
  rownames(new_cw) <- 1:nrow(new_cw)
  new_cw$USEEIO_Code <- NULL
  return(new_cw)
  
}

#' Disaggregate make table based on the allocations specified in the files referenced in the diaggregation specs.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return A standardized make table with old sectors removed and new disaggregated sectors added based on the allocations in the disaggregation specs.
specifiedMakeDisagg <- function (model, disagg){
  
  #Local variable for original sector code
  originalSectorCode <- disagg$OriginalSectorCode
  #Local variable for new sector codes
  newSectorCodes <- disagg$DisaggregatedSectorCodes
  #Local variable for Make table allocations
  makeAllocations <- disagg$MakeFileDF
  
  ###Disaggregate Make Rows, Columns, and Intersection while using the allocation data extracted from the Disaggregation csv. 
  
  #Allocations for column (commodity) disaggregation. 
  #Get rows of the DF which do not contain the original sector code or the new sector codes in the industry column (e.g., get only non 562 sector codes when doing waste disaggregation),
  #and where only the new sector codes are present in the commodity column.
  colPercentages <- subset(makeAllocations, !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes) & CommodityCode %in% newSectorCodes)
  #Applying allocation to disaggregate columns
  disaggregatedColumns <- applyAllocation(disagg,colPercentages,"MakeCol", model$MakeTransactions) 
  
  #Allocations for make intersection. Get rows of DF where only new sector codes are present in both the industryCode and commodityCode columns. 
  intersectionPercentages <-subset(makeAllocations, IndustryCode %in% newSectorCodes & CommodityCode %in% newSectorCodes)
  #Assigning allocations for disaggregated intersection
  disaggregatedIntersection <- applyAllocation(disagg,intersectionPercentages,"MakeIntersection", model$MakeTransactions)
  
  #Allocations for the row (industry) disaggregation. Get all rows of the DF where new sector codes are in the industryCode column, and neither the original nor new sector codes are in the commodityColumn. 
  rowsPercentages <- subset(makeAllocations, IndustryCode %in% newSectorCodes & !(CommodityCode %in% originalSectorCode) & !(CommodityCode %in% newSectorCodes))
  #Assigning allocations for disaggregated rows
  disaggregatedRows  <- applyAllocation(disagg,rowsPercentages,"MakeRow", model$MakeTransactions)

  DisaggMake <- assembleTable(model$MakeTransactions, disagg, disaggregatedColumns, disaggregatedRows, disaggregatedIntersection)
  
  return(DisaggMake)
  
}


#' Disaggregate use table based on the allocations specified in the files referenced in the disaggregation specs.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param domestic Flag that indicates where to use the Domestic Use or UseTransactions table
#' @return A standardized make table with old sectors removed and new disaggregated sectors added based on the allocations in the disaggregation specs.
specifiedUseDisagg <- function (model, disagg, domestic = FALSE){
  
  #Local variable for original sector code
  originalSectorCode <- disagg$OriginalSectorCode
  #Local variable for new sector codes
  newSectorCodes <- disagg$DisaggregatedSectorCodes
  #Local variable for Use table allocations
  UseAllocations <- disagg$UseFileDF
  #Column names in Final Demand
  fdColNames <- colnames(model$FinalDemand)
  VARowNames <- rownames(model$UseValueAdded)

  if(domestic) {
    originalUse<-model$DomesticUseTransactions  
  } else {
    originalUse<-model$UseTransactions
  }
  
  ###Disaggregate Use Rows, Columns, and Intersection while using the allocation data extracted from the Disaggregation.csv

  #Extracting intersection allocation. Get rows of DF where only new sector codes are present in both the industryCode and commodityCode columns. 
  intersectionPercentages <-subset(UseAllocations, IndustryCode %in% newSectorCodes & CommodityCode %in% newSectorCodes)
  
  #Applying allocations for disaggregated intersection
  disaggregatedIntersection <- applyAllocation(disagg,intersectionPercentages,"UseIntersection", originalUse)

  #Allocations for column (industry) disaggregation. 
  #Get rows of the DF which do not contain the original sector code or the new sector codes in the commodity column,
  #where no VA row names are present in the commodity Column, and only the new sector codes are present in the industry column
  colPercentages <- subset(UseAllocations, !(CommodityCode %in% originalSectorCode) & !(CommodityCode %in% newSectorCodes) & !(CommodityCode %in% VARowNames) & IndustryCode %in% newSectorCodes)
  
  #Applying allocation to disaggregat columns
  disaggregatedColumns <- applyAllocation(disagg,colPercentages,"UseCol", originalUse) 

  #Allocations for the row (commodity) disaggregation. Get all rows of the DF where:
  #new sector codes are in the CommodityCode column; the FD column codes are not in the IndustryCode; 
  #and neither the original nor new sector codes are in the IndustryCode column. 
  rowsPercentages <- subset(UseAllocations, CommodityCode %in% newSectorCodes & !(IndustryCode %in% fdColNames) & !(IndustryCode %in% originalSectorCode) & !(IndustryCode %in% newSectorCodes))

  #Assigning allocations for disaggregated rows
  disaggregatedRows  <- applyAllocation(disagg,rowsPercentages,"UseRow", originalUse)

  DisaggUse <- assembleTable(originalUse, disagg, disaggregatedColumns, disaggregatedRows, disaggregatedIntersection)

  return(DisaggUse)
  
}


#' Assemble Table from the various disaggregated components.
#' @param originalTable Dataframe. The original table before disaggregation
#' @param disagg Specifications for disaggregating the current Table
#' @param disaggCols Dataframe. Previously disaggregated columns of the table.
#' @param disaggRows Dataframe. Previously disaggregated rows of the table.
#' @param disaggIntersection Dataframe. Previously disaggregated intersection of the table.
#' @return The Disaggregated table as a dataframe with the disaggregated rows, columns, and intersection included
assembleTable <- function (originalTable, disagg, disaggCols, disaggRows, disaggIntersection){

  #Determine number of new sectors
  numNewSectors <- length(disagg$DisaggregatedSectorCodes) 
  
  #Determine commodity and industry indeces corresponding to the original sector code
  originalRowIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
  originalColIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
  
  #Determine end index of disaggregated sectors
  endRowIndex <- originalRowIndex + numNewSectors
  endColIndex <- originalColIndex + numNewSectors  
  
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
  
  #Appending bottom part of the table to top part of the table
  disaggTable <- rbind(disaggTable, disaggTableBottom)
  
  return(disaggTable)
  
}

#' Allocate values specified by the .yml disaggregation specs to the correct places in a disaggregated row/column of the Use/Make tables. 
#' @param disagg Specifications for disaggregating the current Table
#' @param allocPercentages Dataframe. A subset of the dataframe that contains the percentages to allocate to specific industry and commodity combinations in the disaggregated vector. Parameter use coordinated with @param vectorToDisagg
#' @param vectorToDisagg String. A parameter to indicate what table and what part of that table is being disaggregated (e.g. "MakeCol" or "Intersection") 
#' @param originalTable Dataframe. The original dataframe upon which allocation is performed (e.g., Make or Use)
#' @return A dataframe with the values specified in the disaggSpecs assigned to the correct Make or Use table indeces.
applyAllocation <- function (disagg, allocPercentages, vectorToDisagg, originalTable){
  
  #Local variable for new sector codes
  newSectorCodes <- disagg$DisaggregatedSectorCodes
  numNewSectors <- length(newSectorCodes)
  #Local variable for original sector code
  originalSectorCode <- disagg$OriginalSectorCode

  #These different if blocks are needed because of the different dimensions of the manual and default allocation vectors needed for disaggregating 
  #the Make and Use rows and columns. Each block initializes the manual and default allocation values for the relevant rows or columns.
  if(vectorToDisagg == "MakeRow") {
    #Set up for manual allocations
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[originalVectorIndex,]

    #Create new rows to store manual allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = ncol(originalTable), nrow = length(newSectorCodes)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- names(originalVector)
    rownames(manualAllocVector) <- newSectorCodes
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2

    defaultPercentages <- getDefaultAllocationPercentages(disagg$MakeFileDF, disagg,
                                                          numNewSectors, output='Commodity')

    #Create new rows to store default allocation values by copying the original row a number of times equal to the number of new sectors
    defaultAllocVector <- rbind(originalVector, originalVector[rep(1,numNewSectors-1),])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- defaultAllocVector*defaultPercentages[,1]
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- names(originalVector)
    rownames(defaultAllocVector) <- newSectorCodes
    
  } else if(vectorToDisagg == "MakeCol") {
    
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[,originalVectorIndex, drop = FALSE]

    #Create new cols to store allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = length(newSectorCodes), nrow = nrow(originalTable)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- newSectorCodes
    rownames(manualAllocVector) <- rownames(originalVector)
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2
    
    defaultPercentages <- getDefaultAllocationPercentages(disagg$MakeFileDF, disagg,
                                                          numNewSectors, output='Commodity')
    
    #Create new columns to store default allocation values by copying the original column a number of times equal to the number of new sectors
    defaultAllocVector <- cbind(originalVector, originalVector[,rep(1,numNewSectors-1)])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- data.frame(t(t(defaultAllocVector)*defaultPercentages[,1]))
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- newSectorCodes
    rownames(defaultAllocVector) <- rownames(originalVector)
    
  } else if(vectorToDisagg == "MakeIntersection") {
    
    intersection <- originalTable[which(rownames(originalTable)==disagg$OriginalSectorCode),
                                  which(colnames(originalTable)==disagg$OriginalSectorCode), drop=FALSE]

    defaultPercentages <- getDefaultAllocationPercentages(disagg$MakeFileDF, disagg,
                                                          numNewSectors, output='Commodity')
    
    defaultAllocVector <- calculateDefaultIntersection(intersection, defaultPercentages, newSectorCodes)

    manualAllocVector <- createBlankIntersection(newSectorCodes)
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2
        
  } else if(vectorToDisagg == "UseRow" || vectorToDisagg == "FinalDemand" ) {
    
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(rownames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[originalVectorIndex,]

    #Create new rows to store manual allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = ncol(originalTable), nrow = length(newSectorCodes)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- names(originalVector)
    rownames(manualAllocVector) <- newSectorCodes
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 2
    allocPercentagesColIndex <- 1
    
    defaultPercentages <- getDefaultAllocationPercentages(disagg$UseFileDF, disagg,
                                                          numNewSectors, output='Commodity')
    
    #Create new rows to store default allocation values by copying the original row a number of times equal to the number of new sectors
    defaultAllocVector <- rbind(originalVector, originalVector[rep(1,numNewSectors-1),])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- defaultAllocVector*defaultPercentages[,1]
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- names(originalVector)
    rownames(defaultAllocVector) <- newSectorCodes
    
  } else if (vectorToDisagg == "UseCol" || vectorToDisagg == "ValueAdded") {
    #Get commodity and/or industry indeces corresponding to the original sector code
    originalVectorIndex <- which(colnames(originalTable)==disagg$OriginalSectorCode)
    #Get original row or column
    originalVector <- originalTable[,originalVectorIndex, drop = FALSE]

    #Create new cols to store allocation values (all other values initiated to NA)
    manualAllocVector <- data.frame(matrix(ncol = length(newSectorCodes), nrow = nrow(originalTable)))
    
    #Assign correct column and row names to new rows dataframe
    colnames(manualAllocVector) <- newSectorCodes
    rownames(manualAllocVector) <- rownames(originalVector)
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 2
    allocPercentagesColIndex <- 1
    
    defaultPercentages <- getDefaultAllocationPercentages(disagg$UseFileDF, disagg,
                                                          numNewSectors, output='Industry')
    
    #Create new columns to store default allocation values by copying the original column a number of times equal to the number of new sectors
    defaultAllocVector <- cbind(originalVector, originalVector[,rep(1,numNewSectors-1)])
    #multiply all elements in row by default percentages to obtain default allocation values
    defaultAllocVector <- data.frame(t(t(defaultAllocVector)*defaultPercentages[,1]))
    
    #Assign correct column and row names to new rows dataframe
    colnames(defaultAllocVector) <- newSectorCodes
    rownames(defaultAllocVector) <- rownames(originalVector)
    
  } else if(vectorToDisagg == "UseIntersection") {

    intersection <- originalTable[which(rownames(originalTable)==disagg$OriginalSectorCode),
                                  which(colnames(originalTable)==disagg$OriginalSectorCode), drop=FALSE]
    
    defaultPercentages <- getDefaultAllocationPercentages(disagg$UseFileDF, disagg,
                                                          numNewSectors, output='Industry')
    
    defaultAllocVector <- calculateDefaultIntersection(intersection, defaultPercentages, newSectorCodes)

    manualAllocVector <- createBlankIntersection(newSectorCodes)
    
    #Assign lookup index for allocPercentages vector 
    allocPercentagesRowIndex <- 1
    allocPercentagesColIndex <- 2
    
  } else {
    #todo error handling
  }
  
  if(nrow(allocPercentages)>0) {
    #Check that there are manual allocations to perform
    #Loop to assign the manual allocations
    for (r in 1:nrow(allocPercentages)) {
      
      #Get data from current row of the data imported from the yml file. 
      rowAlloc <- allocPercentages[r,allocPercentagesRowIndex]
      colAlloc <- allocPercentages[r,allocPercentagesColIndex]
      allocationValue <- allocPercentages[r,3]
      
      #Get the indeces where the allocated values go in new disaggregated rows
      rowAllocIndex <- which(rownames(manualAllocVector)==rowAlloc)
      colAllocIndex <- which(colnames(manualAllocVector)==colAlloc)
      
      #Check for indexing errors
      if(length(rowAllocIndex)==0L) {
        logging::logdebug(paste("rowAlloc not found, no allocation made for row", rowAlloc, sep=" ", "in table."))
      }

      if(length(colAllocIndex)==0L) {
        logging::logdebug(paste("colAlloc not found, no allocation made for column", colAlloc, sep=" ", "in table."))
      }
      
      #Calculate value based on allocation percent
      if(vectorToDisagg == "MakeRow" || vectorToDisagg == "UseRow" || vectorToDisagg == "FinalDemand") {
        value <- originalVector[colAllocIndex]*allocationValue
      } else if(vectorToDisagg=="MakeCol" || vectorToDisagg=="UseCol" || vectorToDisagg == "ValueAdded") {
        value <- originalVector[rowAllocIndex, 1, drop = FALSE]*allocationValue #to keep value as a dataframe
      } else if(vectorToDisagg == "MakeIntersection" || vectorToDisagg=="UseIntersection") {
        value <- intersection[1, 1, drop = FALSE]*allocationValue #to keep value as a dataframe. Should be a 1x1 DF
      }
      
      #If either rowAlloc or column are not valid values, set value to 0 to avoid a runtime error
      if(ncol(value)==0) {
        value <- 0
      }
      #Assign value to correct index
      manualAllocVector[rowAllocIndex, colAllocIndex] <- value
    }
  } else {
    logging::logdebug(paste("rowAlloc not found, no allocation made for", vectorToDisagg, sep=" "))
  }

  #replace all NAs with 0
  manualAllocVector[is.na(manualAllocVector)] <-0
  
  #Replace values in the default allocation vector with values from the Manual allocation vector to finalize the vector disaggregation.

  if(vectorToDisagg == "MakeRow"|| vectorToDisagg == "MakeIntersection" || vectorToDisagg=="UseRow" || vectorToDisagg =="UseIntersection" || vectorToDisagg == "FinalDemand") {
    #assumption is that all columns where there was a manual allocation sum up to the value in the original row/column index.
    manualIndeces <- data.frame(which(colSums(manualAllocVector) !=0 ))
    
    if(nrow(manualIndeces) > 0) {
      for (i in 1:nrow(manualIndeces)) {
        #replace values from manual allocation into default allocation
        tempVector <- manualAllocVector[, manualIndeces[i,1], drop=FALSE]
        defaultAllocVector[, manualIndeces[i,1]] <- tempVector
      }
    }

  } else if (vectorToDisagg == "MakeCol" || vectorToDisagg == "UseCol" || vectorToDisagg == "ValueAdded") {
    #assumption is that all rows where there was a manual allocation sum up to the value in the original row/column index.
    manualIndeces <- data.frame(which(rowSums(manualAllocVector) !=0 ))
    
    if(nrow(manualIndeces) > 0) {
      for (i in 1:nrow(manualIndeces)) {
        #replace values from manual allocation into default allocation
        tempVector <- manualAllocVector[manualIndeces[i,1], , drop=FALSE]
        defaultAllocVector[manualIndeces[i,1],] <- tempVector
      }
    }

  } else {
    manualIndeces <- NA #temporary values 
  }

  return(defaultAllocVector)
  
}


#' Obtain a vector of allocation percentages from the specified source file based on disaggregations specifications.
#' @param FileDF dataframe of Make or Use disaggregation data
#' @param disagg Specifications for disaggregating the current Table
#' @param numNewSectors Int. Number of new sectors in the disaggregation
#' @param output String indicating whether allocation values should reference "Commodity" or "Industry" outputs by default
#' @return vector of allocation percentages
getDefaultAllocationPercentages <- function(FileDF, disagg, numNewSectors, output) {
  #Set up for default allocations
  #Get default allocation percentages based on commodity or industry output
  if(output == 'Industry') {
    defaultPercentages <- subset(FileDF, CommodityCode == disagg$OriginalSectorCode)
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$IndustryCode),]
  } else {
    defaultPercentages <- subset(FileDF, IndustryCode %in% disagg$OriginalSectorCode)
    defaultPercentages <- defaultPercentages[match(disagg$DisaggregatedSectorCodes, defaultPercentages$CommodityCode),]
  }

  #If there are no default percentages from values from csv (i.e. number of rows in defaultRowPercentages dataframe is 0) assume uniform split, otherwise use the csv values
  if(nrow(defaultPercentages)==0) {
    #Uniform split
    defaultPercentages <- data.frame(rep(1/numNewSectors, numNewSectors))
  } else {
    defaultPercentages <- defaultPercentages[, 3, drop=FALSE] #Extracting the column with the percentages
  }
  
  return(defaultPercentages)
}


#' Creates an empty dataframe matrix of disaggregated sectors.
#' @param newSectorCodes vector of named disaggregated sectors
#' @return square dataframe matrix with new sectors as row and column names
createBlankIntersection <- function (newSectorCodes) {
  #Create new intersection to store allocation values (all other values initiated to NA)
  intersection <- data.frame(matrix(ncol = length(newSectorCodes), nrow = length(newSectorCodes)))
  
  #Assign correct column and row names to new rows dataframe
  colnames(intersection) <- newSectorCodes
  rownames(intersection) <- newSectorCodes

  return(intersection)
}


#' Creates a square dataframe matrix with values assigned based on default percentages
#' @param originalIntersection int value of the original intersection to be disaggregated
#' @param defaultPercentages vector of allocation percentages
#' @param newSectorCodes vector of named disaggregated sectors
#' @return square dataframe matrix with new sectors as row and column names with default values
calculateDefaultIntersection <- function(originalIntersection, defaultPercentages, newSectorCodes) {
  numNewSectors <- length(newSectorCodes)
  #Create a dataframe to store values for the intersection. This dataframe is of dimensions [numNewSectors, 1]
  intersection <- data.frame(originalIntersection[rep(1,numNewSectors),])
  #multiply all elements in row by default percentages to obtain default allocation values
  intersection <- intersection*defaultPercentages[,1]
  
  #Diagonalize the populated column vector
  intersection <- diag(intersection[,1],numNewSectors,numNewSectors)
  intersection <- data.frame(intersection)
  
  #rename rows and columns
  colnames(intersection) <- newSectorCodes
  rownames(intersection) <- newSectorCodes
  
  return(intersection)
}


#' Obtain default disaggregation percentages for industries from the disaggregation input files. 
#' @param disagg Specifications for disaggregating the current Model
#' @return A dataframe with the default disaggregation percentages for the Industries of the current model
getDisaggIndustryPercentages <-function(disagg) {
  
  defaultPercentages <- subset(disagg$MakeFileDF, IndustryCode %in% disagg$OriginalSectorCode)#get all rows in MakefileDF that have the OriginalSectorCode in the IndustryCode column
  
  return(defaultPercentages)
}


#' Obtain default disaggregation percentages for commodities from the disaggregation input files. 
#' @param disagg Specifications for disaggregating the current Model
#' @return A dataframe with the default disaggregation percentages for the Commodities of the current model
getDisaggCommodityPercentages <- function(disagg) {
  
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
    
    if(model$specs$CommodityorIndustryType == "Industry"){
      FDIndTotals <- data.frame(colSums(model$FinalDemandbyCommodity))
    } else{
      FDIndTotals <- data.frame(colSums(model$FinalDemand))

    }
    
    targetIndTotals <- data.frame(rowSums(model$MakeTransactions))
    colnames(FDIndTotals) <- colnames(targetIndTotals) #needed for rbind step
    targetIndTotals <- rbind(targetIndTotals, FDIndTotals)
    
    targetComTotals <- data.frame(colSums(model$MakeTransactions))
    VAComTotals <- data.frame(rowSums(model$UseValueAdded))
    colnames(VAComTotals) <- colnames(targetComTotals) #needed for rbind step
    targetComTotals <- rbind(targetComTotals, VAComTotals)
    
    balancedDisaggFulluse <- applyRAS(data.matrix(disaggFullUse), targetComTotals[,1], targetIndTotals[,1], relative_diff = NULL, absolute_diff = 1E8, max_itr = 1E5)
    
    
  } else {
    #no change in fulluse
    balancedDisaggFullUse <- disaggFullUse
  }
  
  #break balancedDisaggFullUse back into model components
  domesticTables <- calculateBalancedDomesticTables(model, disagg, balancedDisaggFullUse)
  
  if(model$specs$CommodityorIndustryType == "Industry"){
    model$DomesticFinalDemandbyCommodity <- domesticTables$DomesticFinalDemand
    model$FinalDemandbyCommodity <- balancedDisaggFullUse[1:nrow(model$UseTransactions),-(1:ncol(model$UseTransactions))]
    
  } else{
    model$DomesticFinalDemand <- domesticTables$DomesticFinalDemand
    model$FinalDemand <- balancedDisaggFullUse[1:nrow(model$UseTransactions),-(1:ncol(model$UseTransactions))]
  }

#  model$FinalDemand <- balancedDisaggFullUse[1:nrow(model$UseTransactions),-(1:ncol(model$UseTransactions))]  
#  model$DomesticFinalDemand <- domesticTables$DomesticFinalDemand  

  model$DomesticUseTransactions <- domesticTables$DomesticUseTransactions
  model$UseTransactions <- balancedDisaggFullUse[1:nrow(model$UseTransactions), 1:ncol(model$UseTransactions)]
  model$UseValueAdded <- balancedDisaggFullUse[-(1:nrow(model$UseTransactions)),1:ncol(model$UseTransactions)]

  return(model)
  
}

#' Build a Full Use table using the Use transactions, Use value added, and final demand model objects
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @return dataframe representing a use table that includes the Use transactions, Use value added, and final demand sectors 
buildDisaggFullUse <- function(model, disagg) {
  
  disaggFullUse <- rbind(model$UseTransactions, model$UseValueAdded)
  
  if(model$specs$CommodityorIndustryType == "Industry"){
    originalFD <- model$FinalDemandbyCommodity
  } else{
    originalFD <- model$FinalDemand
  }
  
#  tempVA <- matrix(0, nrow(model$UseValueAdded), ncol(model$FinalDemand))
  tempVA <- matrix(0, nrow(model$UseValueAdded), ncol(originalFD))
#  colnames(tempVA) <- colnames(model$FinalDemand)
  colnames(tempVA) <- colnames(originalFD)
  rownames(tempVA) <- rownames(model$UseValueAdded)
#  fullFD <- rbind(model$FinalDemand, tempVA)
  fullFD <- rbind(originalFD, tempVA)
    
  disaggFullUse <- cbind(disaggFullUse, fullFD)
  
  return(disaggFullUse)
  
}


#' Calculate the domestic use transactions and final demand tables after RAS balancing
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param disagg Specifications for disaggregating the current Table
#' @param balancedFullUse A fullUse table (including UseTransactions, UseValueAdded, and FinalDemand), created to determine whether RAS balancing is needed
#' @return list containing balanced domesticFinalDemand and domesticUseTransactions dataframes. 
calculateBalancedDomesticTables <- function(model, disagg, balancedFullUse) {
  #Calculate domestic use transactions and domestic final demand based on balancedfullUse
  #Idea is to obtain the DomesticUse/UseTransaction ratio before balancing, and apply that to the balanced Use Transactions. 
  #Same for Domestic Final Demand
  domesticUseRatios <-  model$DomesticUseTransactions /model$UseTransactions 
  domesticUseRatios[is.na(domesticUseRatios)] <- 0# means numerator, denominator, or both are 0
  domesticUseRatios[domesticUseRatios == Inf] <-0 #inf means denominator is 0
  
  if(model$specs$CommodityorIndustryType == "Industry"){
    domesticFDRatios <- model$DomesticFinalDemandbyCommodity / model$FinalDemandbyCommodity
  } else {
    domesticFDRatios <- model$DomesticFinalDemand / model$FinalDemand
  }
  
  
#  domesticFDRatios <- model$DomesticFinalDemand / model$FinalDemand
  domesticFDRatios[is.na(domesticFDRatios)] <-0
  domesticFDRatios[domesticFDRatios == Inf] <- 0

  balancedDomesticUseTransactions <- balancedFullUse[1:nrow(model$UseTransactions), 1:ncol(model$UseTransactions)]
  balancedDomesticUseTransactions <- balancedDomesticUseTransactions * domesticUseRatios
  
  balancedDomesticFD <- balancedFullUse[1:nrow(model$UseTransactions),-(1:ncol(model$UseTransactions))]
  balancedDomesticFD <- balancedDomesticFD * domesticFDRatios
  
  newDomesticTables <- list("DomesticUseTransactions" = balancedDomesticUseTransactions, "DomesticFinalDemand" = balancedDomesticFD)
  
  return(newDomesticTables)
}

