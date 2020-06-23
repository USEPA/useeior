#' Prepare economic components of an EEIO form USEEIO model.
#' @param modelname Name of the model from a config file.
#' @export
#' @return A list with USEEIO model economic components.
prepareEEIOModel <- function(modelname) {
  startLogging()
  logging::loginfo('Begin model initialization...')
  model <- list()
  # Get model specs
  model$specs <- getModelConfiguration(modelname)
  # Get BEA IO tables
  model$BEA <- loadBEAtables(model$specs)
  # Get model$Industries and model$Commodities
  if (model$specs$ModelType=="US") {
    model$Commodities <- model$BEA$Commodities
    model$Industries <- model$BEA$Industries
  }
  # Get model$Make, model$Use, model$MakeTransactions, model$UseTransactions, and model$UseValueAdded
  model$Make <- model$BEA$Make
  model$Use <- model$BEA$Use
  model$MakeTransactions <- model$BEA$MakeTransactions
  model$UseTransactions <- model$BEA$UseTransactions
  model$DomesticUseTransactions <- model$BEA$DomesticUseTransactions
  model$UseValueAdded <- model$BEA$UseValueAdded
  # Get GDP tables
  model$GDP <- loadGDPtables(model$specs)
  # Replace Gross Output with Industry Output from Make if modellevel is "Detail"
  if (model$specs$BaseIOLevel=="Detail") {
    MakeIndustryOutput <- model$BEA$MakeIndustryOutput
    MakeIndustryOutput <- MakeIndustryOutput[rownames(model$GDP$BEAGrossOutputIO), colnames(MakeIndustryOutput), drop = FALSE]
    model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear)] <- MakeIndustryOutput[, 1]
  }
  # Get model$CommodityOutput, model$CommodityCPI, model$IndustryOutput, model$IndustryCPI, and model$FinalDemand
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    model$CommodityOutput <- generateCommodityOutputforYear(model$specs$PrimaryRegionAcronym, IsRoUS = FALSE, model)
    model$CommodityCPI <- generateCommodityCPIforYear(model$specs$IOYear, model) # return a one-column table for IOYear
    # Get model$FinalDemand
    model$FinalDemand <- model$BEA$UseFinalDemand
    # Get model$DomesticFinalDemand
    model$DomesticFinalDemand <- model$BEA$DomesticFinalDemand
  } else {
    # Get model$IndustryOutput from GDP tables
    if (model$specs$PrimaryRegionAcronym=="US") {
      model$IndustryOutput <- model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE]
    }
    # Get model$IndustryCPI from GDP tables
    model$IndustryCPI <- model$GDP$BEACPIIO[, as.character(model$specs$IOYear), drop = FALSE]
    # Transform model$BEA$UseFinalDemand with MarketShares
    model$FinalDemand <- transformFinalDemandwithMarketShares(model$BEA$UseFinalDemand, model)#This output needs to be tested - producing strange results
    # Transform model$BEA$DomesticFinalDemand with MarketShares
    model$DomesticFinalDemand <- transformFinalDemandwithMarketShares(model$BEA$DomesticFinalDemand, model)#This output needs to be tested - producing strange results
  }
  # Get model$SectorNames
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    USEEIONames <- utils::read.table(system.file("extdata", "USEEIO_Commodity_Code_Name.csv", package = "useeior"),
                                     sep = ",", header = TRUE, stringsAsFactors = FALSE)
    model$SectorNames <- merge(as.data.frame(model$Commodities, stringsAsFactors = FALSE), USEEIONames,
                               by.x = "model$Commodities", by.y = "Code", all.x = TRUE, sort = FALSE)
  } else {
    model$SectorNames <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
  }
  colnames(model$SectorNames) <- c("SectorCode", "SectorName")
  # Get model$IntermediateMargins and model$FinalConsumerMargins
  model$IntermediateMargins <- getMarginsTable(model, "intermediate")
  model$FinalConsumerMargins <- getMarginsTable(model, "final consumer")
  return(model)
}

#' Build an EEIO from USEEIO model.
#' @param model the model prepared with prepareEEIOModel().
#' @export
#' @return A list with USEEIO model components and attributes.
buildEEIOModel <- function(model) {
  # In order for the modifications of createBioeconomyModel() be done to re-use this code, prepareEEIOModel() must be outside this function.
  # For this, I had to change the parameter
  # # Prepare model
  # model <- prepareEEIOModel(modelname)
  # Generate matrices
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
  # model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse
  model$W <- as.matrix(model$BEA$UseValueAdded)

  if(model$specs$CommoditybyIndustryType == "Commodity") {
    logging::loginfo(paste("Building commodityxcommodity direct requirement matrix ..."))
    model$A <- model$U_n %*% model$V_n
    #  model$A_d <- model$U_d_n %*% model$V_n
  } else if(model$specs$CommoditybyIndustryType == "Industry") {
    logging::loginfo(paste("Building industryxindustry requirement matrix ..."))
    model$A <- model$V_n %*% model$U_n
    model$A_d <- model$V_n %*% model$U_d_n
  }
  # Create an imports requirements matrix 
  #model$A_m <- model$A - model$A_d
  # Generate satellite tables
  model$SatelliteTables <- loadsattables(model)
  # Combine satellite tables (coeffs_by_sector) into a single df
  StandardizedSatelliteTable <- data.frame()
  for (table in model$SatelliteTables$coeffs_by_sector) {
    StandardizedSatelliteTable <- rbind(StandardizedSatelliteTable, table)
  }
  # ransform into a flow x sector matrix
  StandardizedSatelliteTable["Flow"] <- apply(StandardizedSatelliteTable[, c("FlowName", "FlowCategory", "FlowSubCategory", "FlowUnit")],
                                              1 ,FUN = joinStringswithSlashes)
  StandardizedSatelliteTable["Sector"] <- apply(StandardizedSatelliteTable[, c("ProcessCode", "ProcessLocation")], 1, FUN = joinStringswithSlashes)
  
  #! Needs to be cast and made into matrix, but the problem is that the sectors need to have the order and completness of the model sectors list and not just those in the sat tables
  sattables_cast <- reshape2::dcast(StandardizedSatelliteTable, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount") #! check why aggregation is needed
  # Move Flow to rowname so matrix is all numbers
  rownames(sattables_cast) <- sattables_cast$Flow
  sattables_cast$Flow <- NULL
  # Complete sector list using model$Industries
  columns_to_add <- tolower(paste(model$Industries[!model$Industries%in%StandardizedSatelliteTable$ProcessCode], model$specs$PrimaryRegionAcronym, sep = "/"))
  sattables_cast[, columns_to_add] <- 0
  # Adjust column order to be the same with V_n rownames
  sattables_cast <- sattables_cast[, tolower(paste(rownames(model$V_n), model$specs$PrimaryRegionAcronym, sep = "/"))]
  # Generate B matrix
  model$B <- as.matrix(sattables_cast)
  
  #------------------------------------------------------------
  # Save this untransformed matrix for bioeconomy modifications
  model$B_untransformed <- model$B
  #------------------------------------------------------------
  # Transform B into a flowxcommodity matrix using market shares matrix for commodity models
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    model$B <- model$B %*% model$V_n
    colnames(model$B) <- tolower(paste(colnames(model$B), model$specs$PrimaryRegionAcronym, sep = "/"))
  }
  
  # Generate C matrix: LCIA indicators
  factors_from_static <- loadindicators(model$specs)
  factors_from_static$GHG <- tolower(paste(factors_from_static$Name, factors_from_static$Category, factors_from_static$Subcategory,
                                           factors_from_static$Unit, sep = "/"))
  model$C <- reshape2::dcast(factors_from_static, Abbreviation ~ GHG, value.var = "Amount")
  rownames(model$C) <- model$C$Abbreviation
  model$C <- as.matrix(model$C[, rownames(model$B)])
  model$C[is.na(model$C)] <- 0
  
  #Add direct impact matrix
  model$D <- model$C %*% model$B 
  
  # Calculates total requirements matrix as Leontief inverse of A (L)
  logging::loginfo("Calculating total requirements matrix...")
  I <- diag(nrow(model$A))
  #I_d <- diag(nrow(model$A_d))
  model$L <- solve(I - model$A)
  #model$L_d <- solve(I_d - model$A_d)
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating total emissions per dollar matrix...")
  model$M <- model$B %*% model$L
  colnames(model$M) <- tolower(paste(colnames(model$M), model$specs$PrimaryRegionAcronym, sep = "/"))
  #M_d are the domestic emissions per dollar using domestic Leontief
  #model$M_d <- model$B %*% model$L_d
  #colnames(model$M_d) <- tolower(paste(colnames(model$M_d), model$specs$PrimaryRegionAcronym, sep = "/"))
  #Calculate total requirements for imports - note different method
  #model$L_m <- model$A_m %*% model$L_d
  #M_e are the external emissions per dollar using the domestic technology assumption
  #model$M_e <- model$B %*% model$L_m
  #colnames(model$M_e) <- tolower(paste(colnames(model$M_e), model$specs$PrimaryRegionAcronym, sep = "/"))
  # Calculate total impacts per dollar (U), impact category x sector
  model$U <- model$C %*% model$M
  #U_d are the domestic impacts per dollar
  #model$U_d <- model$C %*% model$M_d
  #U_e are the external impacts per dollar using the domestic technology assumption
  #model$U_e <- model$C %*% model$M_e
  
  logging::loginfo("Model build complete.")
  return(model)
}