#' Build an EEIO form USEEIO model.
#' @param modelname Name of the model from a config file.
#' @export
#' @return A list with USEEIO model components and attributes.
buildEEIOmodel <- function(modelname) {
  startLogging()
  logging::loginfo('Begin model initialization...')
  model <- list()
  # Get model specs
  model$specs <- getModelConfiguration(modelname)
  # Get BEA IO tables
  model$BEA <- loadBEAtables(model$specs)
  # Get model$Industries and model$Commodities
  if (model$specs$ModelType=="US") {
    model$Industries <- model$BEA$Industries
    model$Commodities <- model$BEA$Commodities
  }
  # Get GDP tables
  model$GDP <- loadGDPtables(model$specs)
  # Get model$CommodityOutput, model$CommodityCPI, model$IndustryOutput, model$IndustryCPI, and model$FinalDemand
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    model$CommodityOutput <- generatePriceAdjustedCommodityOutputforYear(model$specs$IOYear, model$specs$PrimaryRegionAcronym, IsRoU = FALSE, model)
    model$CommodityCPI <- generatePriceAdjustedCommodityCPIforYear(model$specs$IOYear, model) # return a one-column table for IOYear
    # Adjust model$FinalDemand by CPI
    ReferenceCurrencyYearCPI <- generatePriceAdjustedCommodityCPIforYear(model$specs$ReferenceCurrencyYear, model)
    ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio <- ReferenceCurrencyYearCPI[, as.character(model$specs$ReferenceCurrencyYear)]/model$CommodityCPI[, as.character(model$specs$IOYear)]
    if (model$specs$BaseIOLevel=="Detail") {
      ReferenceCurrencyYearCPI[ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio=="NaN", "ReferenceCurrencyYeartoOutputYearCPIRatio"] <- 1
    }
    model$FinalDemand <- model$BEA$UseFinalDemand[rownames(ReferenceCurrencyYearCPI), ]
    model$FinalDemand <- model$FinalDemand*ReferenceCurrencyYearCPI$ReferenceCurrencyYeartoOutputYearCPIRatio
  } else {
    # Get model$IndustryOutput from GDP tables
    if (model$specs$PrimaryRegionAcronym=="US") {
      model$IndustryOutput <- model$GDP$BEAGrossOutputIO[, as.character(model$specs$IOYear), drop = FALSE]
    }
    # Get model$IndustryCPI from GDP tables
    model$IndustryCPI <- model$GDP$BEACPIIO[, as.character(model$specs$IOYear), drop = FALSE]
    # Adjust model$FinalDemand by CPI
    ModelIndustryCPI <- model$IndustryCPI
    ModelIndustryCPI$ReferenceCurrencyYeartoOutputYearRatio <- model$GDP$BEACPIIO[, as.character(model$specs$ReferenceCurrencyYear)]/model$GDP$BEACPIIO[, as.character(model$specs$IOYear)]
    model$FinalDemand <- merge(model$BEA$UseFinalDemand, ModelIndustryCPI[, "ReferenceCurrencyYeartoOutputYearRatio", drop = FALSE], by = 0, all.x = TRUE)
    rownames(model$FinalDemand) <- model$FinalDemand$Row.names
    # Modify ReferenceCurrencyYeartoOutputYearRatio of Used and Other sectors to 1
    model$FinalDemand[is.na(model$FinalDemand$ReferenceCurrencyYeartoOutputYearRatio), "ReferenceCurrencyYeartoOutputYearRatio"] <- 1
    model$FinalDemand <- model$FinalDemand[, model$BEA$FinalDemandCodes]*ModelIndustryCPI$ReferenceCurrencyYeartoOutputYearRatio
    model$FinalDemand <- model$FinalDemand[, model$BEA$FinalDemandCodes]
    # Transform ModelFinalDeamnd with MS
    model$FinalDemand <- transformFinalDemandwithMarketShares(model$FinalDemand, model)#This output needs to be tested - producing strange results
  }
  # Get model$SectorNames
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    USEEIONames <- utils::read.table(system.file("extdata", "USEEIO_Commodity_Code_Name.csv", package = "useeior"),
                                     sep = ",", header = TRUE, stringsAsFactors = FALSE)
    model$SectorNames <- merge(as.data.frame(model$BEA$Commodities, stringsAsFactors = FALSE), USEEIONames,
                               by.x = "model$BEA$Commodities", by.y = "Code", all.x = TRUE)
  } else {
    SectorNamesFile <- paste(model$specs$BaseIOSchema, model$specs$BaseIOLevel, "Industry_Code_Name.csv", sep = "_")
    model$SectorNames <- utils::read.table(system.file("extdata", SectorNamesFile, package = "useeior"),
                                           sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }
  colnames(model$SectorNames) <- c("SectorCode", "SectorName")
  # Get model$IndustryMargins and model$FinalConsumerMargins
  model$IndustryMargins <- getMarginsTable(model$specs, "Industry") # Magrins Matrix
  model$IndustryMargins <- merge(as.data.frame(model$BEA$Commodities), model$IndustryMargins, by.x = "model$BEA$Commodities", by.y = "CommodityCode", all.x = TRUE)
  model$IndustryMargins[is.na(model$IndustryMargins$PRObyPURRatios), "PRObyPURRatios"] <- 1
  colnames(model$IndustryMargins)[1] <- "SectorCode"
  model$IndustryMargins$SectorCode <- as.character(model$IndustryMargins$SectorCode)
  
  model$FinalConsumerMargins <- getMarginsTable(model$specs, "FinalConsumer") # PCE&PEQ bridge df
  model$FinalConsumerMargins <- merge(as.data.frame(model$BEA$Commodities), model$FinalConsumerMargins, by.x = "model$BEA$Commodities", by.y = "CommodityCode", all.x = TRUE)
  model$FinalConsumerMargins[is.na(model$FinalConsumerMargins$PRObyPURRatios), "PRObyPURRatios"] <- 1
  colnames(model$FinalConsumerMargins)[1] <- "SectorCode"
  model$FinalConsumerMargins$SectorCode <- as.character(model$FinalConsumerMargins$SectorCode)
  

  ### Build Model
  # Generate Demand
  model$f <- as.matrix(rowSums(model$FinalDemand))
  # Generate IO tables
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model) #normalized Use

  if(model$specs$CommoditybyIndustryType == 'Commodity') {
    logging::loginfo(paste("Building commodityxcommodity direct requirement matrix ..."))
    model$A <- model$U_n %*% model$V_n #transformDirectRequirementswithMarketShares(U_n,V_n)
  } else if(model$specs$CommoditybyIndustryType == 'Industry') {
    logging::loginfo(paste("Building industryxindustry requirement matrix ..."))
    model$A <- model$V_n %*% model$U_n #transformDirectRequirementswithMarketShares(V_n,U_n)
  }
  # Generate satellite tables
  model$sattableslist <- loadsattables(model)
  # Combine satellite table dfs into a single df
  model$sattables <- data.frame()
  for (satable in model$sattableslist) {
    model$sattables <- rbind(model$sattables, satable)
  }
  # ransform into a flow x sector matrix
  model$sattables["Flow"] <- apply(model$sattables[, c("FlowName", "FlowCategory", "FlowSubCategory", "FlowUnit")], 1 ,FUN = joinStringswithSlashes)
  model$sattables["Sector"] <- apply(model$sattables[, c("ProcessCode", "ProcessLocation")], 1, FUN = joinStringswithSlashes)

  #! Needs to be cast and made into matrix, but the problem is that the sectors need to have the order and completness of the model sectors list and not just those in the sat tables
  model$sattables_cast <- reshape2::dcast(model$sattables, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount") #! check why aggregation is needed
  # Move Flow to rowname so matrix is all numbers
  rownames(model$sattables_cast) <- model$sattables_cast$Flow
  model$sattables_cast$Flow <- NULL
  # Complete sector list using model$BEA$Industries
  columns_to_add <- tolower(paste(model$BEA$Industries[!model$BEA$Industries%in%model$sattables$ProcessCode], model$specs$PrimaryRegionAcronym, sep = "/"))
  model$sattables_cast[, columns_to_add] <- 0
  # Adjust column order to be the same with V_n rownames
  model$sattables_cast <- model$sattables_cast[, tolower(paste(rownames(model$V_n), model$specs$PrimaryRegionAcronym, sep = "/"))]
  # Generate B matrix
  model$B <- as.matrix(model$sattables_cast)

  # Transform B into a flowxcommodity matrix using market shares matrix for commodity models
  if(model$specs$CommoditybyIndustryType == 'Commodity') {
    model$B <- model$B %*% model$V_n
    colnames(model$B) <- tolower(paste(colnames(model$B), model$specs$PrimaryRegionAcronym, sep = "/"))
  }

  # Generate C matrix: LCIA indicators
  factors_from_static <- loadindicators(model$specs)
  factors_from_static$GHG <- tolower(paste(factors_from_static$Name, factors_from_static$Category, factors_from_static$Subcategory,
                                           factors_from_static$Unit, sep = "/"))
  factors_from_static[factors_from_static$Name%in%c("Carbon dioxide", "Methane", "Dinitrogen monoxide"), "Group"] <- "Major GHGs"
  factors_from_static[!factors_from_static$Name%in%c("Carbon dioxide", "Methane", "Dinitrogen monoxide"), "Group"] <- "Other GHGs"
  model$C <- reshape2::dcast(factors_from_static, Group ~ GHG, value.var = "Amount")
  rm(factors_from_static)
  rownames(model$C) <- model$C$Group
  model$C <- as.matrix(model$C[, rownames(model$B)])
  model$C[is.na(model$C)] <- 0

  # Calculates total requirements matrix as Leontief inverse of A (L)
  logging::loginfo("Calculating total requirements matrix...")
  I <- diag(nrow(model$A))
  model$L <- solve(I-model$A)
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating total emissions per dollar matrix...")
  model$M <- model$B %*% model$L
  # Calculate total impacts per dollar (U), impact category x sector
  model$U <- model$C %*% model$M
  # Translates M from producer to purchaser price (M_bar)
  logging::loginfo("Adjusting total emissions per dollar from producer to purchaser prices...")
  PHI_C <- as.vector(model$IndustryMargins$PRObyPURRatios)
  #! PHI_C <- as.vector(model$FinalConsumerMargins$PRObyPURRatios)
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    model$M_bar <- model$M %*% diag(PHI_C)
    colnames(model$M_bar) <- model$Commodities
  } else {
    CM <- generateCommodityMixMatrix(model)
    PHI_I <- as.vector(PHI_C %*% CM)
    model$M_bar <- model$M %*% diag(PHI_I)
    colnames(model$M_bar) <- model$Industries
  }

  logging::loginfo("Model build complete.")

  return(model)
}

#' Calculate total emissions/resources (LCI) or total impact for USEEIO model for a given demand vector and perspective.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param perspective Perspective of the model, can be "DIRECT", "INTERMEDIATE", or "FINAL".
#' @export
#' @return A list with LCI and LCIA results of the EEIO model.
#'
calculate <- function(model, perspective) {
  result = list()
  if (perspective=="DIRECT") {
    # Calculate DirectPerspectiveLCI (transposed m_d with total impacts in form of sectorxflows)
    logging::loginfo("Calculating Direct Perspective LCI...")
    c <- getScalingVector(model$L, model$f)
    result$m_d <- calculateDirectPerspectiveLCI(model$B, c)
    # Normalize DirectPerspectiveLCI
    logging::loginfo("Normalize Direct Perspective LCI...")
    result$m_d_norm <- normalizeResultMatrixByTotalImpacts(result$m_d)
    # Calculate DirectPerspectiveLCIA (transposed u_d with total impacts in form of sectorximpact categories)
    logging::loginfo("Calculating Direct Perspective LCIA...")
    result$u_d <- calculateDirectPerspectiveLCIA(model$B, model$C, c)
    # Normalize DirectPerspectiveLCIA
    logging::loginfo("Normalize Direct Perspective LCIA...")
    result$u_d_norm <- normalizeResultMatrixByTotalImpacts(result$m_d)
  }

  logging::loginfo("Result calculation complete.")
  return(result)
}

#' Write model components to output folder.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @export
writeModelComponents <- function(model) {
  # Define output folder
  dir.create(paste("Model Builds/", model$specs$Model, sep = ""), recursive = TRUE) # meant to be flexible up to users
  outputfolder <- paste("Model Builds/", model$specs$Model, sep = "")

  # Demand
  if(model$specs$PrimaryRegionAcronym=="US") {
    Demand <- getUSTotalConsProd(model)
    #add in food system demand
    #Demand <- addDemandforSubsystem("food",Demand)
  }
  # Format model build components for IOMB
  MarketShares <- formatIOTableforIOMB(model$V_n, model)
  DirectRequirementsCoefficients <- formatIOTableforIOMB(model$A, model)
  # Satellite table
  sattableGHG <- model$sattableslist[["GHG"]]
  # Indicators
  indicatorsGHG <- model$C
  # write model build components to csv
  utils::write.csv(Demand, paste(outputfolder, "FinalDemand.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(MarketShares, paste(outputfolder, "MarketShares.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(DirectRequirementsCoefficients, paste(outputfolder, "DirectRequirementsCoefficients.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(sattableGHG, paste(outputfolder, "Satellite_GHG.csv", sep = "/"), row.names = FALSE)
  utils::write.csv(indicatorsGHG, paste(outputfolder, "Indicators_GHG.csv", sep = "/"), row.names = FALSE)
  # Write logs to file in Model Builds folder
  logtimestamp <- Sys.Date()
  dir.create("modelbuildlogs", recursive = TRUE) # meant to be flexible up to users
  logfilename <- paste0("modelbuildlogs/", model$specs$Model, logtimestamp, ".log")
  logging::addHandler(logging::writeToFile, file = logfilename, level = "INFO")

  logging::loginfo("Model components written to Model Build folder.")
}

#' Multiply the Leontief inverse L and the demand vector.
#' @param L Leontief inverse.
#' @param demand Final demand vector.
#' @return Scaling vector.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 8.
getScalingVector <- function(L, demand) {
  c <- L %*% demand
  return(c)
}

#' Multiply the B matrix and the scaling vector c.
#' @param B Marginal impact per unit of the environmental flows.
#' @param c Scaling vector.
#' @return Transposed m_d with total impacts in form of sector x flows.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 8.
calculateDirectPerspectiveLCI <- function(B, c) {
  m_d <- B %*% diag(as.vector(c), nrow(c))
  colnames(m_d) <- rownames(c)
  m_d <- t(m_d)
  return(m_d)
}

#' Multiply the C matrix and the product of B matrix and scaling vector c.
#' @param B Marginal impact per unit of the environmental flows.
#' @param C LCIA indicators.
#' @param c Scaling vector.
#' @return Transposed u_d with total impacts in form of sector x impact categories.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 8.
calculateDirectPerspectiveLCIA <- function(B, C, c) {
  u_d <-  C %*% (B %*% diag(as.vector(c), nrow(c)))
  colnames(u_d) <- rownames(c)
  u_d <- t(u_d)
  return(u_d)
}

#' Divide/Normalize a sector x flows matrix by the total of respective flow (column sum)
#' @param m A sector x flows matrix.
#' @return A normalized sector x flows matrix.
normalizeResultMatrixByTotalImpacts <- function(m) {
  #Use sweep function to prevent error
  m_norm <- sweep(m, MARGIN = 2, FUN = "/", STATS = colSums(m))
  return(m_norm)
}

