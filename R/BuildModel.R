#' Build an EEIO model.
#' @param modelname Name of the model from a config file.
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A list of EEIO model complete components and attributes
#' @export
buildModel <- function(modelname, configpaths = NULL) {
  model <- initializeModel(modelname, configpaths)
  model <- loadIOData(model, configpaths)
  model <- loadandbuildSatelliteTables(model)
  model <- loadandbuildIndicators(model)
  model <- loadDemandVectors(model)
  model <- constructEEIOMatrices(model)
  return(model)
}

#' Construct EEIO matrices based on loaded IO tables, built satellite tables,
#' and indicator tables.
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @return A list with EEIO matrices.
constructEEIOMatrices <- function(model) {
  # Combine data into a single totals by sector df
  model$TbS <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  # Set common year for flow when more than one year exists
  model$TbS <- setCommonYearforFlow(model$TbS)
  # Generate coefficients 
  model$CbS <- generateCbSfromTbSandModel(model)
  
  # Generate matrices
  model$V <- as.matrix(model$MakeTransactions) # Make
  model$C_m <- generateCommodityMixMatrix(model) # normalized t(Make)
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  if (model$specs$CommodityorIndustryType=="Industry") {
    FinalDemand_df <- model$FinalDemandbyCommodity
    DomesticFinalDemand_df <- model$DomesticFinalDemandbyCommodity
  } else {
    FinalDemand_df <- model$FinalDemand
    DomesticFinalDemand_df <- model$DomesticFinalDemand
  }
  model$U <- as.matrix(dplyr::bind_rows(cbind(model$UseTransactions,
                                              FinalDemand_df),
                                        model$UseValueAdded)) # Use
  model$U_d <- as.matrix(dplyr::bind_rows(cbind(model$DomesticUseTransactions,
                                                DomesticFinalDemand_df),
                                          model$UseValueAdded)) # DomesticUse
  colnames(model$U_d) <- colnames(model$U)
  model[c("U", "U_d")] <- lapply(model[c("U", "U_d")],
                                 function(x) ifelse(is.na(x), 0, x))
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
  model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse
  model$q <- model$CommodityOutput
  model$x <- model$IndustryOutput
  model$mu <- model$InternationalTradeAdjustment
  if(model$specs$CommodityorIndustryType == "Commodity") {
    logging::loginfo("Building commodity-by-commodity A matrix (direct requirements)...")
    model$A <- model$U_n %*% model$V_n
    logging::loginfo("Building commodity-by-commodity A_d matrix (domestic direct requirements)...")
    model$A_d <- model$U_d_n %*% model$V_n
  } else if(model$specs$CommodityorIndustryType == "Industry") {
    logging::loginfo("Building industry-by-industry A matrix (direct requirements)...")
    model$A <- model$V_n %*% model$U_n
    logging::loginfo("Building industry-by-industry A_d matrix (domestic direct requirements)...")
    model$A_d <- model$V_n %*% model$U_d_n
  }
  
  if(model$specs$ModelType == "EEIO-IH"){
    model$A <- hybridizeAMatrix(model)
    model$A_d <- hybridizeAMatrix(model, domestic=TRUE)
  }

  # Calculate total requirements matrix as Leontief inverse (L) of A
  logging::loginfo("Calculating L matrix (total requirements)...")
  I <- diag(nrow(model$A))
  I_d <- diag(nrow(model$A_d))
  model$L <- solve(I - model$A)
  logging::loginfo("Calculating L_d matrix (domestic total requirements)...")
  model$L_d <- solve(I_d - model$A_d)
  
  # Generate B matrix
  logging::loginfo("Building B matrix (direct emissions and resource use per dollar)...")
  model$B <- createBfromFlowDataandOutput(model)

  if(model$specs$ModelType == "EEIO-IH"){
    model$B <- hybridizeBMatrix(model)
  }
    
  # Generate C matrix
  logging::loginfo("Building C matrix (characterization factors for model indicators)...")
  model$C <- createCfromFactorsandBflows(model$Indicators$factors,rownames(model$B))

  # Add direct impact matrix
  logging::loginfo("Calculating D matrix (direct environmental impacts per dollar)...")
  model$D <- model$C %*% model$B 
  
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating M matrix (total emissions and resource use per dollar)...")
  model$M <- model$B %*% model$L
  colnames(model$M) <- colnames(model$M)
  # Calculate M_d, the domestic emissions per dollar using domestic Leontief
  logging::loginfo("Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...")
  model$M_d <- model$B %*% model$L_d
  colnames(model$M_d) <- colnames(model$M)
  
  # Calculate total impacts per dollar (N), impact category x sector
  logging::loginfo("Calculating N matrix (total environmental impacts per dollar)...")
  model$N <- model$C %*% model$M
  # Calculate U_d, the domestic impacts per dollar
  logging::loginfo("Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...")
  model$N_d <- model$C %*% model$M_d
  
  # Calculate year over model IO year price ratio
  logging::loginfo("Calculating Rho matrix (price year ratio)...")
  model$Rho <- calculateModelIOYearbyYearPriceRatio(model)
  
  # Calculate producer over purchaser price ratio.
  logging::loginfo("Calculating Phi matrix (producer over purchaser price ratio)...")
  model$Phi <- calculateProducerbyPurchaserPriceRatio(model)
  
  #Clean up model elements not written out or used in further functions to reduce clutter
  mat_to_remove <- c("MakeTransactions", "UseTransactions", "DomesticUseTransactions",
                     "UseValueAdded", "FinalDemand", "DomesticFinalDemand",
                     "InternationalTradeAdjustment", "CommodityOutput", "IndustryOutput",
                     "U_n", "U_d_n")
  if (model$specs$CommodityorIndustryType=="Industry") {
    mat_to_remove <- c(mat_to_remove,
                       c("FinalDemandbyCommodity", "DomesticFinalDemandbyCommodity",
                         "InternationalTradeAdjustmentbyCommodity"))
  }
  model <- within(model, rm(list = mat_to_remove))
  
  if(model$specs$ModelType == "EEIO-IH"){
    model <- hybridizeModelObjects(model)
  }  
  
  logging::loginfo("Model build complete.")
  return(model)
}

#'Creates the B matrix from the flow data
#'@param model, a model with econ and flow data loaded
#'@return B, a matrix in flow x sector format with values of flow per $ output sector
createBfromFlowDataandOutput <- function(model) {

  CbS_cast <- standardizeandcastSatelliteTable(model$CbS,model)
  B <- as.matrix(CbS_cast)
  # Transform B into a flow x commodity matrix using market shares matrix for commodity models
  if(model$specs$CommodityorIndustryType == "Commodity") {
    B <- B %*% model$V_n
    colnames(B) <- model$Commodities$Code_Loc
  }
  return(B)
}

#' Prepare coefficients (x unit/$) from the totals by flow and sector (x unit)
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @return A dataframe of Coefficients-by-Sector (CbS) table
generateCbSfromTbSandModel <- function(model) {
  CbS <- data.frame()
    
    #Loop through model regions to get regional output
    for (r in model$specs$ModelRegionAcronyms) {
      tbs_r <- model$TbS[model$TbS$Location==r, ]
      cbs_r <- data.frame()
      if (r=="RoUS") {
        IsRoUS <- TRUE
      } else {
        IsRoUS <- FALSE
      }
      #Loop through data years
      data_years <- sort(unique(tbs_r$Year))
      for (year in data_years){
        cbs_r_y <- generateFlowtoDollarCoefficient(tbs_r[tbs_r$Year==year, ], year,
                                                   model$specs$IOYear, r, IsRoUS = IsRoUS,
                                                   model, output_type = "Industry")
        cbs_r <- rbind(cbs_r,cbs_r_y)
      }
      CbS <- rbind(CbS,cbs_r)
    }
  return(CbS)
}

#' Converts flows table into flows x sector matrix-like format
#' @param df a dataframe of flowables, contexts, units, sectors and locations
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @return A matrix-like dataframe of flows x sector 
standardizeandcastSatelliteTable <- function(df,model) {
  # Add fields for sector as combinations of existing fields
  df[, "Sector"] <- apply(df[, c("Sector", "Location")],
                          1, FUN = joinStringswithSlashes)
  # Cast df into a flow x sector matrix
  df_cast <- reshape2::dcast(df, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  # Move Flow to rowname so matrix is all numbers
  rownames(df_cast) <- df_cast$Flow
  df_cast$Flow <- NULL
  # Complete sector list according to model$Industries
  df_cast[, setdiff(model$Industries$Code_Loc, colnames(df_cast))] <- 0
  # Adjust column order to be the same with V_n rownames
  df_cast <- df_cast[, model$Industries$Code_Loc]
  return(df_cast)
}

#' Generate C matrix from indicator factors and a model B matrix
#' @param factors df in model$Indicators$factors format
#' @param B_flows Flows from B matrix to use for reference
#' @return C, a matrix in indicator x flow format
createCfromFactorsandBflows <- function(factors,B_flows) {
  # Add flow field to factors
  factors$Flow <- apply(factors[, c("Flowable", "Context", "Unit")],
                        1, FUN = joinStringswithSlashes)
  
  #Subset factor flows by flows in B matrix
  factors <- factors[factors$Flow %in% unique(B_flows),]
  
  C <- reshape2::dcast(factors, Indicator ~ Flow, value.var = "Amount")
  rownames(C) <- C$Indicator
  # Get flows in B not in C and add to C
  flows_inBnotC <- setdiff(B_flows, colnames(C))
  C[, flows_inBnotC] <- 0
  C[is.na(C)] <- 0
  # Filter and resort model C flows and make it into a matrix
  C <- as.matrix(C[, B_flows])
  return(C)
}

