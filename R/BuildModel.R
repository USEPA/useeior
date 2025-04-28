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
  model <- constructEEIOMatrices(model, configpaths)
  return(model)
}

#' Construct EEIO matrices based on loaded IO tables, built satellite tables,
#' and indicator tables.
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A list with EEIO matrices.
constructEEIOMatrices <- function(model, configpaths = NULL) {
  # Combine data into a single totals by sector df
  model$TbS <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  # Set common year for flow when more than one year exists
  model$TbS <- setCommonYearforFlow(model$TbS)
  # Generate coefficients 
  model$CbS <- generateCbSfromTbSandModel(model)
  
  model <- buildEconomicMatrices(model)
  
  # Generate B matrix
  logging::loginfo("Building B matrix (direct emissions and resource use per dollar)...")
  model$B <- createBfromFlowDataandOutput(model)
  B_h <- standardizeandcastSatelliteTable(model$CbS, model, final_demand=TRUE)
  if(!is.null(B_h)) {
    model$B_h <- as.matrix(B_h)
  }
  if(model$specs$ModelType == "EEIO-IH"){
    model$B <- hybridizeBMatrix(model)
  }
  if(!is.null(model$Indicators)) {
    # Generate C matrix
    logging::loginfo("Building C matrix (characterization factors for model indicators)...")
    model$C <- createCfromFactorsandBflows(model$Indicators$factors,rownames(model$B))

    # Add direct impact matrix
    logging::loginfo("Calculating D matrix (direct environmental impacts per dollar)...")
    model$D <- model$C %*% model$B
  }

  model <- buildPriceMatrices(model)
  
  if(!is.null(model$specs$ExternalImportFactors) && model$specs$ExternalImportFactors) {
    # Alternate model build for implementing Import Factors
    model <- buildModelwithImportFactors(model, configpaths)
  } else {
    # Standard model build procedure
  
    # Calculate total emissions/resource use per dollar (M)
    logging::loginfo("Calculating M matrix (total emissions and resource use per dollar)...")
    model$M <- model$B %*% model$L
  
    colnames(model$M) <- colnames(model$M)
    # Calculate M_d, the domestic emissions per dollar using domestic Leontief
    logging::loginfo("Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...")
    model$M_d <- model$B %*% model$L_d
    colnames(model$M_d) <- colnames(model$M)
  }  
  if(!is.null(model$Indicators)) {
    # Calculate total impacts per dollar (N), impact category x sector
    if(!is.null(model$M)) {
      logging::loginfo("Calculating N matrix (total environmental impacts per dollar)...")
      model$N <- model$C %*% model$M
    }
    if(!is.null(model$M_m)) {
      logging::loginfo("Calculating N_m matrix (total environmental impacts per dollar from imported activity)...")
      model$N_m <- model$C %*% model$M_m
    }
    logging::loginfo("Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...")
    model$N_d <- model$C %*% model$M_d
  }

  # Clean up model elements not written out or used in further functions to reduce clutter
  mat_to_remove <- c("MakeTransactions", "UseTransactions", "DomesticUseTransactions",
                     "UseValueAdded", "FinalDemand", "DomesticFinalDemand",
                     "InternationalTradeAdjustment", "CommodityOutput", "IndustryOutput",
                     "U_n", "U_d_n") 
  # Drop U_n_m, UseTransactions_m for models with external import factors
  if(!is.null(model$specs$ExternalImportFactors) && model$specs$ExternalImportFactors){
    mat_to_remove <- c(mat_to_remove, "U_n_m", "UseTransactions_m")
  }
  
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

#' Construct the economic matrices of an IO model based on loaded IO tables.
#' @param model An EEIO model object with model specs, IO tables
#' @return A list with EEIO economic matrices.
buildEconomicMatrices <- function(model) {
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

  if (model$specs$IODataSource=="stateior") {
    model$U_n <- generate2RDirectRequirementsfromUseWithTrade(model, domestic = FALSE)
    model$U_d_n <- generate2RDirectRequirementsfromUseWithTrade(model, domestic = TRUE)
  } else {
    model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
    model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse 
  }

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

  return(model)
}

#' Construct the price adjustment matrices, Rho, Tau, and Phi
#' @param model An EEIO model object with model specs and IO tables
#' @return A list with EEIO price adjustment matrices.
buildPriceMatrices <- function(model) {
  # Calculate year over model IO year price ratio
  logging::loginfo("Calculating Rho matrix (price year ratio)...")
  model$Rho <- calculateModelIOYearbyYearPriceRatio(model)

  if (model$specs$IODataSource!="stateior") {
    # Calculate producer over purchaser price ratio.
    logging::loginfo("Calculating Phi matrix (producer over purchaser price ratio)...")
    model$Phi <- calculateProducerbyPurchaserPriceRatio(model)
  }

  # Calculate basic over producer price ratio.
  logging::loginfo("Calculating Tau matrix (basic over producer price ratio)...")
  model$Tau <- calculateBasicbyProducerPriceRatio(model)
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
  hh_codes <- model$FinalDemandMeta[model$FinalDemandMeta$Group%in%c("Household"), "Code"]
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
        # Split out Household emissions and generate coefficients from final demand
        cbs_h_r_y <- generateFlowtoDollarCoefficient(tbs_r[tbs_r$Year==year & tbs_r$Sector %in% hh_codes, ],
                                                     year, model$specs$IOYear, r, IsRoUS = IsRoUS,
                                                     model, output_type = "Industry",
                                                     final_demand = TRUE)
        cbs_r <- rbind(cbs_r,cbs_r_y,cbs_h_r_y)
      }
      CbS <- rbind(CbS,cbs_r)
    }
  return(CbS)
}

#' Converts flows table into flows x sector matrix-like format
#' @param df a dataframe of flowables, contexts, units, sectors and locations
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @param final_demand, bool, generate matrix based on final demand columns
#' @return A matrix-like dataframe of flows x sector 
standardizeandcastSatelliteTable <- function(df, model, final_demand = FALSE) {
  # Add fields for sector as combinations of existing fields
  df[, "Sector"] <- apply(df[, c("Sector", "Location")],
                          1, FUN = joinStringswithSlashes)
  # Cast df into a flow x sector matrix
  df_cast <- reshape2::dcast(df, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount")
  # Move Flow to rowname so matrix is all numbers
  rownames(df_cast) <- df_cast$Flow
  df_cast$Flow <- NULL
  if(final_demand) {
    codes <- model$FinalDemandMeta[model$FinalDemandMeta$Group%in%c("Household"), "Code_Loc"]
    if(any(codes %in% colnames(df_cast))) {
      df_cast <- df_cast[, codes, drop=FALSE]
    } else {
      # no final demand emissions in any satellite table, no need for B_h
      return(NULL)
    }
  } else {
    # Complete sector list according to model$Industries
    df_cast[, setdiff(model$Industries$Code_Loc, colnames(df_cast))] <- 0
    # Adjust column order to be the same with V_n rownames
    df_cast <- df_cast[, model$Industries$Code_Loc]    
  }
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

  if("Greenhouse Gases" %in% factors$Indicator) {
    # Make sure CO2e flows are characterized (see issue #281)
    f <- B_flows[!(B_flows %in% factors$Flow) & grepl("kg CO2e", B_flows)]
    C["Greenhouse Gases", f] <- 1
  }
  # Filter and resort model C flows and make it into a matrix
  C <- as.matrix(C[, B_flows])
  return(C)
}

#' Build two-region models for all 50 states based on a single config reference file.
#' @param modelname Name of the model from a config file.
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @param validate bool, if TRUE print validation results for each model
#' @param year int, indicating for which year to run the models
#' @return A list of EEIO models for each state with complete components and attributes
#' @export
buildTwoRegionModels <- function(modelname, configpaths = NULL, validate = FALSE, year = NULL) {
  model_ls <- list()
  q_comparison_failures_ls <- list()
  basemodel <- initializeModel(modelname, configpaths)
  
  if(!is.null(year)){
    basemodel$specs$IOYear <- year
  }
  
  for (s in state.abb){
    model <- basemodel
    state <- paste("US", s, sep="-")
    model$specs$ModelRegionAcronyms[1] <- state
    cat("\n")
    logging::loginfo(paste0("Building two-region model for ",
                            paste(state, model$specs$ModelRegionAcronyms[2], sep="/"),
                            "..."))
    cat("\n")
    
    model_ls[[state]] <- tryCatch(
      {
        model <- loadIOData(model, configpaths)
        model <- loadandbuildSatelliteTables(model)
        model <- loadandbuildIndicators(model)
        model <- loadDemandVectors(model)
        model <- constructEEIOMatrices(model)
        if (validate) {
          printValidationResults(model)
        }
        model_ls[[state]] <- model
      },
      error=function(e)
      {
        message(paste0("Error for ", state, " model."))
        return(NA)
      }
    )# end of try catch
    
    
  }
  return(model_ls)
}

#' Build an IO model with economic components only.
#' @param modelname Name of the model from a config file.
#' @param configpaths str vector, paths (including file name) of model configuration file
#' and optional agg/disagg configuration file(s). If NULL, built-in config files are used.
#' @return A list of IO model with only economic components
#' @export
buildIOModel <- function(modelname, configpaths = NULL) {
  model <- initializeModel(modelname, configpaths)
  model <- loadIOData(model, configpaths)
  model <- loadDemandVectors(model)
  
  model <- buildEconomicMatrices(model)
  model <- buildPriceMatrices(model)

  logging::loginfo("EIO model build complete.")
  return(model)
}
