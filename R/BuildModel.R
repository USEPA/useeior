#' Build an EEIO form USEEIO model. Requires model object with 
#' loaded IO tables (see loadIOtables), built satellite tables, and built
#' @param model Model file loaded with IO tables and satellite tables built
#' @export
#' @return A list with USEEIO model components and attributes.
buildEEIOModel <- function(model) {
  if(model$specs$ModelType!="US"){
    stop("This function needs to be revised before it is suitable for multi-regional models")
  }
  
  # Generate matrices
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
  model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse
  model$W <- as.matrix(model$UseValueAdded)
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    logging::loginfo("Building commodity-by-commodity A matrix (direct requirement) ...")
    model$A <- model$U_n %*% model$V_n
    model$A_d <- model$U_d_n %*% model$V_n
  } else if(model$specs$CommoditybyIndustryType == "Industry") {
    logging::loginfo("Building industry-by-industry A matrix (direct requirement) ...")
    model$A <- model$V_n %*% model$U_n
    model$A_d <- model$V_n %*% model$U_d_n
  }
  # Create an imports requirements matrix 
  logging::loginfo("Building A_m matrix (direct requirements import) ...")
  model$A_m <- model$A - model$A_d

  # Generate B matrix
  logging::loginfo("Building B matrix (direct emissions and resource use per dollar) ...")
  model$B <- createBfromEnvDataandOutput(model)
    
  # Generate C matrix
  logging::loginfo("Building C matrix (characterization factors for model indicators) ...")
  model$C <- createCfromFactorsandBflows(model$Indicators$factors,rownames(model$B))

  # Add direct impact matrix
  logging::loginfo("Calculating D matrix (direct environmental impacts per dollar) ...")
  model$D <- model$C %*% model$B 
  
  # Calculate total requirements matrix as Leontief inverse of A (L)
  logging::loginfo("Calculating L matrix (total requirements) ...")
  I <- diag(nrow(model$A))
  I_d <- diag(nrow(model$A_d))
  model$L <- solve(I - model$A)
  model$L_d <- solve(I_d - model$A_d)
  
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating M matrix (total emissions and resource use per dollar) ...")
  model$M <- model$B %*% model$L
  colnames(model$M) <- addSlashandNameItem(colnames(model$M), model$specs$PrimaryRegionAcronym)
  # Calculate M_d, the domestic emissions per dollar using domestic Leontief
  model$M_d <- model$B %*% model$L_d
  colnames(model$M_d) <- colnames(model$M)
  
  # Calculate total requirements for imports - note different method
  model$L_m <- model$A_m %*% model$L_d
  
  # Calculate M_e, the external emissions per dollar using the domestic technology assumption
  #model$M_e <- model$B %*% model$L_m
  #colnames(model$M_e) <- tolower(paste(colnames(model$M_e), model$specs$PrimaryRegionAcronym, sep = "/"))
  
  # Calculate total impacts per dollar (U), impact category x sector
  logging::loginfo("Calculating U matrix (total environmental impacts per dollar) ...")
  model$U <- model$C %*% model$M
  # Calculate U_d, the domestic impacts per dollar
  model$U_d <- model$C %*% model$M_d
  #Calculate U_e, the external impacts per dollar using the domestic technology assumption
  #model$U_e <- model$C %*% model$M_e
  
  logging::loginfo("Model build complete.")
  return(model)
}

createBfromEnvDataandOutput <- function(model) {
  # Generate coefficients 
  CbS <- generateCbSfromTbSandModel(model)
  CbS_cast <- standardizeandcastSatelliteTable(CbS,model)
  B <- as.matrix(CbS_cast)
  # Transform B into a flow x commodity matrix using market shares matrix for commodity models
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    B <- B %*% model$V_n
  }
  colnames(B) <- tolower(apply(cbind(colnames(B), model$specs$PrimaryRegionAcronym),
                                     1, FUN = joinStringswithSlashes))
  return(B)
}

generateCbSfromTbSandModel <- function(model) {
  TbS <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  CbS <- data.frame()
    #Loop through model regions to get regional output
    for (r in model$specs$ModelRegionAcronyms) {
      tbs_r <- TbS[TbS$Location==r, ]
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
                                                              model$specs$IOYear, r, IsRoUS = IsRoUS, model)
        cbs_r <- rbind(cbs_r,cbs_r_y)
      }
      CbS <- rbind(CbS,cbs_r)
    }

  return(CbS)
}


#' Generate C matrix from indicator factors and a model B matrix
#' @param factors, df in model$Indicators$factors format
#' @param B, the model B matrix to use for reference
#' @return a C matrix in indicator x flow format
createCfromFactorsandBflows <- function(factors,B_flows) {
  # Add flow field to factors
  factors$Flow <- tolower(apply(factors[, c("Flowable", "Context", "Unit")],
                                   1, FUN = joinStringswithSlashes))
  
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

