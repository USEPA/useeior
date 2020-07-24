#' Build an EEIO form USEEIO model.
#' @param model Model file loaded with IO tables.
#' @export
#' @return A list with USEEIO model components and attributes.
buildEEIOModel <- function(model) {

  # Generate matrices
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use
  model$U_d_n <- generateDirectRequirementsfromUse(model, domestic = TRUE) #normalized DomesticUse
  model$W <- as.matrix(model$BEA$UseValueAdded)
  if(model$specs$CommoditybyIndustryType == "Commodity") {
    logging::loginfo(paste("Building commodityxcommodity direct requirement matrix ..."))
    model$A <- model$U_n %*% model$V_n
    model$A_d <- model$U_d_n %*% model$V_n
  } else if(model$specs$CommoditybyIndustryType == "Industry") {
    logging::loginfo(paste("Building industryxindustry requirement matrix ..."))
    model$A <- model$V_n %*% model$U_n
    model$A_d <- model$V_n %*% model$U_d_n
  }
  # Create an imports requirements matrix 
  model$A_m <- model$A - model$A_d
  # Generate satellite tables
  model$SatelliteTables <- loadsattables(model)
  # Combine satellite tables (coeffs_by_sector) into a single df
  StandardizedSatelliteTable <- data.frame()
  for (table in model$SatelliteTables$coeffs_by_sector) {
    StandardizedSatelliteTable <- rbind(StandardizedSatelliteTable, table)
  }
  # transform into a flow x sector matrix
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

  # Transform B into a flow x commodity matrix using market shares matrix for commodity models
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
  I_d <- diag(nrow(model$A_d))
  model$L <- solve(I - model$A)
  model$L_d <- solve(I_d - model$A_d)
  # Calculate total emissions/resource use per dollar (M)
  logging::loginfo("Calculating total emissions per dollar matrix...")
  model$M <- model$B %*% model$L
  colnames(model$M) <- tolower(paste(colnames(model$M), model$specs$PrimaryRegionAcronym, sep = "/"))
  #M_d are the domestic emissions per dollar using domestic Leontief
  model$M_d <- model$B %*% model$L_d
  colnames(model$M_d) <- tolower(paste(colnames(model$M_d), model$specs$PrimaryRegionAcronym, sep = "/"))
  #Calculate total requirements for imports - note different method
  model$L_m <- model$A_m %*% model$L_d
  #M_e are the external emissions per dollar using the domestic technology assumption
  model$M_e <- model$B %*% model$L_m
  colnames(model$M_e) <- tolower(paste(colnames(model$M_e), model$specs$PrimaryRegionAcronym, sep = "/"))
  # Calculate total impacts per dollar (U), impact category x sector
  model$U <- model$C %*% model$M
  #U_d are the domestic impacts per dollar
  model$U_d <- model$C %*% model$M_d
  #U_e are the external impacts per dollar using the domestic technology assumption
  model$U_e <- model$C %*% model$M_e
  
  logging::loginfo("Model build complete.")
  return(model)
}

