# Model Calculations

#' Calculate total emissions/resources (LCI) or total impact for USEEIO model for a given demand vector and perspective.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param perspective Perspective of the model, can be "DIRECT", "INTERMEDIATE", or "FINAL".
#' @param demand A name of a built in model demand vector or a named vector with names as one or more model sectors and numeric values in USD with the same dollar year as model
#' @param use_domestic_requirements A boolean value: if TRUE, use domestic A_d; if FALSE, use A matrices.
#' @export
#' @return A list with LCI and LCIA results of the EEIO model.
calculateEEIOModel <- function(model, perspective, demand = "Production", use_domestic_requirements = FALSE) {
  result <- list()
  # Generate Direct Requirements (A) matrix and Demand dataframe flexibly based on "use_domestic"
  if (use_domestic_requirements) {
    L <- model$L_d
  } else {
    L <- model$L
  }
  
  #Try to load demand
  if (class(demand)=="character") {
    #assume this is a model build-in demand 
    #try to load the model vector
    meta <- model$DemandVectors$meta
    if (demand %in% meta$Name) {
      #Get the idea from the meta table, which will be the name of the vector
      id <- meta[which(meta$Name==demand),"ID"]
      d <- model$DemandVectors$vectors[[id]]
    } else {
      logging::logerror(paste("The name given for the demand,",demand,"is not in the model list of demand vectors."))
    }
    
  } else { 
    #! Need to check that the given demand 
    
    if (isDemandVectorValid(demand,L)) {
      
      d <-formatDemandVector(demand,L)
      
    } else {
      logging::logerror("Format of the demand vector is invalid. Cannot calculate result.")
    }
    
  } 
  
  #convert it into a matrix
  f <- as.matrix(d)  

  # DirectPerspective LCI and DirectPerspective LCIA
  if (perspective=="DIRECT") {
    # Calculate DirectPerspectiveLCI (transposed m_d with total impacts in form of sectorxflows)
    logging::loginfo("Calculating Direct Perspective LCI...")
    c <- getScalingVector(L, f)
    result$LCI_d <- calculateDirectPerspectiveLCI(model$B, c)
    # Calculate DirectPerspectiveLCIA (transposed u_d with total impacts in form of sectorximpact categories)
    logging::loginfo("Calculating Direct Perspective LCIA...")
    result$LCIA_d <- calculateDirectPerspectiveLCIA(model$B, model$C, c)
  } else if (perspective=="FINAL") {
    # Calculate FinalPerspectiveLCI 
    logging::loginfo("Calculating Final Perspective LCI...")
    result$LCI_f <- calculateFinalPerspectiveLCI(model$M, f)
    # Calculate FinalPerspectiveLCIA 
    logging::loginfo("Calculating Final Perspective LCIA...")
    result$LCIA_f <- calculateFinalPerspectiveLCIA(model$N, f)
  }
  
  logging::loginfo("Result calculation complete.")
  return(result)
}

#' Adjust multipliers based on currency year, price type, and margin type.
#' @param matrix_name Name of matrix representing the multiplier that needs price adjustment.
#' @param currency_year An integer representing the currency year.
#' @param purchaser_price A boolean value indicating whether to adjust producer's price to purchaser's price.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @export
#' @return A list of price-adjusted multipliers.
adjustMultiplierPrice <- function(matrix_name, currency_year, purchaser_price=TRUE, model) {
  price_adjusted_result <- list()
  # Adjust price year of multiplier
  if (currency_year!=model$specs$IOYear) {
    matrix <- adjustMultiplierPriceYear(matrix_name, currency_year, model)
  } else {
    logging::loginfo(paste("Keeping multipliers in", model$specs$IOYear, "dollar..."))
    matrix <- model[[matrix_name]]
  }
  # Adjust price type of multiplier
  if (purchaser_price) {
    price_adjusted_result[[paste(matrix_name, "pur", currency_year, sep = "_")]] <- adjustMultiplierPriceType(matrix, currency_year, model)
  } else {
    logging::loginfo("Keeping total emissions per dollar in producer prices...")
    price_adjusted_result[[paste(matrix_name, "pro", currency_year, sep = "_")]] <- matrix
  }
  logging::loginfo("Result price adjustment complete.")
  return(price_adjusted_result)
}

#' Calculate year by model IO year price ratio.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A data.frame of year by model IO year price ratio.
calculateYearbyModelIOYearPriceRatio <- function(model) {
  CPI_df <- model[[paste0("MultiYear", model$specs$CommoditybyIndustryType, "CPI")]]
  CPI_ratio <- CPI_df/CPI_df[, as.character(model$specs$IOYear)]
  return(CPI_ratio)
}

#' Calculate producer to purchaser price ratio.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A vector of producer to purchaser price ratio.
calculateProducerbyPurchaserPriceRatio <- function(model) {
  # Get Margins table
  Margins <- model$FinalConsumerMargins
  Margins <- merge(Margins, model$PriceYearRatio, by.x = "Code_Loc", by.y = 0, all.y = TRUE)
  # Prepare ratio table PHI
  PHI <- model$PriceYearRatio
  for (year in colnames(model$PriceYearRatio)) {
    # Because year of model$FinalConsumerMargins is model$specs$BaseIOSchema
    # Adjust ProducersValue from model$specs$BaseIOSchema to currency year using model$PriceYearRatio
    ProducersValue <- Margins$ProducersValue * (Margins[, year]/Margins[, as.character(model$specs$BaseIOSchema)])
    # Adjust Transportation, Wholesale and Retail using corresponding CPI_ratio
    TWR_CPI <- useeior::Sector_CPI_IO[c("48TW", "42", "44RT"), ]
    TWR_CPI_ratio <- TWR_CPI[, year]/TWR_CPI[, as.character(model$specs$BaseIOSchema)]
    TWRValue <- sweep(Margins[, c("Transportation", "Wholesale", "Retail")], 2, TWR_CPI_ratio, "*")
    # Re-calculate PurchasersValue
    PurchasersValue <- rowSums(Margins[, c("ProducersValue", "Transportation", "Wholesale", "Retail")])
    # Generate PRObyPURRatios, or phi vector
    PHI[, year] <- ProducersValue/(ProducersValue + rowSums(TWRValue))
  }
  PHI[is.na(PHI)] <- 1
  return(PHI)
}

#' Adjust multipliers from IO year to currency year price.
#' @param matrix_name Name of matrix representing the multiplier that needs price year adjustment.
#' @param currency_year An integer representing the currency year.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A matrix representing the multiplier that is adjusted to currency year price.
adjustMultiplierPriceYear <- function(matrix_name, currency_year, model) {
  #price_adjusted_result <- list()
  CPI_ratio <- model$PriceYearRatio[, as.character(currency_year)]
  logging::loginfo(paste("Adjusting multipliers from", model$specs$IOYear, "to", currency_year, "dollars..."))
  # Apply the adjustment in each row of the matrix
  matrix <- model[[matrix_name]] %*% diag(CPI_ratio)
  colnames(matrix) <- colnames(model[[matrix_name]])
  #price_adjusted_result[[paste(matrix_name, "pro", currency_year, sep = "_")]] <- matrix
  return(matrix)
}

#' Adjust multipliers from producer to purchaser price.
#' @param matrix A matrix representing the multiplier that needs price type adjustment.
#' @param currency_year An integer representing the currency year.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A matrix representing the multiplier that is adjusted to purchaser price.
adjustMultiplierPriceType <- function(matrix, currency_year, model) {
  logging::loginfo("Adjusting total emissions per dollar from producer to purchaser prices...")
  matrix_new <- matrix %*% diag(model$PriceTypeRatio[, as.character(currency_year)])
  colnames(matrix_new) <- colnames(matrix)
  return(matrix_new)
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

#' The final perspective LCI aligns flows with sectors consumed by final users
#' Multiply the M matrix and the diagonal of demand, y.
#' @param M, a model M matrix, direct + indirect flows per $ output of sector.
#' @param y, a model demand vector
#' @return matrix, model sectors x model flows with total flows per sector
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 8.
calculateFinalPerspectiveLCI <- function(M, y) {
  lci_f <-  M %*% diag(as.vector(y))
  lci_f <- t(lci_f)
  colnames(lci_f) <- rownames(M)
  rownames(lci_f) <- colnames(M)
  return(lci_f)
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
  lcia_d <-  C %*% (B %*% diag(as.vector(c), nrow(c)))
  colnames(lcia_d) <- rownames(c)
  lcia_d <- t(lcia_d)
  return(lcia_d)
}

#' The final perspective aligns impacts with sectors consumed by final users
#' Multiply the N matrix and the diagonal of demand, y.
#' @param N, a model N matrix, direct + indirect impact per unit of the environmental flows.
#' @param y, a model demand vector
#' @return Transposed u_d with total impacts in form of sector x impact categories.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 8.
calculateFinalPerspectiveLCIA <- function(N, y) {
  lcia_f <-  N %*% diag(as.vector(y))
  lcia_f <- t(lcia_f)
  colnames(lcia_f) <- rownames(N)
  rownames(lcia_f) <- colnames(N)
  return(lcia_f)
}

#' Divide/Normalize a sector x flows matrix by the total of respective flow (column sum)
#' @param m A sector x flows matrix.
#' @export
#' @return A normalized sector x flows matrix.
normalizeResultMatrixByTotalImpacts <- function(m) {
  #Use sweep function to prevent error
  m_norm <- sweep(m, MARGIN = 2, FUN = "/", STATS = colSums(m))
  return(m_norm)
}
