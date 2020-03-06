# Model Calculations

#' Calculate total emissions/resources (LCI) or total impact for USEEIO model for a given demand vector and perspective.
#'
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param perspective Perspective of the model, can be "DIRECT", "INTERMEDIATE", or "FINAL".
#' @param demand A character value or a matrix: if the former, can be "production" or "consumption"; if the latter, can be demand matrix for one or more sectors.
#' @param for_imports_using_domestic If TRUE, use total requirements for imports matrix (A)
#' @param use_domestic A boolean value: if TRUE, use domestic A and FinalDemand matrices; if FALSE, use original A and FinalDemand matrices.
#'
#' @export
#' @return A list with LCI and LCIA results of the EEIO model.
calculateEEIOModel <- function(model, perspective, demand = "production", use_domestic = FALSE, for_imports_using_domestic=FALSE) {
  result <- list()
  # Generate Direct Requirements (A) matrix and Demand dataframe flexibly based on "use_domestic"
  if (use_domestic) {
    Demand <- model$DomesticFinalDemand
    L <- model$L_d
    #If this is for the imports
    if (for_imports_using_domestic) {
      L <- model$L_m
    }
  } else {
    Demand <- model$FinalDemand
    L <- model$L
  }
  # Generate f (vector) matrix based on "demand"
  if (demand=="production") {
    f <- as.matrix(rowSums(Demand))
  } else if (demand=="consumption") {
    f <- as.matrix(rowSums(Demand[, model$BEA$TotalConsumptionCodes]))
  } else if (class(demand)=="matrix") {
    if (ncol(demand)==ncol(Demand)) {
      # Replace sectors in Demand with the sectors from demand
      Demand_adj <- rbind(Demand[!rownames(Demand)%in%rownames(demand), ], demand)
      f <- as.matrix(rowSums(Demand_adj))
    } else if (ncol(demand)==1) {
      # Replace sectors in Demand with the sectors from demand
      f <- rbind(as.matrix(rowSums(Demand[!rownames(Demand)%in%rownames(demand), ])), demand)
    }
    # Order row sequence of f to be the same with that of Demand
    f <- as.matrix(f[match(rownames(Demand), rownames(f)), ])
  }
  # DirectPerspectiveLCI and DirectPerspectiveLCIA
  if (perspective=="DIRECT") {
    # Calculate DirectPerspectiveLCI (transposed m_d with total impacts in form of sectorxflows)
    logging::loginfo("Calculating Direct Perspective LCI...")
    c <- getScalingVector(L, f)
    result$LCI_d <- calculateDirectPerspectiveLCI(model$B, c)
    # Calculate DirectPerspectiveLCIA (transposed u_d with total impacts in form of sectorximpact categories)
    logging::loginfo("Calculating Direct Perspective LCIA...")
    result$LCIA_d <- calculateDirectPerspectiveLCIA(model$B, model$C, c)
  }
  
  logging::loginfo("Result calculation complete.")
  return(result)
}

#' Adjust multipliers based on currency year, price type, and margin type.
#' @param matrix A matrix representing the multiplier that needs price adjustment.
#' @param currency_year An integer representing the currency year.
#' @param purchaser_price A boolean value indicating whether to adjust producer's price to purchaser's price.
#' @param margin_type A character value: can be "intermediate" or "final consumer".
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @export
#' @return A list of price-adjusted multipliers.
adjustMultiplierPrice <- function(matrix, currency_year, purchaser_price=TRUE, margin_type="intermediate", model) {
  price_adjusted_result <- list()
  # Generate CPI_ratio based on currency_year and model$specs$IOYear
  if (model$specs$CommoditybyIndustryType=="Commodity") {
    currency_year_CPI <- generateCommodityCPIforYear(currency_year, model)
    CPI_ratio <- as.data.frame(currency_year_CPI[, as.character(currency_year)]/model$CommodityCPI[, as.character(model$specs$IOYear)])
    rownames(CPI_ratio) <- rownames(model$CommodityCPI)
  } else {
    CPI_ratio <- as.data.frame(model$GDP$BEACPIIO[, as.character(currency_year)]/model$GDP$BEACPIIO[, as.character(model$specs$IOYear)])
    rownames(CPI_ratio) <- rownames(model$GDP$BEACPIIO)
  }
  colnames(CPI_ratio) <- "Ratio"
  # Adjust from producer's to purchaser's price
  if (purchaser_price) {
    # Get Margins table based on margin_type
    if (margin_type=="intermediate") {
      Margins <- model$IntermediateMargins
    } else {
      Margins <- model$FinalConsumerMargins
    }
    logging::loginfo("Adjusting margins from IO year to currency year dollars...")
    # Adjust ProducersValue using CPI_ratio
    Margins <- merge(Margins, CPI_ratio, by.x = "SectorCode", by.y = 0, all.y = TRUE)
    Margins$ProducersValue <- Margins$ProducersValue * Margins$Ratio
    # Adjust Transportation, Wholesale and Retail using corresponding CPI_ratio
    TWR_CPI_ratio <- useeior::Sector_CPI_IO[c("48TW", "42", "44RT"), as.character(currency_year)]/useeior::Sector_CPI_IO[c("48TW", "42", "44RT"), as.character(model$specs$IOYear)]
    Margins[, c("Transportation", "Wholesale", "Retail")] <- sweep(Margins[, c("Transportation", "Wholesale", "Retail")], 2, TWR_CPI_ratio, "*")
    # Generate PRObyPURRatios vector
    Margins$PRObyPURRatios <- Margins$ProducersValue/rowSums(Margins[, c("ProducersValue", "Transportation", "Wholesale", "Retail")])
    Margins[is.na(Margins$PRObyPURRatios), "PRObyPURRatios"] <- 1
    PHI <- Margins$PRObyPURRatios
    logging::loginfo("Adjusting total emissions per dollar from producer to purchaser prices...")
    matrix_name <- paste(matrix, "pur", currency_year, sep = "_")
    price_adjusted_result[[matrix_name]] <- model[[matrix]] %*% diag(PHI)
    colnames(price_adjusted_result[[matrix_name]]) <- colnames(model[[matrix]])
  } else {
    # Adjust from IOYear to currency_year dollars
    if (!currency_year==model$specs$IOYear) {
      logging::loginfo("Adjusting multipliers from IO year to currency year dollars...")
      # Apply the adjustment in each row of the matrix
      matrix_name <- paste(matrix, "pro", currency_year, sep = "_")
      price_adjusted_result[[matrix_name]] <- model[[matrix]] %*% diag(CPI_ratio$Ratio)
      colnames(price_adjusted_result[[matrix_name]]) <- colnames(model[[matrix]])
    }
  }
  logging::loginfo("Result price adjustment complete.")
  return(price_adjusted_result)
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
#' @export
#' @return A normalized sector x flows matrix.
normalizeResultMatrixByTotalImpacts <- function(m) {
  #Use sweep function to prevent error
  m_norm <- sweep(m, MARGIN = 2, FUN = "/", STATS = colSums(m))
  return(m_norm)
}