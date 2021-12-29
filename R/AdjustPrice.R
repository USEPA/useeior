#' Adjust price year and type (producer's or purchaser's) of a model result matrix.
#' Model result matrices are M, M_d, N, N_d
#' Year adjustments from 2007-2018 supported
#' @param matrix_name Name of the result matrix that needs price adjustment, e.g. "N"
#' @param currency_year An integer representing the currency year, e.g. 2018.
#' @param purchaser_price A logical value indicating whether to adjust producer's price to purchaser's price.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A model result matrix after price adjustment
#' @export
adjustResultMatrixPrice <- function(matrix_name, currency_year, purchaser_price=TRUE, model) {
  # Adjust price year of matrix
  if (currency_year!=model$specs$IOYear) {
    logging::loginfo(paste("Adjusting", matrix_name, "matrix from",
                           model$specs$IOYear, "to", currency_year, "dollar..."))
    mat <- adjustMultiplierPriceYear(matrix_name, currency_year, model)
  } else {
    logging::loginfo(paste("Keeping", matrix_name, "matrix in", model$specs$IOYear, "dollar..."))
    mat <- model[[matrix_name]]
  }
  # Adjust price type of multiplier
  if (purchaser_price) {
    logging::loginfo(paste("Adjusting", matrix_name, "matrix from producer to purchaser price..."))
    mat <- adjustMultiplierPriceType(mat, currency_year, model)
  } else {
    logging::loginfo(paste("Keeping", matrix_name, "matrix in producer price..."))
  }
  logging::loginfo("Result price adjustment complete.")
  return(mat)
}

#' Calculate model IO year by year price ratio.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe of model IO year by year price ratio.
calculateModelIOYearbyYearPriceRatio <- function(model) {
  CPI_df <- model[[paste0("MultiYear", model$specs$CommodityorIndustryType, "CPI")]]
  CPI_ratio_matrix <- as.matrix(CPI_df[, as.character(model$specs$IOYear)]/CPI_df)
  return(CPI_ratio_matrix)
}

#' Calculate producer to purchaser price ratio.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe of producer to purchaser price ratio.
calculateProducerbyPurchaserPriceRatio <- function(model) {
  # Get Margins table
  Margins <- merge(model$Margins, model$Rho, by.x = "Code_Loc", by.y = 0, all.y = TRUE)
  Margins <- Margins[match(rownames(model$Rho), Margins$Code_Loc), ]
  # Prepare ratio table PHI
  PHI <- model$Rho
  for (year in colnames(model$Rho)) {
    # Adjust ProducersValue from model$specs$IOyear to currency year using model$Rho
    ProducersValue <- Margins$ProducersValue * (Margins[, year]/Margins[, as.character(model$specs$IOYear)])
    # Adjust Transportation, Wholesale and Retail using corresponding CPI_ratio
    TWR_CPI <- useeior::Sector_CPI_IO[c("48TW", "42", "44RT"), ]
    TWR_CPI_ratio <- TWR_CPI[, year]/TWR_CPI[, as.character(model$specs$IOYear)]
    TWRValue <- sweep(Margins[, c("Transportation", "Wholesale", "Retail")], 2, TWR_CPI_ratio, "*")
    # Calculate PurchasersValue
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
  CPI_ratio <- model$Rho[, as.character(currency_year)]
  # Apply the adjustment in each row of the matrix
  matrix <- model[[matrix_name]] %*% diag(CPI_ratio)
  colnames(matrix) <- colnames(model[[matrix_name]])
  return(matrix)
}

#' Adjust multipliers from producer to purchaser price.
#' @param matrix A matrix representing the multiplier that needs price type adjustment.
#' @param currency_year An integer representing the currency year.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A matrix representing the multiplier that is adjusted to purchaser price.
adjustMultiplierPriceType <- function(matrix, currency_year, model) {
  matrix_new <- matrix %*% diag(model$Phi[, as.character(currency_year)])
  colnames(matrix_new) <- colnames(matrix)
  return(matrix_new)
}

