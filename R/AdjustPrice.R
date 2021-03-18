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
#' @return A data.frame of producer to purchaser price ratio.
calculateProducerbyPurchaserPriceRatio <- function(model) {
  # Get Margins table
  Margins <- model$FinalConsumerMargins
  Margins <- merge(Margins, model$Rho, by.x = "Code_Loc", by.y = 0, all.y = TRUE)
  # Prepare ratio table PHI
  PHI <- model$Rho
  for (year in colnames(model$Rho)) {
    # Because year of model$FinalConsumerMargins is model$specs$BaseIOSchema
    # Adjust ProducersValue from model$specs$BaseIOSchema to currency year using model$Rho
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
  CPI_ratio <- model$Rho[, as.character(currency_year)]
  logging::loginfo(paste("Adjusting multiplier from", model$specs$IOYear, "to", currency_year, "dollars..."))
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
  logging::loginfo("Adjusting multiplier from producer to purchaser price...")
  matrix_new <- matrix %*% diag(model$Phi[, as.character(currency_year)])
  colnames(matrix_new) <- colnames(matrix)
  return(matrix_new)
}

