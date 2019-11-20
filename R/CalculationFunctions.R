# Model Calculations

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