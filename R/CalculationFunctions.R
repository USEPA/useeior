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

#' Dot multiplies two vectors to calculate an impact score and the percent contribution each score to the total
#' @param x, numeric vector of length n
#' @param y, numeric vector of length n
#' @return df, dataframe sorted from highest "contribution", also showing "x","y","impact" 
calculatePercentContributiontoImpact <- function (x,y) {
  df <- cbind.data.frame(x,y)
  df["impact"] <- df[,"x"]*df[,"y"] 
  df["contribution"] <- df["impact"]/sum(df["impact"])
  df <- df[order(df$contribution,decreasing=TRUE),]
  return(df) 
}

#' Calculate the percent contribution of sectors to an N indicator result
#' Uses model L matrix for total requirements and D matrix for direct indicator result
#' @param model, A complete EEIO Model object
#' @param sector, str, index of a model sector for use in the M matrix, e.g. "221100/us"
#' @param indicator, str, index of a model indicator for use in the C matrix, e.g. "Acidification Potential" 
#' @param domestic, boolean, sets model to use domestic flow matrix.  Default is FALSE.
#' @return df, dataframe sorted from highest process contribution "contribution", also showing "x","y","impact" 
#' @export 
calculateSectorContributiontoImpact <- function (model, sector, indicator, domestic=FALSE) {
  L <- model$L
  if (domestic) {
    L <- model$L_d
  }
  D <- model$D
  df <- calculatePercentContributiontoImpact( L[,sector],D[indicator,])
  return(df)
}

#' Calculate the percent contribution of M flows to an N indicator result
#' Uses model M matrix for flows and C matrix for indicator
#' @param model, A complete EEIO Model object
#' @param sector, str, index of a model sector for use in the M matrix, e.g. "221100/us"
#' @param indicator, str, index of a model indicator for use in the C matrix, e.g. "Acidification Potential" 
#' @param domestic, boolean, sets model to use domestic flow matrix.  Default is FALSE.
#' @return df, dataframe sorted from highest flow contribution "contribution", also showing "x","y","impact" 
#' @export 
calculateFlowContributiontoImpact <- function (model, sector, indicator, domestic=FALSE) {
  M <- model$M
  C <- model$C
  if (domestic) {
    M <- model$M_d
  }
  df <- calculatePercentContributiontoImpact(M[,sector], C[indicator,])
  return(df)
}
