# Model Calculations

#' Calculate total emissions/resources (LCI) and total impacts (LCIA) for an EEIO model
#' for a given perspective and demand vector.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param perspective Perspective of the model, can be "DIRECT" or "FINAL". "DIRECT" perspective
#' aligns results with the sectors in which they are produced, while "FINAL" perspective aligns
#' results with the sectors consumed by the final user.
#' @param demand A demand vector, can be name of a built-in model demand vector, e.g. "Production" or "Consumption",
#' or an actual demand vector with names as one or more model sectors and
#' numeric values in USD with the same dollar year as model.
#' @param location, str optional location code for demand vector, required for two-region models
#' @param use_domestic_requirements A logical value: if TRUE, use domestic demand and L_d matrix;
#' if FALSE, use complete demand and L matrix.
#' @export
#' @return A list with LCI and LCIA results (in data.frame format) of the EEIO model.
calculateEEIOModel <- function(model, perspective, demand = "Production", location = NULL, use_domestic_requirements = FALSE) {
  result <- list()
  # Generate Total Requirements (L or L_d) matrix based on whether "use_domestic"
  if (use_domestic_requirements) {
    L <- model$L_d
    M <- model$M_d
    N <- model$N_d
  } else {
    L <- model$L
    M <- model$M
    N <- model$N
  }
  
  # Prepare demand vector
  if (is.character(demand)) {
    #assume this is a model build-in demand 
    #try to load the model vector
    meta <- model$DemandVectors$meta
    if (demand %in% meta$Type) {
      demand_name <- ifelse(use_domestic_requirements,
                            paste0("Domestic", demand),
                            paste0("Complete", demand))
      # Get vector name (ID) from the meta table
      if(is.null(location)) {
        id <- meta[which(meta$Name==demand_name),"ID"]
        if(length(id)>1) {
          stop("Unique demand vector not found, consider passing location")
        }
      } else {
        id <- meta[which(meta$Name==demand_name &
                         meta$Location==location),"ID"]
      }
      d <- model$DemandVectors$vectors[[id]]
    } else {
      stop(paste0("'", demand, "' is not a valid demand vector name in model."))
    }
  } else {
    # Assume this is a user-defined demand vector
    #! Need to check that the given demand 
    if (isDemandVectorValid(demand,L)) {
      d <- formatDemandVector(demand,L)
    } else {
      stop("Format of the demand vector is invalid. Cannot calculate result.")
    }
  }
  # Convert demand vector into a matrix
  f <- as.matrix(d)  
  # Calculate LCI and LCIA in direct or final perspective
  if (perspective=="DIRECT") {
    # Calculate Direct Perspective LCI (a matrix with direct impacts in form of sector x flows)
    logging::loginfo("Calculating Direct Perspective LCI...")
    s <- getScalingVector(L, f)
    result$LCI_d <- calculateDirectPerspectiveLCI(model$B, s)
    # Calculate Direct Perspective LCIA (matrix with direct impacts in form of sector x impacts)
    logging::loginfo("Calculating Direct Perspective LCIA...")
    result$LCIA_d <- calculateDirectPerspectiveLCIA(model$D, s)
  } else if (perspective=="FINAL") {
    # Calculate Final Perspective LCI (a matrix with total impacts in form of sector x flows)
    logging::loginfo("Calculating Final Perspective LCI...")
    result$LCI_f <- calculateFinalPerspectiveLCI(M, f)
    # Calculate Final Perspective LCIA (matrix with total impacts in form of sector x impacts)
    logging::loginfo("Calculating Final Perspective LCIA...")
    result$LCIA_f <- calculateFinalPerspectiveLCIA(N, f)
  }
  
  logging::loginfo("Result calculation complete.")
  return(result)
}


#' Multiply the Leontief inverse L and the demand vector to calculate scaling vector
#' that represents production needed to fulfill the demand.
#' @param L Leontief inverse.
#' @param demand Final demand vector.
#' @return Scaling vector.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 8.
getScalingVector <- function(L, demand) {
  s <- L %*% demand
  return(s)
}

#' The direct perspective LCI aligns flows with sectors consumed by direct use.
#' Multiply the B matrix and the scaling vector s.
#' @param B Marginal impact per unit of the environmental flows.
#' @param s Scaling vector.
#' @return A matrix with direct impacts in form of sector x flows.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 9.
calculateDirectPerspectiveLCI <- function(B, s) {
  lci_d <- t(B %*% diag(as.vector(s), nrow(s)))
  rownames(lci_d) <- rownames(s)
  return(lci_d)
}

#' The final perspective LCI aligns flows with sectors consumed by final users.
#' Multiply the M matrix and the diagonal of demand, y.
#' @param M, a model M matrix, direct + indirect flows per $ output of sector.
#' @param y, a model demand vector
#' @return A matrix with total impacts in form of sectors x flows..
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 10.
calculateFinalPerspectiveLCI <- function(M, y) {
  lci_f <- t(M %*% diag(as.vector(y)))
  colnames(lci_f) <- rownames(M)
  rownames(lci_f) <- colnames(M)
  return(lci_f)
}

#' The direct perspective LCIA aligns impacts with sectors consumed by direct use.
#' Multiply the D matrix (the product of C matrix and B matrix) and scaling vector s.
#' @param D Direct impact per unit of the environmental flows.
#' @param s Scaling vector.
#' @return A matrix with direct impacts in form of sector x impact categories.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 9.
calculateDirectPerspectiveLCIA <- function(D, s) {
  lcia_d <- t(D %*% diag(as.vector(s), nrow(s)))
  rownames(lcia_d) <- rownames(s)
  return(lcia_d)
}

#' The final perspective LCIA aligns impacts with sectors consumed by final users.
#' Multiply the N matrix and the diagonal of demand, y.
#' @param N, a model N matrix, direct + indirect impact per unit of the environmental flows.
#' @param y, a model demand vector
#' @return A matrix with total impacts in form of sector x impact categories.
#' @references Yang, Yi, Wesley W. Ingwersen, Troy R. Hawkins, Michael Srocka, and David E. Meyer.
#' 2017. “USEEIO: A New and Transparent United States Environmentally-Extended Input-Output Model.”
#' Journal of Cleaner Production 158 (August): 308–18. https://doi.org/10.1016/j.jclepro.2017.04.150.
#' SI1, Equation 10.
calculateFinalPerspectiveLCIA <- function(N, y) {
  lcia_f <- t(N %*% diag(as.vector(y)))
  colnames(lcia_f) <- rownames(N)
  rownames(lcia_f) <- colnames(N)
  return(lcia_f)
}

#' Divide/Normalize a sector x flows matrix by the total of respective flow (column sum)
#' @param m A sector x flows matrix.
#' @return A normalized sector x flows matrix.
#' @export
normalizeResultMatrixByTotalImpacts <- function(m) {
  #Use sweep function to prevent error
  m_norm <- sweep(m, MARGIN = 2, FUN = "/", STATS = colSums(m))
  return(m_norm)
}

#' Dot multiplies two vectors to calculate an impact score and the percent contribution each score to the total
#' @param x, numeric vector of length n
#' @param y, numeric vector of length n
#' @return A dataframe sorted by contribution (high-to-low), also showing "x","y","impact" 
calculatePercentContributiontoImpact <- function (x,y) {
  df <- cbind.data.frame(x,y)
  df["impact"] <- df[,"x"]*df[,"y"] 
  df["contribution"] <- df["impact"]/sum(df["impact"])
  df <- df[order(df$contribution,decreasing=TRUE),]
  return(df) 
}

#' Calculate the percent contribution of sectors to direct+indirect impacts by an indicator,
#' using the product of model L matrix (total requirements) and D matrix (direct impacts by indicator).
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sector, str, index of a model sector for use in the L matrix, e.g. "221100/US".
#' @param indicator, str, index of a model indicator for use in the D matrix, e.g. "Acidification Potential".
#' @param domestic, boolean, sets model to use domestic flow matrix. Default is FALSE.
#' @return A dataframe sorted by contribution (high-to-low), also showing "L", "D", "impact".
#' @export
calculateSectorContributiontoImpact <- function (model, sector, indicator, domestic=FALSE) {
  L <- model$L
  if (domestic) {
    L <- model$L_d
  }
  D <- model$D
  df <- calculatePercentContributiontoImpact(L[,sector],D[indicator,])
  # Rename x and y cols
  colnames(df)[colnames(df)==c("x", "y")] <- c("L", "D")
  # Add sector name for easier interpretation of results 
  rownames(df) <- paste(rownames(df),
                        model$Commodities[match(rownames(df),
                                                model$Commodities$Code_Loc), "Name"],
                        sep = " - ")
  return(df)
}

#' Calculate the percent contribution of flows to direct+indirect impacts by an indicator,
#' using model M matrix for total impacts of flows and C matrix for indicator.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param sector, str, index of a model sector for use in the M matrix, e.g. "221100/US".
#' @param indicator, str, index of a model indicator for use in the C matrix, e.g. "Acidification Potential".
#' @param domestic, boolean, sets model to use domestic flow matrix. Default is FALSE.
#' @return A dataframe sorted by contribution (high-to-low), also showing "M", "C", "impact".
#' @export 
calculateFlowContributiontoImpact <- function (model, sector, indicator, domestic=FALSE) {
  M <- model$M
  C <- model$C
  if (domestic) {
    M <- model$M_d
  }
  df <- calculatePercentContributiontoImpact(M[,sector], C[indicator,])
  # Rename x and y cols
  colnames(df)[colnames(df)==c("x", "y")] <- c("M", "C")
  return(df)
}

#' Aggregate result matrix by rows
#'
#' @param matrix      A matrix with sectors as rows
#' @param to_level    The level of BEA code this matrix will be aggregated to
#' @param crosswalk   Sector crosswalk between levels of detail
#' @return An aggregated matrix with sectors as rows
#' @export
aggregateResultMatrixbyRow <- function (matrix, to_level, crosswalk) {
  # Determine the columns within MasterCrosswalk that will be used in aggregation
  from_code <- "USEEIO"
  to_code <- paste0("BEA_", to_level)
  # Remove location codes from sectors
  df <- as.data.frame(matrix)
  rownames(df) <- gsub("/.*","",rownames(df))
  matrix <- as.matrix(df)
  # Aggregate by rows
  value_columns <- colnames(matrix)
  df_fromlevel <- merge(matrix, unique(crosswalk[, c(from_code, to_code)]), by.x = 0, by.y = from_code)
  df_fromlevel_agg <- stats::aggregate(df_fromlevel[, value_columns], by = list(df_fromlevel[, to_code]), sum)
  rownames(df_fromlevel_agg) <- df_fromlevel_agg[, 1]
  df_fromlevel_agg[, 1] <- NULL
  matrix_fromlevel_agg <- as.matrix(df_fromlevel_agg)
  return(matrix_fromlevel_agg)
}

#' Aggregate result matrix by rows and columns
#'
#' @param matrix      A matrix with sectors as rows and columns
#' @param to_level    The level of BEA code this matrix will be aggregated to
#' @param crosswalk   Sector crosswalk between levels of detail
#' @return An aggregated matrix with sectors as rows and columns
#' @export
aggregateResultMatrix <- function (matrix, to_level, crosswalk) {
  row_agg_matrix <- aggregateResultMatrixbyRow (matrix, to_level, crosswalk)
  col_agg_matrix <- aggregateResultMatrixbyRow (t(row_agg_matrix), to_level, crosswalk)
  agg_matrix <- t(col_agg_matrix)
  return(agg_matrix)
}

#' Calculates sector x sector impacts from a given demand vector.
#' @param y a model demand vector or user-specified demand vector
#' that must have the same dimension with the model demand vector
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param indicator str, index of a model indicator, e.g. "Acidification Potential"
#' @return A matrix of impacts in the form of sector purchased x sector sourced,
#' negative values should be interpreted as "reduced impacts".
#' @export
calculateSectorPurchasedbySectorSourcedImpact <- function (y, model, indicator) {
  L <- model$L
  total_req <- L %*% diag(y)
  colnames(total_req) <- rownames(total_req)
  impacts <- t(diag(model$D[indicator,]) %*% total_req)
  colnames(impacts) <- rownames(impacts)
  return(impacts)
}

#' Calculate sector margin impacts in the form of M and N Matrix
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A list with M_margin and N_margin
#' @export
calculateMarginSectorImpacts <- function(model) {
  # Calculation fractions of producer price for each margin
  MarginCoefficients <- as.matrix(model$Margins[, c("Transportation", "Wholesale", "Retail")]/model$Margins[, c("ProducersValue")])
  rownames(MarginCoefficients) <- model$Margins$SectorCode
  MarginCoefficients[is.na(MarginCoefficients)] <- 0
  
  # Create margin_allocation matrix to allocate fractions by margin sector
  # In the matrix, rows are three margin types and columns are margin sectors
  margin_allocation <- matrix(nrow = 3, ncol = nrow(model$MarginSectors), 0)
  rownames(margin_allocation) <- colnames(MarginCoefficients)
  colnames(margin_allocation) <- model$MarginSectors$Code
  # Assign allocation factors to margin sectors based on total Commodity output
  output_ratio <- calculateOutputRatio(model, output_type="Commodity")
  for (i in rownames(margin_allocation)) {
    codes <- model$MarginSectors[model$MarginSectors$Name==i, "Code"]
    margin_allocation[i, codes] <- output_ratio[match(codes, output_ratio$SectorCode), "toSectorRatio"]
  }
  # Multiply fractions by allocation matrix to get a fraction per margin sector for each commodity
  margins_by_sector <- MarginCoefficients %*% margin_allocation
  
  # Put margins_by_sector into a matrix in the form of A
  A_margin <- model$A
  # Make sure sector ordering is the same
  A_margin[,] <- 0 
  for (i in model$MarginSectors$Code_Loc) {
    A_margin[i, ] <- margins_by_sector[gsub("/.*", "", colnames(A_margin)),
                                       gsub("/.*", "", i)]
  }
  # Multiply M and N by margins_by_sector to derive M_margin and N_margin
  ls <- list("M_margin" = model$M %*% A_margin,
             "N_margin" = model$N %*% A_margin)
  logging::loginfo("Model margin impacts calculated.")
  return(ls)
}

#' For a given impact, provided via indicator or elementary flow label, 
#' disaggregate the total impacts per purchase (indicator: N, flow: M) into 
#' direct impacts (indicator: D, flow: B) and upstream, Tier 1 purchase impacts. 
#' Return a long-format df of exchanges, with sector names mapped to sector codes.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param impact str, a model indicator (e.g., "Greenhouse Gases") row index of N, 
#'                    or elementary flow (e.g., "Methane/emission/air/kg") index of M
#' @param opt_impact str {'indicator', 'elemflow'}, string code to specify impact type
#' @export
#' @return A data frame of direct and per-tier-1-purchase sector impacts
disaggregateTotalToDirectAndTier1 <- function(model, impact, opt_impact="indicator") {
  mtx_direct <- c("indicator"="D", "elemflow"="B")[opt_impact]
  if (is.na(mtx_direct)) {
    stop(paste0("'",opt_impact,"' is not a valid opt_impact string code"))
  }
  # get direct sector impacts
  df_direct <- tryCatch({  # catches bad `impact` row label
      tibble::enframe(model[[mtx_direct]][impact,])
  }, error=function(e) {
      stop(paste0("'",impact,"' is not a valid ",opt_impact," label"))
  })
  df_direct <- dplyr::rename(df_direct, impact_per_purchase=value, sector_code=name) 
  # assign "Direct" as purchased commodity label for data-vis & stat convenience
  df_direct <- dplyr::mutate(df_direct, purchased_commodity = 'Direct')
  # get total impacts per Tier 1 purchase by sector
  df_total <- calculateTotalImpactbyTier1Purchases(model, impact, opt_impact) 
  df_total <- tibble::as_tibble(df_total, rownames="purchased_commodity_code") 
  df_total <- reshape2::melt(df_total, id.vars="purchased_commodity_code",
                                       variable.name="sector_code",
                                       value.name="impact_per_purchase") 
  # map sector codes to names
  sector_map <- setNames(model$Commodities$Name, model$Commodities$Code_Loc)
  df_total <- dplyr::mutate(df_total, 
    purchased_commodity = dplyr::recode(purchased_commodity_code, !!!sector_map))
  # concat direct + total impacts  
  df_impacts <- dplyr::bind_rows(df_total, df_direct) 
  df_impacts <- dplyr::mutate(df_impacts, 
    sector = dplyr::recode(sector_code, !!!sector_map))
  return(df_impacts)
}

#' Calculate sector x sector total impacts (single indicator or elementary flow) 
#' for Tier 1 purchases. Multiply each row of the sector by sector A matrix by 
#' the scalar elements of a single-impact by sector array (indicator: N, flow: M)
#' @param model A complete EEIO model: a list with USEEIO model components and attributes
#' @param impact str, a model indicator (e.g., "Greenhouse Gases") row index of N, 
#'                    or elementary flow (e.g., "Methane/emission/air/kg") index of M
#' @param opt_impact str {'indicator', 'elemflow'}, string code to specify impact type
#' @return A sector by sector, impact-per-tier-1-purchase matrix.
calculateTotalImpactbyTier1Purchases <- function(model, impact, opt_impact='indicator') {
  mtx_total <- c("indicator"="N", "elemflow"="M")[opt_impact]
  if (is.na(mtx_total)) {
    stop(paste0("'",opt_impact,"' is not a valid opt_impact string code"))
  }
  df_direct <- tryCatch({  # catches bad `impact` row label
    totalImpactPerPurchase <- model[[mtx_total]][impact,] * model$A
    # totalImpactPerPurchase <- model$N[impact,] * model[["A"]]
  }, error=function(e) {
    stop(paste0("'",impact,"' is not a valid ",opt_impact," label"))
  })
  return(totalImpactPerPurchase)
}
