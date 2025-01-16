# Function to get backward or forward economic linkages from a USEEIO model,
# Provides results based on input demand, sorted by total (direct+indirect)
# Backward linkages use A and L matrices
# Forward linkages use Ghosh counterparts to A and L
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @param demand demand vector
#' @param type "backward" linkages use A and L matrices "forward" linkages use Ghosh counterparts to A and L
#' @param use_domestic_requirements A logical value: if TRUE, use domestic demand and L_d matrix;
#' if FALSE, use complete demand and L matrix.
#' @param location str optional location code for demand vector, required for two-region models
#' @param cutoff numeric value, shows the cutoff value for sorted results. Values smaller than cutoff are not shown

getSectorLinkages <- function(model, demand, type="backward", location = NULL, use_domestic_requirements=FALSE,cutoff=0.01) {
  f <- prepareDemandVectorForStandardResults(model, demand, location, use_domestic_requirements)
  if(type=="backward") {
    if (use_domestic_requirements) {
      tier1 <- model$A_d %*%f 
      all <- model$L_d %*%f  
    }  else {
      tier1 <- model$A%*%f 
      all <- model$L%*%f   
    }
  } else if(type=="forward") {
    Ghosh_B <- calculateGhoshB(model,use_domestic_requirements)
    Ghosh_G <- calculateGhoshG(Ghosh_B)
    tier1 <- Ghosh_B%*%f 
    all <- Ghosh_G%*%f  
  }
  tier2andbeyond <- all-tier1
  
  # Remove the additional "1" from the Leontief or Ghosh to avoid double counting indirect 
  tier2andbeyond <- tier2andbeyond-f
  total <- tier1 + tier2andbeyond
  
  linkages <- data.frame(direct=tier1,indirect=tier2andbeyond,total=total)
  linkages$Name <- model$Commodities[match(rownames(linkages),
                                           model$Commodities$Code_Loc),"Name"]
  # Filter by share > 1%
  linkages <- linkages[linkages$total > cutoff, ]
  # Sort them
  linkages <- linkages[order(linkages$total, decreasing = TRUE),]
  return(linkages)
}

#function to get supply side Ghosh matrix B
#' @param model An EEIO model object with model specs, IO tables, satellite tables, and indicators loaded
#' @param use_domestic_requirements A logical value: if TRUE, use domestic demand and L_d matrix;
#' if FALSE, use complete demand and L matrix.

calculateGhoshB <- function(model,use_domestic_requirements=FALSE) {
  x <- model$q
  if (use_domestic_requirements) {
    A <- model$A
  } else {
    A <- model$A_d
  }
  
  B <- solve(diag(x)) %*% A %*% diag(x)
  row.names(B) <- model$Commodities$Code_Loc
  colnames(B) <- model$Commodities$Code_Loc
  return(B)
}

# function to calculate inverse of Ghosh matrix B, returns the inverse G
#' @param B Ghosh matrix B
calculateGhoshG <- function(GhoshB) {
  G <- useeior:::calculateLeontiefInverse(GhoshB)
  return(G)
}
