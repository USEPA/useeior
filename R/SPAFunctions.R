# Functions used for Structural Path Analysis

#' Function to test creating and populating data.tree objects
#' Example from https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
#' @description Create and populate data.tree objects
#' @return A populated data.tree object
testDataTrees <- function() {
  
  library(data.tree)
  library(DiagrammeR)
  acme <- Node$new("Acme Inc.")
  accounting <- acme$AddChild("Accounting")
  software <- accounting$AddChild("New Software")
  standards <- accounting$AddChild("New Accounting Standards")
  research <- acme$AddChild("Research")
  newProductLine <- research$AddChild("New Product Line")
  newLabs <- research$AddChild("New Labs")
  it <- acme$AddChild("IT")
  outsource <- it$AddChild("Outsource")
  agile <- it$AddChild("Go agile")
  goToR <- it$AddChild("Switch to R")
  
  print(acme)
  return(acme)
}



#' Wrapper function to run SPA with useeior model object
#' @description Run a SPA based on given paramters
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param y Final demand value for which SPA will be run
#' @param percent Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @param T_max Max number of tiers to search
#' @param indicator Indicator to return indicator results for ($, CO2, etc.)
#' @return A completed SPA in a data.tree object
runSPA <- function(model, y = NULL, percent = 0.01, T_max = 4, indicator = "Economic"){
  
  library(data.tree)
  library(DiagrammeR)
  
  #TODO: add if checks for model types where SPA is not compatible, at least initially
  
  # Use CompleteConsumption final demand vector if no vector is provided
  if(is.null(y)){
    meta <- model$DemandVectors$meta
    demand_name <- "CompleteConsumption"
    location <- model$spec$ModelRegionAcronyms
    id <- meta[which(meta$Name==demand_name & 
                       meta$Location==location),"ID"]
    y <- model$DemandVectors$vectors[[id]]
    y <- as.matrix(y) 
  }
  
  # Define matrix on which to run SPA
  if(indicator == "Economic"){
    
    f <- c(model$L[,1])
    #Set all values to 1
    f[0:nrow(model$L)] <- 1 # want a vector of 1s to serve as an identity matrix eventually

    f_total <- f %*% model$L 
    
  }else{
    
    emissions <- model$C %*% model$B
    indicatorNames <- rownames(emissions)
    indicator_index <- which(indicatorNames %in% indicator) #get the relevant index based on name

    
    if(length(indicator_index) >0){
      f <- emissions[indicator_index,, drop = FALSE] # get relevant row from N
    } else{
      stop("Incorrect indicator name, please revise.")
    }
    f_total <- f %*% model$L # should be the same as model$N
    #all.equal(f_total, model$N[indicator_index,, drop=FALSE])
  }
  

  A <- model$A # To avoid passing the entire model object during recursion.

  #LEFT OFF: I think f and f_total are correct for both economic and non-economic SPAs. Next step: implement tree functions with economic SPA as example.
      
  # SPA_result <- SPAEIO(F, A, y, F_total, T_max, percent, filename, sectornames, thresh_banner) # parameters for matlab 
  
  # # equivalent parameters for useeior: 
  # # A is a part of model; 
  # # don't need filename or sectornames here, can call another function after to print;
  # # thresh_banner is a flag to print parameters in the output file, again don't need that here right now
  # SPA_result <- SPAEIO(f, A, y, f_total, T_max, percent) 

  return(0)
  
}



#' @description Begin SPA based on given parameters
#' @param f A vector of direct emissions intensities by sector. Used to calculate individual node contributions.
#' @param A The direct requirements matrix from the useeior model object
#' @param y Final demand value for which SPA will be run
#' @param f_total a vector total emissions intensities by sector. Used to calculate total emissions.
#' @param T_max Max number of tiers to search
#' @param percent Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @return A completed SPA in a data.tree object
SPAEIO <- function(f, A, y, f_total, T_max, percent){ 

  # Calculate the total emissions and tolerance
  Total_Emissions = f_total*y;
  tol = percent/100 * Total_Emissions;
  
  logging::loginfo(paste0("Tolerance set at ", tol))
  logging::loginfo("Bulding tree...")
  tree <- build_tree(f, A, y, f_total, T_max, tol);
  
  
}


#' @description Initialize SPA tree
#' @param f A vector of direct emissions intensities by sector. Used to calculate individual node contributions.
#' @param A The direct requirements matrix from the useeior model object 
#' @param y Final demand value for which SPA will be run
#' @param f_total a vector total emissions intensities by sector. Used to calculate total emissions.
#' @param T_max Max number of tiers to search
#' @param percent Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @return A completed SPA in a data.tree object
build_tree <- function(f, A, y, f_total, T_max, percent){ #TODO: Rename this to initialize_tree
  
  t <- 0 # tier 0
  # define root of tree
  t0_node <- Node$new("T0")
  t0_node$value_at_node <- build_tree_value(f, A, y, t) #value of the node, i.e., contribution from node
  t0_node$parent <- null #a root node has no parent node
  
  #tree_to_build = Build_tree_func(F, A, y, F_total, 0, T_max, tol, uint16([]));
  #tree_to_build = Build_tree_func(f, A, y, f_total, t, T_max, tol, node); #for useeior
}


