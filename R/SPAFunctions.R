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


#' Export model objects for use in matlab SPA function
#' Requires R.matlab library
#' @description Export model objects for use in matlab SPA function
#' @param model useeio model object 
#' @param filename string indicating the filename to export to
#' @return A populated data.tree object
exportModelObjectsForMatlab <- function(model, filename = "matlab_data") {

  library(R.matlab)  # load matlab library
  data <- list()
  
  A <- model$A
  L <- model$L
  EIOsecnames <- model$Commodities$Name
  EIOsecs <- model$Commodities$Code
  final_ConsumptionComplete <- as.matrix(model$DemandVectors$vectors$`2012_US_Consumption_Household`)
  EIvect <- model$C %*% model$B
  indicatorNames <- rownames(model$C)
  flowNames <- rownames(model$B)
  
  
  filename <- paste(filename, ".mat", sep = "")
  writeMat(filename, 
           A = A,
           L = L,
           EIOsecnames = EIOsecnames, 
           EIOsecs = EIOsecs,
           y = final_ConsumptionComplete,
           EIvect = EIvect,
           indicatorNames = indicatorNames,
           flowNames = flowNames
           )
  

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
  
  if(T_max > 6){
    T_max <- 6
    logging::loginfo("Setting max number of tiers at 6 due to memory constraints")
  }
  
  if(percent < 0.01){
    percent <- 0.01
    logging::loginfo("Setting cut-off at 1% of total value due to memory constraints")
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
#' @param tol Tolerance, i.e., value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @return A completed SPA in a data.tree object
build_tree <- function(f, A, y, f_total, T_max, tol){ #TODO: Rename this to initialize_tree
  
  t <- 0 # tier 0
  # Define root of tree
  t0_node <- Node$new("T0")
  t0_node$value_at_node <- build_tree_value(f, A, y, t0_node, t) #value of the node, i.e., contribution from node #TODO ADD THIS FUNCTION
  t0_node$sub_tree_value <- calculate_tree_vector(f, A, y, f_total, t0_node, t) # Value of the tree below this node #TODO add this function
  t0_node$parent <- null #a root node has no parent node
  
  #tree_to_build = Build_tree_func(F, A, y, F_total, 0, T_max, tol, uint16([]));
  t0_node$children <- build_tree_func(f, A, y, f_total, t, T_max, tol, t0_node); #for useeior
}


#' @description Build SPA tree nodes recursively
#' @param f A vector of direct emissions intensities by sector. Used to calculate individual node contributions.
#' @param A The direct requirements matrix from the useeior model object 
#' @param y Final demand value for which SPA will be run
#' @param f_total a vector total emissions intensities by sector. Used to calculate total emissions.
#' @param t current tier of tree
#' @param T_max Max number of tiers to search
#' @param tol Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @param parent Parent node
#' @return A completed SPA in a data.tree object
build_tree_func <- function(f, A, y, f_total, t, T_max, tol, parent){ 

  N <- dim(A)[1] # Number of potential children
  
  # Initialize current node
  currentNode <- Node$new("?") # TODO: need to figure out how to name the nodes
  
  if(t == T_max){ # At the last tier, i.e., max tree depth
    # TODO: Build final node of this path
    
  }else{ # Not at the last tier
    # Add a subtree
    for(i in 1:length(N)){
      
      child <- node$AddChild(rownames(A)[i])
      
      if(abs(calculate_tree_vector(f, A, y, f_total, child, t+1)) < tol){ #this is a slow line
       # if the value of the tree below this node is less that the tolerance, do not continue this path 
        #currentNode$child[i] <- null #TODO FIGURE OUT HOW TO CUT OFF TREE
      } else{
        #currentNode$child[i] <- build_tree_func(f, A, y, f_total, t+1, child)
      }
     } #end for loop
    
    # TODO: FIGURE THIS PART OUT TOO
    # if numel(sequence)>0 %number of array elements of sequence >0
    # this_tree.value(2) = Calculate_tree_vector(F, A, y, F_total,sequence, T);
    # else
    #   end
    
    
    
  } 
  
  
}


