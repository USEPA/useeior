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
#' @param cut_off Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @param T_max Max number of tiers to search
#' @param indicator Indicator to return indicator results for ($, CO2, etc.)
#' @return A completed SPA in a data.tree object
runSPA <- function(model, y = NULL, cut_off = 0.01, T_max = 4, indicator = "Economic"){
  
  
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
  
  # # Define matrix on which to run SPA
  # if(indicator == "Economic"){
  #   # Run SPA on economic throughput
  #   total_emissions <- y
  #   F_total <- model$L
  # } else {
  #   # Run SPA on emissions related to a specific indicator
  #   result_emissions <- calculateEEIOModel(model, perspective="DIRECT", demand = "Consumption")
  #   result_emissions <- result_emissions$LCIA_d # Get LCIA result
  # 
  #   indicator_index <- which(colnames(result_emissions) %in% indicator) #get the relevant emissions vector
  # 
  #   if(length(indicator_index) >0){
  #     total_emissions <- result_emissions[,indicator_index, drop = FALSE]
  #   } else{
  #     stop("Incorrect indicator name, please revise.")
  #   }
  # 
  # }
  
  # Define matrix on which to run SPA
  if(indicator == "Economic"){
    
    f <- c(model$L[,1])
    #Set all values to 1
    f[0:nrow(model$L)] <- 1
    
#    f <- model$C[1,, drop = FALSE] # Create a named vector using the first row of C
#    f[,0:ncol(model$C)] <- 1 # Set all values to 1, which is the matrix equivalent of multiplying by 1
    f_total <- f2 %*% model$L 
    
  }else{
    
    indicatorNames <- rownames(model$C)
    indicator_index <- which(indicatorNames %in% indicator) #get the relevant index based on name
    
    if(length(indicator_index) >0){
      f <- model$C[indicator_index,, drop = FALSE] # get relevant row from C
    } else{
      stop("Incorrect indicator name, please revise.")
    }
    f_total <- f %*% model$M
  }
  


  #LEFT OFF: I think f and f_total are correct for both economic and non-economic SPAs. Next step: implement tree functions with economic SPA as example.
      
  # SPA_result <- SPAEIO(model, F, A, y, F_total, T_max, percent, filename, sectornames, thresh_banner)
  
  return(0)
  
}



#' @description Begin SPA based on given parameters
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @param y Final demand value for which SPA will be run
#' @param cut_off Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' @param T_max Max number of tiers to search
#' @param indicator Indicator to return indicator results for ($, CO2, etc.)
#' @return A completed SPA in a data.tree object
SPAEIO <- function(model, y, cut_off = 0.01, T_max, indicator = "Economic"){ #NEED TO MODIFY PARAMETERS, RIGHT NOW IT'S A COPY OF runSPA
 
  
}
