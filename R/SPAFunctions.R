# Functions used for Structural Path Analysis

#' Function to test creating and populating data.tree objects
#' Example from https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
#' @description Create and populate data.tree objects
#' @return A populated data.tree object
testDataTrees <- function() {
  
  #library(data.tree)
  #library(DiagrammeR)
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
runSPA <- function(model, y, cut_off = 0.01, T_max, indicator = "Economic"){
  
  # SPA_result <- SPAEIO(F, A, y, F_total, T_max, percent, filename, sectornames, thresh_banner)
  
  return(SPA_result)
  
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
