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