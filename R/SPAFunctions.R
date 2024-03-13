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

  # List objects that are needed to Run SPA in Matlab  
  A <- model$A
  L <- model$L
  EIOsecnames <- model$Commodities$Name
  EIOsecs <- model$Commodities$Code
  EIvect <- model$C %*% model$B
  indicatorNames <- rownames(model$C)
  flowNames <- rownames(model$B)
  y <- as.matrix(model$DemandVectors$vectors$`2012_US_Consumption_Household`) # Get Household consumption vector
  y[which(y < 0)] <-0 # Remove negative inputs into household consumption
  
  # Format filename
  filename <- paste(filename, ".mat", sep = "")
  # Write as a .mat file
  writeMat(filename, 
           A = A,
           L = L,
           EIOsecnames = EIOsecnames, 
           EIOsecs = EIOsecs,
           EIvect = EIvect,
           indicatorNames = indicatorNames,
           flowNames = flowNames,
           y = y
           )

}


#' Import model objects for use in matlab SPA function
#' Requires R.matlab library
#' @description Export model objects for use in matlab SPA function
#' @param filename string indicating the filename to import from. Note that it must include the complete relative path as a part of the filename
#' @return A populated data.tree object
importModelObjectsForMatlab <- function(filename) {
  library(R.matlab)  # load matlab library
  
  matlab_data <- readMat(filename)
  
  # # TODO: Move the below code to a new function
  # 
  # library(networkD3)
  # library(data.tree)
  # library(DiagrammeR)
  # 
  # matlab_data <- readMat(filename) # Need to read in sorted data
  # sorted_values <- matlab_data$sorted[seq(1, length(matlab_data$sorted), 2)] # Get every other row starting at row 1
  # sorted_nodes <- matlab_data$sorted[seq(2, length(matlab_data$sorted), 2)] # Get every other row starting at row 2
  # 
  # # Extract all nodes used in the SPA output
  # D3_nodes_list <- unique(unlist(sorted_nodes))
  # D3_nodes_list <- data.frame(D3_nodes_list) # Convert to data frame
  # colnames(D3_nodes_list) <- c("name") # relabel column name with "names" string to identify these as the names of the nodes for the D3 functions
  # 
  # # Get nodes into a data frame
  # # The line bewlow returns a dataframe with dimensions length(sorted_nodes) by max tier (i.e. longest subtree)
  # D3_nodes_df <- t(sapply(sorted_nodes, '[', seq(max(lengths(sorted_nodes))))) 
  # pathstring <- apply(D3_nodes_df, 1, paste, collapse = "/") # create a pathstring
  # pathstring <- sub("\\/NA.*","",pathstring) # remove all the /NA's from the pathstring
  # 
  
  #TODO: move the below to a new function, read from csv or something
  library(R.matlab)
  library(networkD3)
  library(data.tree)
  library(DiagrammeR)
  
  filename <- "../useeior/work/SPA_testing/SPA_results/255_paints/SPA_result_255.csv" #temporary
  csv_data <- read.csv(filename) # Filename must have complete path and file name, e.g., ../useeior/work/SPA_testing/SPA_results/255_paints/SPA_result_255.csv
  colnames(csv_data) <- csv_data[1,]
  spa <- csv_data[-1,]
  
  # Name columns 
  pathNumberCol <- 1
  pathLengthCol <- 2
  siteEffectCol <- 3
  LCIEffectCol <- 4
  indexCols <- seq(5, length(spa), 2) # columns that contain the indeces of the useeio sector, which make up the paths
  nameCols <- seq(6, length(spa), 2) # columns that contain the names of the useeio sector, which make up the paths
  
  numbericCols <- c(pathNumberCol, pathLengthCol, siteEffectCol, LCIEffectCol, indexCols)
  T_max <- length(indexCols) # Number of tiers
  
  pathIndexes <- spa[,indexCols] # Get every other column, starting at column 5. These are the columns that contain the codes for each path
  pathNames <- spa[,nameCols] # Get every other column starting at col 6. These are the cols that contain the names for each path

  sorted_spa <- spa[ order( spa[,indexCols[T_max] ], decreasing = TRUE), ] # sort spa dataframe by the spa df column at indexCols[T_max], i.e., the spa df with the last tier values
  sorted_pathIndexes <- pathIndexes[ order( pathIndexes[,T_max], decreasing = TRUE), ] # sort Path Indexes by the last tier, i.e., T_max
  
  # For testing visualization functions: Get only those rows of the spa with values at the max tier, 
  # or more specifically, keep those rows where values at the max tier col are not equal to ""
  # NOTE THAT THESE ARE NOT ENOUGH TO DESCRIBE THE COMPLETE TREE
  # TODO: NEED TO ADD CODE TO FIND LEAFS FROM TIERS THAT ARE NOT AT T_MAX
  T_max_leafs <- sorted_spa[which(sorted_spa[,indexCols[T_max]] != ""),]
  pathstring <- apply(T_max_leafs[,indexCols], 1, paste, collapse = "/") # create a pathstring
  T_max_leafs$pathString <- pathstring
  T_max_leafs_index_only <- T_max_leafs[,-c(pathNumberCol, pathLengthCol, nameCols)]
  
  # Create tree
  spa_tree <- as.Node(T_max_leafs_index_only)
  # Create network for networkD3 plotting
  spa_network <- ToDataFrameNetwork(spa_tree, "name")
  # simpleNetwork(spa_network[-3], fontSize = 12) # create D3 network plot. The -3 removes the name column from dataframe for plotting purposes
  
  # Plot as dendogram
  plot(as.dendrogram(spa_tree), center = TRUE)
  
  # Plot as radial network
  useRtreeList <- ToListExplicit(spa_tree, unname = TRUE)
  radialNetwork( useRtreeList)
}



 # The code below is commented out as the functions are not a complete implementation of SPA yet. 
 # They will be left commnented out while they are still in development 
#' #' Wrapper function to run SPA with useeior model object
#' #' @description Run a SPA based on given paramters
#' #' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' #' @param y Final demand value for which SPA will be run
#' #' @param percent Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' #' @param T_max Max number of tiers to search
#' #' @param indicator Indicator to return indicator results for ($, CO2, etc.)
#' #' @return A completed SPA in a data.tree object
#' runSPA <- function(model, y = NULL, percent = 0.01, T_max = 4, indicator = "Economic"){
#'   
#'   library(data.tree)
#'   library(DiagrammeR)
#'   
#'   #TODO: add if checks for model types where SPA is not compatible, at least initially
#'   
#'   # Use CompleteConsumption final demand vector if no vector is provided
#'   if(is.null(y)){
#'     meta <- model$DemandVectors$meta
#'     demand_name <- "CompleteConsumption"
#'     location <- model$spec$ModelRegionAcronyms
#'     id <- meta[which(meta$Name==demand_name & 
#'                        meta$Location==location),"ID"]
#'     y <- model$DemandVectors$vectors[[id]]
#'     y <- as.matrix(y) 
#'   }
#'   
#'   if(T_max > 6){
#'     T_max <- 6
#'     logging::loginfo("Setting max number of tiers at 6 due to memory constraints")
#'   }
#'   
#'   if(percent < 0.01){
#'     percent <- 0.01
#'     logging::loginfo("Setting cut-off at 1% of total value due to memory constraints")
#'   }
#'   
#'   # Define matrix on which to run SPA
#'   if(indicator == "Economic"){
#'     
#'     f <- c(model$L[,1])
#'     #Set all values to 1
#'     f[0:nrow(model$L)] <- 1 # want a vector of 1s to serve as an identity matrix eventually
#' 
#'     f_total <- f %*% model$L 
#'     
#'   }else{
#'     
#'     emissions <- model$C %*% model$B
#'     indicatorNames <- rownames(emissions)
#'     indicator_index <- which(indicatorNames %in% indicator) #get the relevant index based on name
#' 
#'     
#'     if(length(indicator_index) >0){
#'       f <- emissions[indicator_index,, drop = FALSE] # get relevant row from N
#'     } else{
#'       stop("Incorrect indicator name, please revise.")
#'     }
#'     f_total <- f %*% model$L # should be the same as model$N
#'     #all.equal(f_total, model$N[indicator_index,, drop=FALSE])
#'   }
#'   
#' 
#'   A <- model$A # To avoid passing the entire model object during recursion.
#' 
#'   #LEFT OFF: I think f and f_total are correct for both economic and non-economic SPAs. Next step: implement tree functions with economic SPA as example.
#'       
#'   # SPA_result <- SPAEIO(F, A, y, F_total, T_max, percent, filename, sectornames, thresh_banner) # parameters for matlab 
#'   
#'   # # equivalent parameters for useeior: 
#'   # # A is a part of model; 
#'   # # don't need filename or sectornames here, can call another function after to print;
#'   # # thresh_banner is a flag to print parameters in the output file, again don't need that here right now
#'   # SPA_result <- SPAEIO(f, A, y, f_total, T_max, percent) 
#' 
#'   return(0)
#'   
#' }
#' 
#' 
#' 
#' #' @description Begin SPA based on given parameters
#' #' @param f A vector of direct emissions intensities by sector. Used to calculate individual node contributions.
#' #' @param A The direct requirements matrix from the useeior model object
#' #' @param y Final demand value for which SPA will be run
#' #' @param f_total a vector total emissions intensities by sector. Used to calculate total emissions.
#' #' @param T_max Max number of tiers to search
#' #' @param percent Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' #' @return A completed SPA in a data.tree object
#' SPAEIO <- function(f, A, y, f_total, T_max, percent){ 
#' 
#'   # Calculate the total emissions and tolerance
#'   Total_Emissions = f_total*y;
#'   tol = percent/100 * Total_Emissions;
#'   
#'   logging::loginfo(paste0("Tolerance set at ", tol))
#'   logging::loginfo("Bulding tree...")
#'   tree <- build_tree(f, A, y, f_total, T_max, tol);
#'   
#'   
#' }
#' 
#' 
#' #' @description Initialize SPA tree
#' #' @param f A vector of direct emissions intensities by sector. Used to calculate individual node contributions.
#' #' @param A The direct requirements matrix from the useeior model object 
#' #' @param y Final demand value for which SPA will be run
#' #' @param f_total a vector total emissions intensities by sector. Used to calculate total emissions.
#' #' @param T_max Max number of tiers to search
#' #' @param tol Tolerance, i.e., value to determine point at which to cut-off small sub-trees (percent of total effects)
#' #' @return A completed SPA in a data.tree object
#' build_tree <- function(f, A, y, f_total, T_max, tol){ #TODO: Rename this to initialize_tree
#'   
#'   t <- 0 # tier 0
#'   # Define root of tree
#'   t0_node <- Node$new("T0")
#'   t0_node$value_at_node <- build_tree_value(f, A, y, t0_node, t) #value of the node, i.e., contribution from node #TODO ADD THIS FUNCTION
#'   t0_node$sub_tree_value <- calculate_tree_vector(f, A, y, f_total, t0_node, t) # Value of the tree below this node #TODO add this function
#'   t0_node$parent <- null #a root node has no parent node
#'   
#'   #tree_to_build = Build_tree_func(F, A, y, F_total, 0, T_max, tol, uint16([]));
#'   t0_node$children <- build_tree_func(f, A, y, f_total, t, T_max, tol, t0_node); #for useeior
#' }
#' 
#' 
#' #' @description Build SPA tree nodes recursively
#' #' @param f A vector of direct emissions intensities by sector. Used to calculate individual node contributions.
#' #' @param A The direct requirements matrix from the useeior model object 
#' #' @param y Final demand value for which SPA will be run
#' #' @param f_total a vector total emissions intensities by sector. Used to calculate total emissions.
#' #' @param t current tier of tree
#' #' @param T_max Max number of tiers to search
#' #' @param tol Value to determine point at which to cut-off small sub-trees (percent of total effects)
#' #' @param parent Parent node
#' #' @return A completed SPA in a data.tree object
#' build_tree_func <- function(f, A, y, f_total, t, T_max, tol, parent){ 
#' 
#'   N <- dim(A)[1] # Number of potential children
#'   
#'   # Initialize current node
#'   currentNode <- Node$new("?") # TODO: need to figure out how to name the nodes
#'   
#'   if(t == T_max){ # At the last tier, i.e., max tree depth
#'     # TODO: Build final node of this path
#'     
#'   }else{ # Not at the last tier
#'     # Add a subtree
#'     for(i in 1:length(N)){
#'       
#'       child <- node$AddChild(rownames(A)[i])
#'       
#'       if(abs(calculate_tree_vector(f, A, y, f_total, child, t+1)) < tol){ #this is a slow line
#'        # if the value of the tree below this node is less that the tolerance, do not continue this path 
#'         #currentNode$child[i] <- null #TODO FIGURE OUT HOW TO CUT OFF TREE
#'       } else{
#'         #currentNode$child[i] <- build_tree_func(f, A, y, f_total, t+1, child)
#'       }
#'      } #end for loop
#'     
#'     # TODO: FIGURE THIS PART OUT TOO
#'     # if numel(sequence)>0 %number of array elements of sequence >0
#'     # this_tree.value(2) = Calculate_tree_vector(F, A, y, F_total,sequence, T);
#'     # else
#'     #   end
#'     
#'     
#'     
#'   } 
#'   
#'   
#' }


