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


example_sankey <- function(){
  
  
  library(networkD3)
  sankey_data <- list()

  # Initialize node
  nodes = data.frame("name" = 
                       c("Node A", # Node 0
                         "Node B", # Node 1
                         "Node C", # Node 2
                         "Node D"))# Node 3
  # Initialize links
  links = as.data.frame(matrix(c(
    0, 1, 10, # Each row represents a link. The first number
    0, 2, 20, # represents the node being conntected from. 
    1, 3, 30, # the second number represents the node connected to.
    2, 3, 40),# The third number is the value of the node
    byrow = TRUE, ncol = 3))
  names(links) = c("source", "target", "value")
  
  # Plot example sankey
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30)
  
  sankey_data$nodes <- nodes
  sankey_data$links <- links
  
  return(sankey_data)
  
  
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

# TODO: finish properly formatting D3_nodes_df to look like the output of Import/Format SPAFromCSV function
#' #' Import SPA from MATLAB files
#' #' Requires R.matlab library
#' #' @description Import SPA from MATLAB files
#' #' @param filename string indicating the filename to import from. Note that it must include the complete relative path as a part of the filename
#' #' @return A DF formatted to create a data.tree object
#' importSPAFromMatlab <- function(filename) {
#'   library(R.matlab)  # load matlab library
#'   library(networkD3)
#'   library(data.tree)
#'   library(DiagrammeR)
#' 
#'   # matlab_data <- readMat("../useeior/work/SPA_testing/SPA_results/255_paints/sorted_255.mat") # temporary
#'   matlab_data <- readMat(filename) # Need to read in sorted data
#'   sorted_values <- matlab_data$sorted[seq(1, length(matlab_data$sorted), 2)] # Get every other row starting at row 1
#'   sorted_nodes <- matlab_data$sorted[seq(2, length(matlab_data$sorted), 2)] # Get every other row starting at row 2
#' 
#'   # Extract all nodes used in the SPA output
#'   D3_nodes_list <- unique(unlist(sorted_nodes))
#'   D3_nodes_list <- data.frame(D3_nodes_list) # Convert to data frame
#'   colnames(D3_nodes_list) <- c("name") # relabel column name with "names" string to identify these as the names of the nodes for the D3 functions
#' 
#'   # Get nodes into a data frame
#'   # The line below returns a dataframe with dimensions length(sorted_nodes) by max tier (i.e. longest subtree)
#'   D3_nodes_df <- t(sapply(sorted_nodes, '[', seq(max(lengths(sorted_nodes)))))
#'   pathString <- apply(D3_nodes_df, 1, paste, collapse = "/") # create a pathString
#'   pathString <- sub("\\/NA.*","",pathString) # remove all the /NA's from the pathString
#'   D3_nodes_df[is.na(D3_nodes_df)] <- "" # Convert NAs to ""
#'   D3_nodes_df <- data.frame(D3_nodes_df) # conver to df again
#'   D3_nodes_df$pathString <- pathString # add pathstring 
#'   
#'   return(D3_nodes_df)
#'   
#'   #TODO: Add Site and LCI effect values (i.e., data in sorted_values object) to the D3_nodes_df dataframe
#'   
#'   # TODO: Code below for testing need to remove
#'   # # Create tree
#'   # spa_tree <- as.Node(D3_nodes_df)
#'   # # Create network for networkD3 plotting
#'   # spa_network <- ToDataFrameNetwork(spa_tree, "name")
#'   # # simpleNetwork(spa_network[-3], fontSize = 12) # create D3 network plot. The -3 removes the name column from dataframe for plotting purposes
#'   # return(spa_tree)
#' 
#' }


#' Import SPA from CSV file. Note that is SPA produced from MATLAB script
#' @description Import SPA from CSV file. SPA produced from MATLAB script
#' @param filename string indicating the filename to import from. Note that it must include the complete relative path as a part of the filename
#' @return A DF formatted to create a data.tree object
importSPAFromCSV <- function(filename) {

#  filename <- "../useeior/work/SPA_testing/SPA_results/255_paints/SPA_result_255.csv" #temporary
  csv_data <- read.csv(filename) # Filename must have complete path and file name, e.g., ../useeior/work/SPA_testing/SPA_results/255_paints/SPA_result_255.csv

  
  # TODO: Move to a new function: Format SPA DF from CSV 
  colnames(csv_data) <- csv_data[1,]
  spa <- csv_data[-1,]
  
  # Name columns 
  pathNumberCol <- 1
  pathLengthCol <- 2
  siteEffectCol <- 3
  LCIEffectCol <- 4
  indexCols <- seq(5, length(spa), 2) # columns that contain the indeces of the useeio sector, which make up the paths
  nameCols <- seq(6, length(spa), 2) # columns that contain the names of the useeio sector, which make up the paths
  
  
  pathString <- apply(spa[,indexCols], 1, paste, collapse = "/") # create a pathString by concatenating all the index cols for all rows
  pathString <- sub("(\\d)[^0-9]+$", "\\1", pathString) # remove all the "/" that appear after the last number
  spa_node_data <- spa[,-c(pathNumberCol, pathLengthCol, nameCols)] # Remove the Path and Name columns
  spa_node_data$pathString <- pathString
  
   return(spa_node_data)
  # END of FORMAT SPA DF from CSV Function
  
}


#' Plot SPA 
#' @description Plot the SPA according to parameter inputs
#' @param spa_node_data DataFrame containing the SPA in the format required for plotting using the data.tree package
#' @param subtree_to_plot String denoting whether to plot the entire tree (if NULL) or a specific subtree. Options are:
#' "top_LCI" to plot the subtree with the highest LCI effect (i.e., subtree with highest cumulative effect)
#' "top_Site" to plot the subtree containing the highest site effect (i.e., subtree containing the node with the highest individual contribution)
#' "index" to plot all subtrees that contain that specific with a specific index (i.e., specific node). Note that the value for this input is not "index" but the index number as a string, e.g., "244" for index 244.
#' @param plot_type String denoting the type of plot to use. Default to simpleNetwork plot. Options:
#' simpleNetwork, dendogram, radial
plotSPA <- function(spa_node_data, subtree_to_plot = NULL, plot_type = "simpleNetwork") {
  
  # Load required libraries
  library(networkD3)
  library(data.tree)
  library(DiagrammeR)
  
  if(is.null(subtree_to_plot)){
    # Keep entire dataset in tree
    spa_node_data <- spa_node_data
  } else if(subtree_to_plot == "top_LCI"){
    # Find subtree with highest LCI value
    spa_node_data <- spa_node_data[spa_node_data$`LCI effects`== max(spa_node_data$`LCI effects`),, drop = FALSE]
  } else if(subtree_to_plot == "top_Site"){
    # Find subtree with highest site value
    spa_node_data <- spa_node_data[spa_node_data$`Site Effects` == max(spa_node_data$`Site Effects`),, drop = FALSE]
    
  } else{
    # Assume value is an index node
    node_indexes <- sapply("pathString", function(x) grep(subtree_to_plot, spa_node_data[,x])) # Find all rows where the subtree_to_plot value is present in the pathString column

    if(class(node_indexes)[1] == "list"){
      # If there is no match for the node input
      stop("subtree_to_plot parameter undefined")
    } else{
      #spa_node_data <- spa_node_data[node_indexes,]
      spa_node_data <- rbind(spa_node_data[1,],spa_node_data[node_indexes,]) # need to keep the root node
      #TODO: NEED TO MAKE SURE THAT WE KEEP THE NODES THAT ARE ON THE WAY TO THE ONES WE WANT TO KEEP, E.G., WHEN KEEPING PATHS THAT HAVE 231, NEED TO KEEP 255/244
    }
    
    
  } # end of options for subtree_to_plot

  # Create tree
  spa_tree <- as.Node(spa_node_data)
  
  #TODO: add plot type sankey

  if(plot_type == "simpleNetwork"){
    # Create network for networkD3 plotting
    spa_network <- ToDataFrameNetwork(spa_tree, "name")
    simpleNetwork(spa_network[-3], fontSize = 12) # create D3 network plot. The -3 removes the name column from dataframe for plotting purposes
    
  }else if(plot_type == "dendogram"){
    # Plot as dendogram
    plot(as.dendrogram(spa_tree), center = TRUE)
  } else if(plot_type == "radial"){
    # Plot as radial network
    useRtreeList <- ToListExplicit(spa_tree, unname = TRUE)
    radialNetwork( useRtreeList)
  # } else if(plot_type == "sankey"){
  # 
  #   # Create dfs for sankey diagram
  #   # Add nodes DF
  #   nodes <- data.frame(matrix(nrow = 0, ncol = 1))
  # 
  #   # Add links DF
  #   links <- data.frame(matrix(ncol = 3, nrow = dim(spa_node_data)[1]-1)) # -1 rows because we are skipping first row
  #   names(links) = c("source", "target", "value")
  # 
  #   paths <- spa_node_data[,3:(dim(spa_node_data)[2]-1)] # get all spa_nodes_data columns that represent the indexes,i.e., node paths
  # 
  #   for(row in 2:nrow(spa_node_data)){ #skipping first row since that only has value at the root node, and not between two nodes
  #     
  #     
  #     # Initialize loop counters
  #     linksRow <- row - 1
  #     empty_indexes <- which(paths[row, ] == "") # get indexes which are empty
  #     
  #     if(length(empty_indexes) == 0){ # if there are no empty indexes, this means we are at a row with a leaf at max tier
  #       empty_indexes[1] <- ncol(paths) + 1 # set the start of the parent pathstring one index beyond the max tier, i.e., beyond the number of columns in the path df, so that the next line gets the proper parent node string.
  #     }
  #     
  #     # Initialize relevant loop variables
  #     parentNode <- paste(paths[row,1:(empty_indexes[1]-2)], collapse = "/")   # the parent node is the part of the pathstring consisting of the values on this row which start 2 columns before the last populated column, i.e., value != ""
  #     source <- spa_node_data$pathString[row]
  #     target <- parentNode
  #     link_value <- spa_node_data$`Site Effects`[row]
  #     
  #     # Search for current node name in the nodes DF
  #     search_target <- which(nodes$name == target)
  #     if(length(search_target) == 0){ # if the target is not in the nodes DF
  #       nodes <- rbind(nodes, target)
  #     }      
  #     
  #     
  #     search_source <- which(nodes$name == source)
  #     if(length(search_source) == 0){ # if the source is not in the nodes DF
  #       nodes <- rbind(nodes, source)
  #     }
  #     
  #     nodes <- unique(nodes) # Keep only one instance of each node
  # 
  #     # Values for links have to match the order in which the nodes appear in the nodes DF, starting at index 0.
  #     # I.e., node 0 is at nodes DF index 1, and must be included in the links DF with a value of 0 as either source or target
  #     
  #     links[linksRow,1] <- which(nodes == source) - 1 # value at col 1 is node from, our source. In this case, we're going from leaf to parent; the leaf node is defined by the pathstring.
  #     links[linksRow,2] <- which(nodes == target) - 1 # value at col 2 is node to. In this case, we're going from leaf to parent. The parent is defined as indicated in the comment for the parentNode line.
  #     links[linksRow,3] <- link_value # value of link
  #     
  #   }
  #   
  #   # Rename column for nodes DF
  #   colnames(nodes) <- c("name")
  # 
  #   
  #   links <- transform(links, value = as.numeric(value)) # transform value to a numeric column from a char column.
  #   # Plot sankey ## DOES NOT WORK; not all node names are in nodes dataframe.
  #   # Possible solution, tho currently not working, is to stack the source and target columns in links: temp <- stack(links[,1:2])
  #   # Another possibility is to prepend the word "Node" to all node names, or finally to change all names from pathstring format to Node A, node B, etc.
  #   sankeyNetwork(Links = links, Nodes = nodes,
  #                 Source = "source", Target = "target",
  #                 Value = "value", NodeID = "name",
  #                 fontSize= 12, nodeWidth = 30)
  #   
   } 
    else {
    stop("Plot_type undefined")
  }
  
  
  temp <- 1

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


