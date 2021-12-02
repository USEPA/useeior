#' Adds demand vectors and metadata based on useeior defaults and model specs to model object
#' @param model A model list object with the specs object listed
#' @return model with a list of demand vectors and a meta file stored appended as model$DemandVectors
loadDemandVectors <- function(model) {
  logging::loginfo("Loading demand vectors ...")
  model$DemandVectors <- list()
  model$DemandVectors$vectors <- list()
  meta <- loadDefaultDemandVectorMeta(model)
  
  specs <- model$specs$DemandVectors
  for (v in names(specs)) {
    # Populate metadata
    i <- specs[[v]]
    i["Name"] <- v
    i["ID"] <- createDemandID(i)
    #check to see that this id doesn't already exist
    if (i["ID"] %in% meta$ID) {
      logging::logwarn(paste("A demand vector with ID =", i["ID"], "already exists. A new one will not be created."))
    } else {
      #add this to the df of demand vectors
      meta <- rbind(meta,data.frame(i, stringsAsFactors = FALSE) )
    }
  }

  for (row in 1:nrow(meta)) {
    #Check if the demand is registered
    i <- meta[row,]
    if (!is.null(DemandVectorFunctionRegistry[[i$Type]][[i$System]])) {
      logging::loginfo(paste("Loading", i["Name"], "demand vector..."))
      func_to_eval <- DemandVectorFunctionRegistry[[i$Type]][[i$System]]
      demandFunction <- as.name(func_to_eval)
      dv <- do.call(eval(demandFunction), list(model))
      model$DemandVectors$vectors[[i$ID]] <- dv
    } else {
      stop(paste(i$Type,i$System,"not found in registered demand vector functions. This vector must be registered or removed from the model spec."))
    }
  }
    
  model$DemandVectors$meta <- meta
  return(model)
}

#' Loads a package stored csv of demand vector meta for vectors to be created for every model with type and system specified
#' This function adds additional year, location and IDs along with the type and system based on the model given
#' @param model An EEIO model that has been initialized
#' @return a data frame of metadata with cols Type, System, Name, Year, Location and ID with rows for each default
loadDefaultDemandVectorMeta <- function(model) {
  meta <- data.frame(Type=character(),
                     System=character(),
                     Name=character(),
                     Year=character(),
                     Location=character(),
                     ID=character())
  demands <- utils::read.table(system.file("extdata", "default_demand_vectors.csv", package = "useeior"),
                    sep = ",", header = TRUE, stringsAsFactors = FALSE)
  for (row in 1:nrow(demands)) {
    i <- demands[row,]
    i["Name"] <- paste0(i$Type,"_",i$System) 
    i["Year"] <- model$specs$IOYear
    i["Location"] <- model$specs$ModelRegionAcronyms[1]
    i["ID"] <- createDemandID(i)
    meta <- rbind(meta,data.frame(i, stringsAsFactors = FALSE) )
  }
  return(meta)
}

#' Creates an ID for a demand vector based on user provided demandmeta data
#' @param demandmeta, a data frame of metadata with one row for a demand with
#' columns Type, System, Name, Year, Location and ID
#' @return character vector with a name 
createDemandID <- function(demandmeta) {
  i <- demandmeta
  ID <- paste(i$Year,i$Location,i$Type,i$System,sep="_")
  return(ID)
}
