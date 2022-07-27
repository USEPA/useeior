#' Adds demand vectors and metadata based on useeior defaults and model specs to model object
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return model with a list of demand vectors and a meta file stored appended as model$DemandVectors
loadDemandVectors <- function(model) {
  logging::loginfo("Loading demand vectors ...")
  model$DemandVectors <- list()
  model$DemandVectors$vectors <- list()
  meta <- loadDefaultDemandVectorMeta(model)
  
  specs <- model$specs$DemandVectors
  for (v in setdiff(names(specs), "DefaultDemand")) {
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
      logging::loginfo(paste("Loading", i["Location"], i["Name"], "demand vector..."))
      func_to_eval <- DemandVectorFunctionRegistry[[i$Type]][[i$System]]
      demandFunction <- as.name(func_to_eval)
      dv <- do.call(eval(demandFunction), list(model))
      model$DemandVectors$vectors[[i$ID]] <- dv[grepl(i$Location, names(dv))]
    } else {
      stop(paste(i$Type,i$System,"not found in registered demand vector functions. This vector must be registered or removed from the model spec."))
    }
  }
    
  model$DemandVectors$meta <- meta
  return(model)
}

#' Loads a package stored demand vector metadata (.yml) for vectors to be created for every model with type and system specified
#' This function adds additional year, location and IDs along with the type and system based on the model specs.
#' @param model An EEIO model object with model specs and IO tables loaded
#' @return a data frame of metadata with columns Type, System, Name, Year, Location and ID with rows for each default
loadDefaultDemandVectorMeta <- function(model) {
  meta <- data.frame()
  specs <- getConfiguration("DefaultDemandVectors", "demand")
  for (r in model$specs$ModelRegionAcronyms) {
    meta_r <- data.frame()
    for (v in names(specs)) {
      # Populate metadata
      i <- specs[[v]]
      i["Name"] <- v
      i["Year"] <- model$specs$IOYear
      i["Location"] <- r
      i["ID"] <- createDemandID(i)
      meta_r <- rbind(meta_r, as.data.frame(i, stringsAsFactors = FALSE))
    }
    meta <- rbind(meta, meta_r)
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
