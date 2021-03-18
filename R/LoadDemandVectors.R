#' Adds demand vectors and metadata based on model specs to model object
#' @param model A model list object with the specs object listed
#' @return model with a list of demand vectors and a meta file
#' @export
loadDemandVectors <- function(model) {
  logging::loginfo("Loading demand vectors from model spec...")
  model$DemandVectors <- list()
  meta <- data.frame()
  model$DemandVectors$vectors <- list()
  specs <- model$specs$DemandVectors
  for (v in names(specs)) {

    # Populate metadata
    i <- specs[[v]]
    i["Name"] <- v
    i["ID"] <- tolower(paste(i$Year,i$Location,i$Type,i$System,sep="_"))
    meta <- rbind(meta,data.frame(i, stringsAsFactors = FALSE) )

    #Check if the demand is registered
    if (!is.null(dem_vec_fxn_registry[[i$Type]][[i$System]])) {
      logging::loginfo(paste("Loading", v, "demand vector..."))
      func_to_eval <- dem_vec_fxn_registry[[i$Type]][[i$System]]
      demandFunction <- as.name(func_to_eval)
      dv <- do.call(eval(demandFunction), list(model))
      model$DemandVectors$vectors[[i$ID]] <- dv
    } else {
      logging::logerror(paste(v, "not found in registered demand vector functions. This vector must be registered or removed from the model spec."))
      stop()
    }
  }
  model$DemandVectors$meta <- meta
  return(model)
}
  