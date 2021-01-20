#' Adds demand vectors and metadata based on model specs to model object
#' @param model A model list object with the specs object listed
#' @return model with a list of demand vectors and a meta file
#' @export
loadDemandVectors <- function(model) {
  logging::loginfo("Loading demand vectors from model spec ...")
  model$DemandVectors <- list()
  meta <- data.frame()
  model$DemandVectors$vectors <- list()
  specs <- model$specs$DemandVectors
  for (v in names(specs)) {

    # Populate metadata
    i <- specs[[v]]
    i["Type"] <- v
    meta <- rbind(meta,data.frame(i))

    #Check if the demand is registered
    if (v %in% names(dem_vec_fxn_registry)) {
       func_to_eval <- dem_vec_fxn_registry[[v]]
       demandFunction <- as.name(func_to_eval)
       dv <- do.call(eval(demandFunction), list(model))
       model$DemandVectors$vectors[[v]] <- dv
       logging::loginfo(paste("Loaded",v,"demand vector."))
    } else {
      logging::logwarn(paste(v, "not found in registered demand vector functions. Not including", v))
    }
  }
  model$DemandVectors$meta <- meta
  return(model)
}
  