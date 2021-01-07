#' Adds demand vectors based on model specs to model object
#' @param model A model list object with the specs object listed
#' @return eeio model with a list of demand vectors added
#' @format A list with lists of totals by sector and formatted satellite tables
#' @export
loadDemandVectors <- function(model) {
  logging::loginfo("Loading demand vectors from model spec ...")
  model$demands <- list()
  for (v in model$specs$Demand$DemandVectors) {
    #Check if the demand is registered
    if (v %in% names(dem_vec_fxn_registry)) {
       func_to_eval <- dem_vec_fxn_registry[[v]]
       demandFunction <- as.name(func_to_eval)
       dv <- do.call(eval(demandFunction), list(model))
       model$demands[[v]] <- dv
       logging::loginfo(paste("Loaded",v,"demand vector."))
    } else {
      logging::logwarn(paste(v, "not found in registered demand vector functions. Not including", v))
    }
  }
  return(model)
}
