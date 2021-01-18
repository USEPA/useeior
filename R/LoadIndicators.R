factor_fields <- c("Indicator","Flowable","Context","Unit","Amount")
meta_fields <- c("FullName","Abbreviation","Group","Unit","SimpleUnit","SimpleName")
#Indicator and FullName are the same

#' Load indicators and associated factors in a list based on model config.
#' @param specs Specifications of the model.
#' @return A list with df for indicators and df for factors.
loadIndicators <- function(specs) {
   logging::loginfo("Initializing model indicators...")
   meta <- data.frame()
   factors <- data.frame()
   for (s in specs$Indicators) {
      logging::loginfo(paste("Getting", tolower(s$FullName), "indicators..."))
      
      # Populate metadata
      i <- s[meta_fields]
      meta <- rbind(meta,data.frame(i))

      #Get factors
      f <- loadFactors(s)
      factors <- rbind(factors,f)
   }   
   indicators <- list(meta=meta,factors=factors)
   return(indicators)
}

#' Load indicator factors based on spec from static or dynamic source
#' @param specs Specification of an indicator
#' @return A dataframe of factors with factor_fields
loadFactors <- function(ind_spec) {
   if(ind_spec$StaticSource) {
      # Load LCIA factors from static file
      StaticIndicatorFactors <- loadLCIAfactors()
      # Subset LCIA factors list for the abbreviations
      factors <- StaticIndicatorFactors[StaticIndicatorFactors$Code == ind_spec$Abbreviation, ]
      # Add Indicator column
      factors <- cbind("Indicator" = ind_spec$FullName, factors)
   } else {
      func_to_eval <- ind_spec$ScriptFunctionCall
      indloadfunction <- as.name(func_to_eval)
      factors <- do.call(eval(indloadfunction), list(ind_spec$ScriptFunctionParameters))
      factors <- prepareLCIAmethodforIndicators(factors)
   }
   factors <- factors[,factor_fields]
   return(factors)
}


#' Loads data for all model indicators as listed in model specs
#' @param list a model object with IO data loaded
#' @return list a model object with Indicators added
#' @export
loadandbuildIndicators <- function(model) {
   # Generate C matrix: LCIA indicators
   indicators <- loadIndicators(model$specs)
   # Add to model object
   model$indicators <- indicators
   return(model)
}
