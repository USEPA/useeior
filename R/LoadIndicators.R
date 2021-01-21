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
      meta_fields <- configr::read.config(system.file("extdata/IOMB_Fields.yml", package="useeior"))[["Indicator"]][["Meta"]]
      i <- s[meta_fields]
      meta <- rbind(meta,data.frame(i))

      #Get factors
      f <- loadFactors(s)
      #Make sure indicator name comes from spec and not factor source data
      f$Indicator <- s[["FullName"]]
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
   factor_fields <- configr::read.config(system.file("extdata/IOMB_Fields.yml", package="useeior"))[["Indicator"]][["Factor"]]
   factors <- factors[,factor_fields]
   return(factors)
}

#' Loads all LCIA factors from static source file after melting it to long file
#' @return A dataframe with "Flowable", "UUID", "Context", "Unit", "Amount", "Code".
loadLCIAfactors <- function() {
   # Load static LCIA factors
   lciafact <- utils::read.table(system.file("extdata", "USEEIO_LCIA_Factors.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
   # Melt these so there is one indicator score per line
   lciafactlong <- reshape2::melt(lciafact, id.vars = c("Flowable", "Context", "Unit", "UUID"))
   # Add Code and Amount
   lciafactlong[, "Code"] <- as.character(lciafactlong$variable)
   lciafactlong[, "Amount"] <- as.numeric(lciafactlong$value)
   # Drop zeroes and keep wanted columns
   lciafactlong <- lciafactlong[lciafactlong$value>0,
                                c("Flowable", "UUID", "Context", "Unit", "Amount", "Code")]
   return(lciafactlong)
}

#' Loads data for all model indicators as listed in model specs
#' @param list a model object with IO data loaded
#' @return list a model object with Indicators added
#' @export
loadandbuildIndicators <- function(model) {
   # Generate C matrix: LCIA indicators
   indicators <- loadIndicators(model$specs)
   # Add to model object
   model$Indicators <- indicators
   return(model)
}
