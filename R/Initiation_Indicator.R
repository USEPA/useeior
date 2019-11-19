#' Load indicator factors in a list based on model config.
#' @param specs specifications of the model.
#' @return a list of indicator factors not yet formatted for IOMB.
loadindicators <- function(specs) {
   logging::loginfo('Getting model indicators...')

   StaticIndicatorFactors <- loadLCIAfactors()

   #Static LCIA handling - getting factors from LCIA_factors for those indicators used in this model
   #create a list for appending abbreviations of those staticsource
   abbr_from_static <- c()
   for (ind in specs$Indicators) {
      if(ind$StaticSource) {
         abbr <- ind$Abbreviation
         logging::loginfo(paste('Adding',ind$FullName, 'indicator.'))
         abbr_from_static <- append(abbr_from_static,abbr)
      }
   }
   #Subset LCIA factors list for the list of abbbreviations
   factors_from_static <- subset(StaticIndicatorFactors, Abbreviation %in% abbr_from_static)
   #Place holder to later rbind in factors_from_dynamic
   indicators <- factors_from_static

   return(indicators)
}





