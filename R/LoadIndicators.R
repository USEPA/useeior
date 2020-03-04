#' Load indicator factors in a list based on model config.
#' @param specs Specifications of the model.
#' @return A list of indicator factors not yet formatted for IOMB.
loadindicators <- function(specs) {
   logging::loginfo('Getting model indicators...')

   StaticIndicatorFactors <- loadLCIAfactors()

   # Static LCIA handling - getting factors from LCIA_factors for those indicators used in this model
   # Create a list for appending abbreviations of those staticsource
   abbr_from_static <- c()
   for (ind in specs$Indicators) {
      if(ind$StaticSource) {
         abbr <- ind$Abbreviation
         logging::loginfo(paste('Adding',ind$FullName, 'indicator.'))
         abbr_from_static <- append(abbr_from_static,abbr)
      }
   }
   # Subset LCIA factors list for the list of abbbreviations
   factors_from_static <- StaticIndicatorFactors[StaticIndicatorFactors$Abbreviation %in% abbr_from_static, ]
   # Place holder to later rbind in factors_from_dynamic
   indicators <- factors_from_static

   return(indicators)
}

#' Deprecated function for generating LCIA output using LCIA_indicators static file
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A LCIA dataframe not yet formatted for IOMB.
generateLCIA <- function (model) {
   # Load LCIA factors
   lciafactors <- loadLCIAfactors()
   # Import LCIA indicators
   lciaindicators <- utils::read.table(system.file("extdata", "USEEIO_LCIA_Indicators.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
   indicators <- as.vector(unlist(lapply(model$specs$Indicators, FUN = `[[`, "Abbreviation")))
   lciaindicators <- lciaindicators[lciaindicators$Abbreviation%in%indicators, ]
   # Merge LCIA factors and indicators to get meta data
   lcia <- merge(lciafactors, lciaindicators, by = "Abbreviation")
   return(lcia)
}

#' Loads all LCIA factors from static source file after melting it to long file
#' @return A dataframe with "Name""Category""Subcategory""Unit""UUID""Abbreviation""Amount"
loadLCIAfactors <- function() {
   lciafact <- utils::read.table(system.file("extdata", "USEEIO_LCIA_Factors.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
   #Melt these so there is one indicator score per line
   lciafactlong <- reshape2::melt(lciafact, id.vars = c(1:5))
   #Convert variable to character
   lciafactlong$variable <- as.character(lciafactlong$variable)
   #Convert values to numeric
   lciafactlong$value <- as.numeric(lciafactlong$value)
   #drop zeroes
   lciafactlong <- lciafactlong[lciafactlong$value>0, ]
   #Change colname for merging later
   names(lciafactlong)[names(lciafactlong) == "variable"] <- "Abbreviation"
   names(lciafactlong)[names(lciafactlong) == "value"] <- "Amount"
   return(lciafactlong)
}

