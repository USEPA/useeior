#' Load satellite tables in a list based on model.
#' @param model Configuration of the model.
#' @return Lists of national totals by sector and formatted satellite tables
#' @format A list with lists of totals by sector and formatted satellite tables
#' \describe{
#'  \itemize{
#'    \item totals_by_sector
#'      \itemize{
#'        \item {SectorCode} {The code of the sector in the model IO schema}
#'        \item SectorName
#'        \item FlowName
#'        \item Year
#'        \item FlowAmount
#'        \item ReliabilityScore
#'        \item GeographicalCorrelation
#'        \item TechnologicalCorrelation
#'        \item DataCollection
#'        \item Location
#'        \item Compartment
#'        \item Unit
#'        \item Location
#'        \item MetaSources
#'      }
#'    \item tables
#'      \itemize{
#'        \item FlowName
#'        \item CAS
#'        \item FlowCategory
#'        \item FlowSubCategory
#'        \item FlowUUID
#'        \item ProcessName
#'        \item ProcessCode
#'        \item ProcessLocation
#'        \item FlowAmount       
#'        \item UncertaintyDistribution
#'        \item UncertaintyExpectedValue
#'        \item UncertaintyDispersion
#'        \item UncertaintyMin
#'        \item UncertaintyMax
#'        \item DQReliability
#'        \item DQTemporal
#'        \item DQGeographical
#'        \item DQTechnological
#'        \item DQDataCollection
#'        \item MetaYearofData
#'        \item MetaTags
#'        \item MetaSources
#'        \item MetaYearofData        
#'        \item MetaOther
#'      }
#'  }
#' }
#' @description Only works for static national totals by BEA sector in a set format
loadsattables <- function(model) {
  sattables <- list()
  sattables$totals_by_sector <- list()
  sattables$coeffs_by_sector <- list()
  
  logging::loginfo("Initializing model satellite tables...")

  for (sat in model$specs$SatelliteTable) {
    logging::loginfo(paste("Adding model satellite tables..."))
    #Check if its the table uses a static file..if so proceed
    if(!is.null(sat$StaticFile)) {
      totals_by_sector <- utils::read.table(system.file("extdata", sat$StaticFile, package = "useeior"),
                                    sep = ",", header = TRUE, stringsAsFactors = FALSE)
      #If BEA based
      if (sat$SectorListSource == "BEA") {
        #If BEA years is not the same as model year, must perform allocation
        if (sat$SectorListYear == 2007 && model$specs$BaseIOSchema == 2012) {
          #apply allocation
        } else if (sat$SectorListLevel == "Detail" && model$specs$BaseIOLevel != "Detail") {
          totals_by_sector <- aggregateSatelliteTable(totals_by_sector, sat$SectorListLevel, model$specs$BaseIOLevel, model)
        }
      } else {
        #In NAICS #
      }
      #Add in DQ columns and additional contextual scores not provided
      totals_by_sector <- scoreContextualDQ(totals_by_sector) #just sets TemporalCorrelation for now
      
      #Check that all DQ columns are present
      if(length(getDQfields(totals_by_sector))!=5){
        logging::logerror('Missing 1 or more data quality fields in satellite data.')
      }
      
      #split table based on data years
      if (length(sat$DataYears)>1) {
        print("more than 1 data year")
      }
      #Split table based on regions
      sattablecoeffs <- data.frame()
      for (r in model$specs$ModelRegionAcronyms) {
        sattable_r <- totals_by_sector[totals_by_sector$Location==r, ]
        if (r=="RoUS") {
          IsRoUS <- TRUE
        } else {
          IsRoUS <- FALSE
          #Change label to location
          if (model$specs$ModelType=="state") {
            sattable_r[, "Location"] <- paste("US-", r, sep = "")
          }
        }
        sattablecoeffs_r <- generateFlowtoDollarCoefficient(totals_by_sector, sat$DataYears[1], model$specs$IOYear,r, IsRoUS=IsRoUS, model)
        sattablecoeffs <- rbind(sattablecoeffs,sattablecoeffs_r)
      }
      #Need to have sector name
      sattablecoeffs$SectorName <- NULL
      sattablecoeffs_withsectors <- merge(sattablecoeffs, model$SectorNames, by = "SectorCode")

      sattablestandardized <- generateStandardSatelliteTable(sattablecoeffs_withsectors, mapbyname = TRUE, sat)
    } else {
      #Source is dynamic - not currently working
      source(sat$ScriptSource)
      func_to_eval <- sat$ScriptFunctionCall
      satgenfunction <- as.name(func_to_eval)
      sattablestandardized <- do.call(eval(satgenfunction), sat$ScriptFunctionParameters)
    }
    #append it to list
    sattables$totals_by_sector[[sat$Abbreviation]] <- totals_by_sector
    sattables$coeffs_by_sector[[sat$Abbreviation]] <- sattablestandardized
  }
  return(sattables)
}
