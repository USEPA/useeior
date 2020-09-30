#' Load totals by sector/region and builds satellite tables based on model specs.
#' Supports BEA and NAICS based totals. These totals can be provided as static files
#' or dynamic function calls are supported. NAICS-based totals are aggregated/allocated to BEA sectors
#' as part of the preparation.
#' @param model A model list object with the specs object listed
#' @return Lists of national totals by sector and formatted satellite tables
#' @format A list with lists of totals by sector and formatted satellite tables
#' \describe{
#'  \itemize{
#'    \item totals_by_sector
#'      \itemize{
#'        \item SectorCode {The code of the sector in the model IO schema}
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
#'    \item coeffs_by_sector
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
    } else {
     #Source is dynamic
     
      func_to_eval <- sat$ScriptFunctionCall
      totalsgenfunction <- as.name(func_to_eval)
      totals_by_sector <- do.call(eval(totalsgenfunction), list(sat$ScriptFunctionParameters))
      
    }
            #If BEA based
    if (sat$SectorListSource == "BEA") {
        #If BEA years is not the same as model year, must perform allocation
      if (sat$SectorListYear == 2007 && model$specs$BaseIOSchema == 2012) {
          #apply allocation
      } else if (sat$SectorListLevel == "Detail" && model$specs$BaseIOLevel != "Detail") {
          totals_by_sector <- aggregateSatelliteTable(totals_by_sector, sat$SectorListLevel, model$specs$BaseIOLevel, model)
        }
    } else if (("NAICS" %in% sat$SectorListSource)){
      #In NAICS #
      totals_by_sector <- mapFlowTotalsbySectorandLocationfromNAICStoBEA(totals_by_sector, sat$DataYears[1], model)
    }
    #Add in DQ columns and additional contextual scores not provided
    totals_by_sector <- scoreContextualDQ(totals_by_sector) #just sets TemporalCorrelation for now
    
    #Check for disaggregation
    if(!is.null(model$specs$DisaggregationSpecs)){
      totals_by_sector <- disaggregateSatelliteTable(model, totals_by_sector)
    }
    
    #Check that all DQ columns are present
    len_dq_fields <- length(getDQfields(totals_by_sector))
    if(len_dq_fields!=5){
      logging::logerror(paste0('Missing 1 or more data quality fields in satellite data. ',len_dq_fields, " present"))
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
          sattable_r[,  "Location"] <- paste("US-", r, sep = "")
        }
      }
      sattablecoeffs_r <- generateFlowtoDollarCoefficient(totals_by_sector, sat$DataYears[1], model$specs$IOYear, r, IsRoUS=IsRoUS, model)
      sattablecoeffs <- rbind(sattablecoeffs,sattablecoeffs_r)
    }
    #Need to have sector name
    sattablecoeffs$SectorName <- NULL
    #! This is incorrect because the coeffs still just have industry names and not model sector 
    sattablecoeffs_withsectors <- merge(sattablecoeffs, model$SectorNames, by = "SectorCode")

    sattablestandardized <- generateStandardSatelliteTable(sattablecoeffs_withsectors, sat)
    
    #If dataset is static, it will use the embedded mapping files to map flows to internal flow names
    if (!is.null(sat$StaticFile)) {
      sattablestandardized <- mapListbyName(sattablestandardized, sat)
    } 
    
    #append it to list
    sattables$totals_by_sector[[sat$Abbreviation]] <- totals_by_sector
    sattables$coeffs_by_sector[[sat$Abbreviation]] <- sattablestandardized
  }
  return(sattables)
}

#' Loads data for all satellite tables as lists in model specs
#' @param list a model object with IO data loaded
#' @return list a model object with Satellite tables added 
#' @export
loadbuildSatelliteTables <- function(model) {
  # Generate satellite tables
  model$SatelliteTables <- loadsattables(model)
  # Combine satellite tables (coeffs_by_sector) into a single df
  StandardizedSatelliteTable <- data.frame()
  for (table in model$SatelliteTables$coeffs_by_sector) {
    StandardizedSatelliteTable <- rbind(StandardizedSatelliteTable, table)
  }
  # transform into a flow x sector matrix
  StandardizedSatelliteTable["Flow"] <- apply(StandardizedSatelliteTable[, c("FlowName", "FlowCategory", "FlowSubCategory", "FlowUnit")],
                                              1 ,FUN = joinStringswithSlashes)
  StandardizedSatelliteTable["Sector"] <- apply(StandardizedSatelliteTable[, c("ProcessCode", "ProcessLocation")], 1, FUN = joinStringswithSlashes)
  
  #! Needs to be cast and made into matrix, but the problem is that the sectors need to have the order and completness of the model sectors list and not just those in the sat tables
  sattables_cast <- reshape2::dcast(StandardizedSatelliteTable, Flow ~ Sector, fun.aggregate = sum, value.var = "FlowAmount") #! check why aggregation is needed
  # Move Flow to rowname so matrix is all numbers
  rownames(sattables_cast) <- sattables_cast$Flow
  sattables_cast$Flow <- NULL
  # Complete sector list using model$Industries
  columns_to_add <- tolower(paste(model$Industries[!model$Industries%in%StandardizedSatelliteTable$ProcessCode], model$specs$PrimaryRegionAcronym, sep = "/"))
  sattables_cast[, columns_to_add] <- 0
  # Adjust column order to be the same with V_n rownames
  model$sattables_cast <- sattables_cast[, tolower(paste(rownames(model$MakeTransactions), model$specs$PrimaryRegionAcronym, sep = "/"))]
  return(model)
}
