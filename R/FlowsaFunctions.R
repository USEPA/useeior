# Functions for handling data from flowsa, https://github.com/USEPA/flowsa

#' Load flowsa's FlowBySector df and collapse sector columns
#' @param sat_spec, a standard specification for a single satellite table
#' @return A data frame for flowsa data in sector by region totals format
getFlowbySectorCollapsed <- function(sat_spec) {
  # Access flowsa getFlowBySector_collapsed by indicating StaticSource: False
  if (!(sat_spec$StaticSource)) {
    method_name <- sub(".parquet$", "", sat_spec$StaticFile)
    flowsa <- reticulate::import("flowsa")
    fbs_collapsed <- flowsa$collapse_FlowBySector(method_name)
    # checks columns that are all None values and converts to NA
    for (i in colnames(fbs_collapsed)) {
      if (is.list(fbs_collapsed[[i]])) {
        if (i == 'MetaSources') {
          # MetaSources must be a string if all None
          fbs_collapsed[ , i] <- ""
        } else {
          fbs_collapsed[ , i] <- NA
        }
      }
    }
  } else {
    f <- loadDataCommonsfile(sat_spec$StaticFile)
    fbs <- as.data.frame(arrow::read_parquet(f))
    # collapse the FBS sector columns into one column based on FlowType
    fbs$Sector <- NA
    fbs$Sector <- ifelse(fbs$FlowType=='TECHNOSPHERE_FLOW', fbs$SectorConsumedBy, fbs$Sector)
    fbs$Sector <- ifelse(fbs$FlowType=='WASTE_FLOW', fbs$SectorProducedBy, fbs$Sector)
    fbs$Sector <- ifelse((fbs$FlowType=='WASTE_FLOW') & (is.na(fbs$SectorProducedBy)), fbs$SectorConsumedBy, fbs$Sector)
    fbs$Sector <- ifelse((fbs$FlowType=='ELEMENTARY_FLOW') & (is.na(fbs$SectorProducedBy)), fbs$SectorConsumedBy, fbs$Sector)
    fbs$Sector <- ifelse((fbs$FlowType=='ELEMENTARY_FLOW') & (is.na(fbs$SectorConsumedBy)), fbs$SectorProducedBy, fbs$Sector)
    fbs$Sector <- ifelse((fbs$FlowType=='ELEMENTARY_FLOW') & (fbs$SectorConsumedBy %in% c('F010', 'F0100', 'F01000')) &
                           (fbs$SectorProducedBy %in% c('22', '221', '2213', '22131', '221310')), fbs$SectorConsumedBy, fbs$Sector)
    # drop sector consumed/produced by columns
    fbs_collapsed <- fbs[,!(names(fbs) %in% c('SectorProducedBy', 'SectorConsumedBy'))]
  }
  # reorder col
  fbs_collapsed <- prepareFlowBySectorCollapsed(fbs_collapsed, satellite=TRUE)

  return(fbs_collapsed)
}


#' Adjusts flowbysector data from flowsa
#' @param fbsc A FlowBySector collapsed df from flowsa
#' @param satellite bool, set to TRUE when used for env satellite tables
#' @return A data frame of sector by region totals
prepareFlowBySectorCollapsed <- function(fbsc, satellite=TRUE) {
  # Replace Python type None with NA
  fbsc <- replaceNonewithNA(fbsc)
  # add columns if not present
  cols <- c('FlowUUID')
  fbsc[cols[!(cols %in% colnames(fbsc))]] <- ""
  # Ensure correct type, parquet can come in as vctrs_unspecified
  fbsc[c("Context", "FlowUUID")] <- sapply(fbsc[c("Context", "FlowUUID")], function(x) as.character(x))
  # If context is NA replace with blank
  fbsc[,"Context"][is.na(fbsc[,"Context"])] <- ""
  fbsc[,"FlowUUID"][is.na(fbsc[,"FlowUUID"])] <- ""
  if(satellite) {
  # Filter technosphere flows
    acceptable_types <- c("ELEMENTARY_FLOW", "WASTE_FLOW")
    fbsc <- fbsc[fbsc$FlowType %in% acceptable_types, ]
  }
  # Map location codes to names
  fbsc$Location <- mapLocationCodestoNames(fbsc$Location, unique(fbsc$LocationSystem))
  # Get standard sat table fields
  fields <- getStandardSatelliteTableFormat()
  if (!"Sector" %in% colnames(fbsc)) {
    # keep SPB and SCB when not a FBS collapsed
    fields <- append(fields, c("SectorProducedBy", "SectorConsumedBy"), 3)
  }
  # Remove unused data
  fbsc <- fbsc[which(colnames(fbsc) %in% fields)]
  return(fbsc)
}


#' Load flowsa's FlowBySector df
#' @param filepath, a filepath to local parquet file
#' @return A data frame for flowsa data in sector by region totals format
getFlowbySector <- function(filepath) {
  f <- loadDataCommonsfile(filepath)
  fbs <- as.data.frame(arrow::read_parquet(f))
  fbs <- prepareFlowBySectorCollapsed(fbs, satellite=FALSE)
  return(fbs)
}
