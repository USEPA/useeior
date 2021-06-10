#' Functions to map elementary or waste flows in satellite tables drawing on an internal mapping file
#' Import flow mapping file.
flowmapping <- utils::read.table(system.file("extdata", "Crosswalk_USEEIO_FlowMapping.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                                 fill = TRUE, colClasses = c(rep("character", 26), rep("numeric", 2)))


#' Map resource and emission names in a standardized satellite table to new names by flow name
#' @param sattable A standardized satellite table with resource and emission names from original sources.
#' @param sattablemeta Meta data of the satellite table.
#' @return A standardized satellite table with new resource and emission names.
mapListbyName <- function (sattable, sattablemeta) {
  originalflowsource <- sattablemeta$OriginalFlowSource
  if(originalflowsource=="") {
    stop("Set variable 'originalflowsource' first.")
  }
  # Get subset for mapping
  sourcemapping <- flowmapping[flowmapping$Source==originalflowsource, ]
  fieldstokeep <- c("OriginalName", "NewName", "CAS", "NewCategory", "NewSubCategory", "NewUnit", "UUID")
  sourcemapping <- sourcemapping[, fieldstokeep]
  # Merge sattable with sourcemapping
  sattablewithmap <- merge(sattable, sourcemapping, by.x = "Flowable", by.y = "OriginalName", all.x = TRUE)
  # Add old flow name as tag is this changes
  if(!identical(sattablewithmap$Flowable, sattablewithmap$NewName)) {
    sattablewithmap$MetaTags <- sattablewithmap$Flowable
  }
  sattablewithmap$Flowable <- sattablewithmap$NewName
  sattablewithmap$Context <- apply(sattablewithmap[, c("NewCategory", "NewSubCategory")],
                                   1, FUN = joinStringswithSlashes)
  # If context is "/" replace with blank
  sattablewithmap$Context[sattablewithmap$Context == "/"] <- ""
  
  sattablewithmap$CAS <- sattablewithmap$CAS.y
  sattablewithmap$Unit <- sattablewithmap$NewUnit
  # Get column names from standard satellite table
  standardnames <- getStandardSatelliteTableFormat()
  sattable <- sattablewithmap[, standardnames]
  # Check for unmapped flows
  unmapped <- apply(sattable['Flowable'], 1, function(x){any(is.na(x))})
  if(sum(unmapped)>0){
    logging::logwarn("Some flows not mapped, they will be removed")
    sattable <- sattable[!unmapped, ]
  }
  
  return(sattable)
}
