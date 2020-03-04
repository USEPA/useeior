#' Import flow mapping file.
flowmapping <- utils::read.table(system.file("extdata", "Crosswalk_USEEIO_FlowMapping.csv", package = "useeior"),
                                 sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE,
                                 fill = TRUE, colClasses = c(rep("character", 26), rep("numeric", 2)))

#' List original sources of environmental flows.
listOriginalSources <- function () {
  return(unique(flowmapping$Source))
}

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

  sattablewithmap <- merge(sattable, sourcemapping, by.x = "FlowName", by.y = "OriginalName", all.x = TRUE)
  # Add old flow name as tag is this changes
  if(!identical(sattablewithmap$FlowName, sattablewithmap$NewName)) {
    sattablewithmap$MetaTags <- sattablewithmap$FlowName
  }
  sattablewithmap$FlowName <- sattablewithmap$NewName
  sattablewithmap$FlowCategory <- sattablewithmap$NewCategory
  sattablewithmap$FlowSubCategory <- sattablewithmap$NewSubCategory
  sattablewithmap$CAS <- sattablewithmap$CAS.y
  sattablewithmap$FlowUnit <- sattablewithmap$NewUnit
  sattablewithmap$FlowUUID <- sattablewithmap$UUID
  # Get column names from standard satellite table
  standardnames <- colnames(getStandardSatelliteTableFormat())
  sattable <- sattablewithmap[,standardnames]
  return(sattable)
}

#' Map resource and emission names in a standardized satellite table to new names by name and category.
#' Must set original source before getting this information.
#' @param originalname Original name of flows.
#' @param originalcategory Original category of flows, an optional parameter.
#' @param sattablemeta Meta data of the satellite table.
#' @return A full flow information as a vector of 6 components using original name only.
mapFlowbyNameandCategory <- function (originalname, originalcategory = "", sattablemeta) {
  originalflowsource <- sattablemeta$OriginalFlowSource
  if(originalflowsource=="") {
    stop("Set variable 'originalflowsource' first.")
  }
  sourcemapping <- flowmapping[flowmapping$Source==originalflowsource, ]
  if (nrow(sourcemapping)==0) {
    stop(paste("No flows found for source", originalflowsource))
  }
  if (originalcategory != "") {
    matchingrow <- sourcemapping[sourcemapping$OriginalName==originalname&&sourcemapping$OriginalCategory==originalcategory, ]
  } else {
    matchingrow <- sourcemapping[sourcemapping$OriginalName==originalname, ]
  }
  if(nrow(matchingrow)==0) {
    stop(paste("No flow found with original name", originalname, "and category", originalcategory, "for source", originalflowsource))
  } else if (nrow(matchingrow)>1) {
    stop(paste("More than 1 flow with name", originalname, "found for source", originalflowsource, "You must specify a context."))
  }
  name <- as.character(matchingrow["NewName"])
  CAS <- as.character(matchingrow["CAS"])
  category <- as.character(matchingrow["NewCategory"])
  subcategory <- as.character(matchingrow["NewSubCategory"])
  UUID <- as.character(matchingrow["UUID"])
  unit <- as.character(matchingrow["NewUnit"])
  flow <- c(name, CAS, category, subcategory, unit)
  return(flow)
}

#' Map resource and emission names in a standardized satellite table to new names by code and category.
#' Must set original source before getting this information.
#' @param originalcas Original name of flows.
#' @param originalcategory Original category of flows, an optional parameter.
#' @param sattablemeta Meta data of the satellite table.
mapFlowbyCodeandCategory <- function (originalcas, originalcategory = "", sattablemeta) {
  originalflowsource <- sattablemeta$OriginalFlowSource
  if(originalflowsource=="") {
    stop("Set variable 'originalflowsource' first.")
  }
  sourcemapping <- flowmapping[flowmapping$Source==originalflowsource, ]
  if (nrow(sourcemapping)==0) {
    stop(paste("No flows found for source", originalflowsource))
  }
  if (originalcategory != "") {
    matchingrow <- sourcemapping[sourcemapping$OriginalCAS==originalcas&&sourcemapping$OriginalCategory==originalcategory, ]
  } else {
    matchingrow <- sourcemapping[sourcemapping$OriginalCAS==originalcas, ]
  }
  if(nrow(matchingrow)==0) {
    stop(paste("No flow found with original CAS", originalcas, "and category", originalcategory, "for source", originalflowsource))
  } else if (nrow(matchingrow)>1) {
    stop(paste("More than 1 flow with CAS", originalcas, "found for source", originalflowsource, "You must specify a context."))
  }
  name <- as.character(matchingrow["NewName"])
  CAS <- as.character(matchingrow["CAS"])
  category <- as.character(matchingrow["NewCategory"])
  subcategory <- as.character(matchingrow["NewSubCategory"])
  UUID <- as.character(matchingrow["UUID"])
  unit <- as.character(matchingrow["NewUnit"])
  flow <- c(name, CAS, category, subcategory, UUID, unit)
  return(flow)
}
