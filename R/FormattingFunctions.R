#' Format a dataframe having IO table structure (with just sector code in row and column names) for IOMB process.
#' Change row and column names of the dataframe to "code/names/locationcode".
#' @param IOtable The dataframe, having IO table structure with just sector code, to be formatted.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe, having IO table structure, with row and column names from "code" to "code/names/locationcode".
formatIOTableforIOMB <- function (IOtable, model) {
  # Load pre-saved IndustryCodeName
  IndustryCodeName <- get(paste(model$specs$BaseIOLevel, "IndustryCodeName", model$specs$BaseIOSchema, sep = "_"))
  colnames(IndustryCodeName) <- c("Code", "Name")
  # Load pre-saved or model-specific CommodityCodeName tables, depent on modeltype
  if (model$specs$CommoditybyIndustryType=="Industry") {
    CommodityCodeName <- get(paste(model$specs$BaseIOLevel, "CommodityCodeName", model$specs$BaseIOSchema, sep = "_"))
  } else {
    CommodityCodeName <- model$SectorNames
  }
  colnames(CommodityCodeName) <- c("Code", "Name")
  # Determine whether it is Industry/Commodity in IOtable row/column
  # Modify IOtable row and column names
  if (length(unique(rownames(IOtable)%in%IndustryCodeName$Code)) == 1) {
    # Row name is Industry
    IndustryCodeName <- IndustryCodeName[IndustryCodeName$Code%in%rownames(IOtable), ]
    IndustryCodeName <- IndustryCodeName[order(factor(IndustryCodeName$Code, levels = rownames(IOtable))), ]
    rownames(IOtable) <- tolower(paste(rownames(IOtable), IndustryCodeName[, 2], "US", sep = "/"))
  } else {
    # Row name is Commodity
    CommodityCodeName <- CommodityCodeName[CommodityCodeName$Code%in%rownames(IOtable), ]
    CommodityCodeName <- CommodityCodeName[order(factor(CommodityCodeName$Code, levels = rownames(IOtable))), ]
    rownames(IOtable) <- tolower(paste(rownames(IOtable), CommodityCodeName[, 2], "US", sep = "/"))
  }
  if (length(unique(colnames(IOtable)%in%IndustryCodeName$Code)) == 1) {
    # Column name is Industry
    IndustryCodeName <- IndustryCodeName[IndustryCodeName$Code%in%colnames(IOtable), ]
    IndustryCodeName <- IndustryCodeName[order(factor(IndustryCodeName$Code, levels = colnames(IOtable))), ]
    colnames(IOtable) <- tolower(paste(colnames(IOtable), IndustryCodeName[, 2], "US", sep = "/"))
  } else {
    # Column name is Commodity
    CommodityCodeName <- CommodityCodeName[CommodityCodeName$Code%in%colnames(IOtable), ]
    CommodityCodeName <- CommodityCodeName[order(factor(CommodityCodeName$Code, levels = colnames(IOtable))), ]
    colnames(IOtable) <- tolower(paste(colnames(IOtable), CommodityCodeName[, 2], "US", sep = "/"))
  } 
  # Need code here to format state IO tables

  return(IOtable)
}

#' Format a dataframe having LCIA table structure for IOMB process.
#' Change row names of the dataframe to "code/names/locationcode".
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe, having LCIA table structure, with row names from "code" to "code/names/locationcode".
formatLCIAforIOMB <- function(model) {
  # Generate LCIA table
  lcia <- generateLCIA(model)
  # Load standard LCIA fields for IOMB
  lciafields <- utils::read.table(system.file("extdata", "IOMB_LCIA_fields.csv", package = "useeior"),
                                  sep = ",", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  # Change column names to match those in IOMB format
  colnames(lcia) <- c("Code", "Flow", "Compartment", "Sub-Compartment", "Unit", "Flow-UUID", "Amount", "Name", "Group", "Ref.Unit")
  # Add LCIA-Method column
  lcia[, "LCIA-Method"] <- "USEEIO-LCIA"
  # format to meet IOMB format
  formattedlcia <- lcia[, colnames(lciafields)]
  return(formattedlcia)
}

#' Format the sector meta data of a model for IOMB process.
#' @param model A complete EEIO model: a list with USEEIO model components and attributes.
#' @return A dataframe, having sector meta data table structure, with column names required by IOMB. 
formatSectorMetaDataforIOMB <- function (model) {
  sectormetadata <- model$SectorNames
  sectormetadata[, c("Category",	"Sub-category")] <- ""
  sectormetadata$Location <- model$specs$PrimaryRegionAcronym
  sectormetadata$Description <- ""
  return(sectormetadata)
}