#' Model Configuration - Main attributes
#' @format A list of main model attributes.
#' \describe{
#'  \item{Model}{string, Name of the model using standard naming scheme}
#'  \item{BaseIOSchema}{numeric, value indicating yea of the base BEA IO schema, 2007 or 2012}
#'  \item{BaseIOLevel}{string, BEA IO levels. Options are "Detail","Summary", or "Sector"}
#'  \item{IOYear}{numeric, value indicatinbg year of IO data, e.g. 2012}
#'  \item{PrimaryRegionAcronym}{string, acronyms for main model region, e.g. "US"}
#'  \item{ModelRegionAcronyms}{list, acronyms for all model regions}
#'  \item{ModelType}{string, either "US" or "state" currently supported}
#'  \item{ModelSource}{string, only "BEA" currently supported}
#'  \item{BasePriceType}{string, base IO table price type, only "PRO" currently supported}
#'  \item{BasewithDefinitions}{boolean, TRUE if IO tables after redefinitions, FALSE if before}
#'  \item{CommodityorIndustryType}{string, "Commodity" or "Industry"}
#'  \item{ScrapIncluded}{boolean, TRUE if scrap is separate sector, false if not}
#'  \item{SatelliteTable}{named list of satellite table objects named by table acynonyms. @seealso [SatelliteTableConfiguration]}
#'  \item{Demand}{list. @seealso [DemandConfiguration]}
#'  \item{Indicators}{named list of indicators by indicator acronym.  @seealso [IndicatorConfiguration]}
#'  \item{DisaggregationSpecs}{string, name of YML file with disaggregation specs}
#'}
"ModelConfiguration"

#' Satellite Table Configuration
#' @format A list of satellite table attributes
#' \describe{
#'  \item{FullName}{string, Name of the satellite table}
#'  \item{Abbreviation}{string, Abbreviation for table}
#'  \item{StaticSource}{boolean, TRUE if loading table from static source}
#'  \item{StaticFile}{string, path to static file}
#'  \item{FileLocation}{string, description of the location of the source file}
#'  \item{DataYears}{list of data years}
#'  \item{Locations}{list of location acyronyms}
#'  \item{SectorListSource}{string, name of source "NAICS" or "BEA" accepted}
#'  \item{SectorListYear}{numeric, year of sector list schema, e.g. 2017}
#'  \item{SectorListLevel}{string, BEA IO levels. Options are "Detail","Summary", or "Sector"}
#'  \item{OriginalFlowSource}{string, name of original flow source for mapping}
#'  \item{DataSources}{list of data sources, @seealso [DataSourceConfiguration]}
#'}
"SatelliteTableConfiguration"

#' Demand Configuration
#' @format A list of demand attibutes
#' \describe{
#'  \item{DemandYear}{numeric, year of demand}
#'  \item{DemandSubsystems}{list of names of demand subsystems}
#' }
"DemandConfiguration"

#' Indicator Configuration
#' @format A list of indicator attributes
#' \describe{
#'  \item{Name}{string, Name of the indicator}
#'  \item{Code}{string, Abbreviation for the indicator}
#'  \item{Group}{string, Impact category of the indicator}
#'  \item{Unit}{string, unit of the indicator}
#'  \item{SimpleUnit}{string, simple unit of the indicator}
#'  \item{SimpleName}{string, simple name of the indicator}
#'  \item{StaticSource}{boolean, TRUE if this is a static source}
#'  \item{StaticFile}{string, name of the source file}
#'  \item{FileLocation}{string, description of the location of the source file}
#'  \item{ScriptFunctionCall}{string, the function used to process the indicator data}
#'  \item{ScriptFunctionParameters}{list, parameters passed to ScriptFunctionCall}
#'  \item{DataSources}{list of data sources, @seealso [DataSourceConfiguration]}
#'  }
"IndicatorConfiguration"

#' DataSourceConfiguration
#' @format A list of data source attributes
#' \describe{
#'  \item{Title}{string, Name of source},
#'  \item{Author}{string, name or orgs or individuals}
#'  \item{DataYear}{numeric, year of data},
#'  \item{URL}{string, URL data retrieved from}
#'  \item{Primary}{boolean, TRUE if this is the primary source}
#' }
"DataSourceConfiguration"

