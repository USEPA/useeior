# Disaggregation and Aggregation File Specifications
This page describes the file formats needed to run the aggregation and disaggregation functions in USEEIOR. Aggregating a sector requires only one .yml input file, while disaggregation requires a .yml input file and several .csv files to specifiy the disaggregation parameters. 

# Disaggreation and Aggregation .yml File Specification
The disaggregation and aggregation files are assigned in a yml file based on the parameters shown below. Each file is a list, named based on the [Code/location](https://github.com/USEPA/useeior/blob/master/format_specs/Model.md#sector-meta) of the sector to be disaggregated or aggregated to (e.g. `221100/US`)


## Disaggregation 


| Item | Type | Required? | Description |
| ---  | ---  | ---       | ---------   |
| OriginalSectorCode | str | Y | Code/location of the sector to be disaggregated |
| OriginalSectorName | str | Y | Name of the sector to be disaggregated |
| DisaggregationType | str | Y |  Can be "Predefined" for a uniform disaggregation or "User Defined" for a custom disaggregation with user-supplied inputs|
| SectorFile | str | Y | Pointer to a file containing [disaggregated sectors mapping](#disaggregated-sectors-format)  |
| MakeFile | str | N | Pointer to a file containing [make table allocations for disaggregated sectors](#disaggregated-table-format) |
| UseFile | str | N | Pointer to a file containing [use table allocations for disaggregated sectors](#disaggregated-table-format) |
| EnvFile | str | N | Pointer to a file containing satellite table data for [disaggregated sectors](#disaggregated-satellite-table-format) |

## Aggregation

| Item | Type | Required? | Description |
| ---  | ---  | ---       | ---------   |
| Sectors | str | Y | Bracketed list of strings of the Code/location of the sectors to be aggregated, with the first item in the list being the main aggregation sector (i.e., where all sectors will be aggregated to).  |


# Disaggregation Table Specifications
Input disaggregation tables are in csv format with the fields shown below. The Disaggregated Table format applies to both the Disaggregated Make and Disaggregated Use tables. 

## Disaggregated Sectors Format
This .csv file is a crosswalk between the most relevant 6-digit NAICS codes and the disaggregated sectors.


|  Field             |  Type   |  Required? |  Description                                 |
| ------------------ | ------- | ---------- | ---------------------------------------------|
|  NAICS\_2012\_Code |  string |  Y         |  NAICS 2012 6-digit code                     |
|  USEEIO\_Code      |  string |  Y         |  Code for new sector in the form of Code/location |
|  USEEIO\_Name      |  string |  Y         |  Name for new sector                         |
|  Category          |  string |  Y         |  2-digit NAICS Code:Description combination which the disaggregated sector would fall under (e.g., 22:Utilities for disaggregated electricity sector 221111/US)                                                              |
|  Subcategory       |  string |  Y         |  3- or 4-digit NAICS Code:Description combination which the disaggregated sector would fall under (e.g., 2211:Electric Power Generation, Transmission, and Distribution for disaggregated electricity sector 221111/US) |
|  Description       |  string |  Y         |  Description of the disaggregated sector     |

## Disaggregated Table Format
These .csv files contain the input values required for the disaggregation of the Make and Use tables. One file is required for each table.  

Field | Type | Required? | Description |
-- | -- | -- | -- |
IndustryCode | string | Y | Code/location for industry. Values included in this column can represent the original industry code to be disaggregated; the new industry codes representing disaggregated sectors; or existing industries where an allocation for a newly disaggregated commodity is to be assigned.  |
CommodityCode | string | Y | Code/location for commodity. Values included in this column can represent the original commodity code to be disaggregated; the new commodity codes representing disaggregated sectors; or existing commodities where an allocation for a newly disaggregated industry is to be assigned.  |
PercentAllocation | numeric | Y | Percent of sector output dedicated to the newly disaggregated sector. This column represents the allocation value for the specific industry/commodity combination specified by the IndustryCode and CommodityCode columns. All values must be between 0 and 1, to represent the percent allocated to the specific industry/commodity combination. |
Note | string | N |  This column contains short text strings describing the allocation performed in each row. The text is intended to describe how the allocation value is used in the disaggregation. |

## Disaggregated Satellite Table Format
Matches [totals-by-sector](https://github.com/USEPA/useeior/blob/master/format_specs/Model.md#totals_by_sector) but includes an additional field `SatelliteTable` indicating the satellite table to which each record applies.
