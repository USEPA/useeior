# Model Customization File Specifications
This page describes the file formats needed to customize models in useeior.
This includes models with aggregation and/or disaggregation of target sectors, integrated hybrid models hybridized with process LCI data, mixed unit models (which contain physical flow data), and waste input-output models.


# Disaggreation and Aggregation .yml File Specification
Aggregating a sector requires only one .yml input file, while disaggregation requires a .yml input file and several .csv files to specify the disaggregation parameters. 
The disaggregation and aggregation files are assigned in a yml file based on the parameters shown below. Each file is a list, named based on the [Code/location](Model.md#sector-meta) of the sector to be disaggregated or aggregated to (e.g. `221100/US`)


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
Matches [totals-by-sector](Model.md#totals_by_sector) but includes an additional field `SatelliteTable` indicating the satellite table to which each record applies.


# Hybridization File Specification
Model hybridization, e.g., with LCA data from unit processes is available via [Model Type: EEIO-IH](ModelSpecification.md#model-types).
This hybrid model type requires as input data normalized environmental and technical (i.e., supply chain) data.
Additional examples of integrated hybrid models can be found in the [Hybrid Input-Output (HIO) data repository](https://github.com/USEPA/HIO/tree/main/useeior).

| Item | Type | Required? | Description |
| ---  | ---  | ---       | ---------   |
| TechFile | str | N | Pointer to a file containing [technical inputs and outputs for hybrid sectors](#hybridization-technical-table-format) |
| EnvFile | str | N | Pointer to a file containing normalized satellite table data for [hybrid sectors](#hybridization-environmental-table-format) |

## Hybridization Technical Table Format

To match the style of the `A` matrix:

Rows and columns are sectors in the format of `code_loc`

To build the `A` matrix, the technological data frame requires the following fields:

 Field | Type | Required |  Note |
----------- |  ---- | ---------| -----  |
ProcessID | str | Y | ProcessID of the consuming process
ProcessName | str | Y |
ProcessUnit | str | Y | Reference flow
Location | str | Y | two-digit code, e.g., `US`
Amount | float | Y | Normalized per unit of reference flow
FlowID | str | Y | ProcessID of the flow being consumed, use [code_loc format](Model.md#sector-meta)
Flow | str | N | ProcessName of the flow being consumed
FlowUnit | str | N | FEDEFL nomenclature


## Hybridization Environmental Table Format

To match the style of the `B` matrix:

Rows are `flowable/context/unit` and columns are sector in the format of `code_loc`

To build the `B` matrix, the environmental data frame requires the following fields:

 Field | Type | Required |  Note |
----------- |  ---- | ---------| -----  |
ProcessID | str | Y | ProcessID of the source process
ProcessName | str | Y |
Location | str | Y | two-digit code, e.g., `US`
Amount | float | Y | Per unit of reference flow
Flowable | str | Y | FEDEFL nomenclature
Context | str | Y | FEDEFL nomenclature
Unit | str | Y | FEDEFL nomenclature
FlowUUID | str| Y | FEDEFL nomenclature

# Mixed Unit File Specification
Model hybridization which converts economic flows for target sectors into physical flows is available via [Model Type: MUIO](ModelSpecification.md#model-types).
This hybridization approach requires one .yml input file with one or more csv files that label specific sectors as mixed unit (using the MUIO tag) and price conversation data. 
Additional examples of mixed unit models can be found in the [Hybrid Input-Output (HIO) data repository](https://github.com/USEPA/HIO/tree/main/useeior).

|  Field             |  Type   |  Required? |  Description                                 |
| ------------------ | ------- | ---------- | ---------------------------------------------|
|  SectorType        |  string |  Y         |  e.g., `MUIO`                                |
|  Tag               |  string |  Y         |  Str tag used to identify the physical sector|
|  NAICS_2012_Code   |  string |  Y         |  NAICS 2012 6-digit code                     |
|  USEEIO_Code       |  string |  Y         |  Code for new sector in the form of Code/location |
|  USEEIO_Name       |  string |  Y         |  Name for new sector                         |
|  Category          |  string |  Y         |  2-digit NAICS Code:Description combination  |
|  Subcategory       |  string |  N         |  3- or 4-digit NAICS Code:Description combination  |
|  Description       |  string |  N         |  Description of the sector                   |
|  Price             |  numeric|  Y         |  Price conversion to target unit             |
|  Unit              |  string |  Y         |  Physical target unit                        |

# Waste Input Output File Specification
Model hybridization which appends physical waste flows and waste treatment processes via [Model Type: WIO](ModelSpecification.md#model-types).
This hybridization approach requires one .yml input file with one or more csv files that label specific sectors as: Waste Treatment Industries and Waste Treatment Commodities (economic sectors); as well as Waste Generation by Mass and Waste Generation by Treatment (physical sectors). 
Additional examples of mixed unit models can be found in the [Hybrid Input-Output (HIO) data repository](https://github.com/USEPA/HIO/tree/main/useeior).

## Waste Sectors Format

|  Field             |  Type   |  Required? |  Description                                 |
| ------------------ | ------- | ---------- | ---------------------------------------------|
|  SectorType        |  string |  Y         |  e.g., `WIO`                                |
|  USEEIO_Code       |  string |  Y         |  Code for new sector in the form of Code/location |
|  USEEIO_Name       |  string |  Y         |  Name for new sector                         |
|  Category          |  string |  Y         |  2-digit NAICS Code:Description combination  |
|  Subcategory       |  string |  N         |  3- or 4-digit NAICS Code:Description combination  |
|  Description       |  string |  N         |  Description of the sector                   |
|  Location          |  string |  N         |  two-digit code, e.g., `US`                  |
|  Unit              |  string |  Y         |  Physical target unit                        |

## Waste Make and Use Table Format

Field | Type | Required? | Description |
-- | -- | -- | -- |
IndustryCode | string | Y | in the form of Code/location |
CommodityCode | string | Y | in the form of Code/location |
Amount | numeric | Y |  |
Unit   | str | Y |  |
WIO Section | str | N |  |
Note | string | N |   |

## Import Emission Factors

Specifications for import emission factors file, e.g.,

```
ImportFactors:
  StaticFile: "useeior/US_summary_import_factors_exio_2019_17sch.csv"
  FileLocation: "DataCommons"
```

