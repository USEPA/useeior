## Model Format
A fully constructed USEEIO model contains the following elements.

| Item | Type | Description |
| --- | --- | --------- |
| specs | list | [A list of USEEIO model specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md) |
| crosswalk | table | [The crosswalk table for a given model](#Crosswalk-format), including 1 NAICS code column and 3 BEA code columns (sector, summary, and detail) |
| Commodities | table | [Commodity name table](#Commodities-and-Industries-table-format) |
| Industries | table | [Industry name table](#Commodities-and-Industries-table-format) |
| FinalDemandSectors | table | [Final demand name table](#Final-Demand-table-format) |
| MarginSectors | table | [Margins name table](#Commodities-and-Industries-table-format) |
| ValueAddedSectors | table | [Value Added name table](#Commodities-and-Industries-table-format) |
| MakeTransactions | matrix | The Make (industry x commodity) matrix for a given model |
| UseTransactions | matrix | The Use (commodity x industry) matrix for a given model |
| DomesticUseTransactions | matrix | The domestic Use (commodity x industry) matrix for a given model |
| UseValueAdded | matrix | The value added (sector x industry) matrix for a given model |
| FinalDemand | matrix | The final demand (commodity x sector) matrix for a given model |
| DomesticFinalDemand | matrix | The domestic final demand (commodity x sector) matrix for a given model |
| IndustryOutput | vector | Total output by industry for a given model |
| CommodityOutput | vector | Total output by commodity for a given model |
| MultiYearIndustryOutput | table | The multi-year industry output table for a given model |
| MultiYearCommodityOutput | table | The multi-year commodity output table for a given model |
| Margins | table | [The final consumer margins table](#Margins-Specifications-format) for a given model |
| MultiYearIndustryCPI | table | The multiyear industry CPI<sup>1</sup> table for a given model |
| MultiYearCommodityCPI | table | The multiyear commodity CPI<sup>1</sup> table for a given model |
| DisaggregationSpecs | list | A list containing elements for one or more [disaggregations](https://github.com/USEPA/useeior/tree/master/format_specs/DisaggregationSpecifications.md) |
| SatelliteTables | table | [The satellite tables for a given model](#satellite-tables) |
| Indicators | table | [The indicators for a given model](#indicators) |
| DemandVectors | table | [The demand vectors for a given model](#demand-vectors) |
| V | matrix | The Make matrix for a given model |
| U | matrix | The Use matrix (including final demand and value added) for a given model |
| U_d | matrix | The domestic Use matrix (including final demand and value added) for a given model |
| A | matrix | The direct requirements matrix for a given model |
| A_d | matrix | The domestic direct requirements matrix for a given model |
| TbS | matrix | [The total Flow-by-Sector table across all satellite tables](#satellite-tables) |
| CbS | matrix | [The total Coefficient-by-Sector table across all satellite tables](#satellite-tables) |
| B | matrix | The direct emissions and resource use matrix for a given model |
| C | matrix | The characterization factor matrix for a given model |
| D | matrix | The direct impact matrix for a given model |
| L | matrix | The Leontief inverse matrix for a given model |
| L_d | matrix | The domestic Leontief inverse matrix for a given model |
| M | matrix | The total emissions and resource use matrix for a given model |
| M_d | matrix | The total emissions and resource use (from and by domestic activity) matrix for a given model |
| N | matrix | The total impact matrix for a given model |
| N_d | matrix | The total impact (from domestic activity) matrix for a given model |
| Rho | table | The CPI<sup>1</sup> price year ratio table for a given model|
| Phi | table | The producer over purchaser price ratio table for a given model|

<sup>1</sup> Chain-type Price Index

## Crosswalk format

| Item | Type | Description |
| --- | --- | --------- |
| NAICS | str | 6-digit [NAICS code](https://www.census.gov/naics/) |
| BEA_Sector | str | Code used at the BEA Sector level |
| BEA_Summary | str | Code used at the BEA Summary level |
| BEA_Detail | str | Code used at the BEA Detail level |

## Commodities and Industries table format

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Commodity or industry name |
| Code_Loc | str | Code plus location (e.g. `1111A0/US`) |

## Final Demand table format

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Final demand name |
| Group | str | Classification of final demand vector (e.g. Household) |
| Code_Loc | str | Code plus location (e.g. `F01000/US`) |

## Margins Specifications format

| Item | Type | Description |
| --- | --- | --------- |
| SectorCode | str | 6-digit code |
| ProducersValue | numeric |  |
| Transportation | numeric |  |
| Wholesale | numeric |  |
| Retail | numeric |  |
| Name | str | Sector name |
| Code_Loc | str | Code plus location (e.g. `1111A0/US`) |
| PurchasersValue | numeric | Sum of ProducersValue, Transportation, Wholesale, and Retail |

## Satellite Tables

totals_by_sector - list of dataframes, one for each satellite table, which contain the Flow-by-Sector table, based on the [flow-by-sector collapsed format of flowsa](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md#flow-by-sector-collapsed-format) with some fields removed. Also includes an additional field `SectorName`.

flows - the unique flows found across all satellite tables with fields sourced from the [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md)
| Item | Type | Description |
| --- | --- | --------- |
| Flowable | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Context | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Unit | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| FlowUUID | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |

## Indicators

meta - table of indicators included in the model

| Item | Type | Description |
| --- | --- | --------- |
| Name | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| Code | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| Group | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| Unit | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| SimpleUnit | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| SimpleName | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |

factors - table of indicator factors included in the model across all indicators
| Item | Type | Description |
| --- | --- | --------- |
| Indicator | str | Matches the Name of the indicator |
| Flowable | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Context | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Unit | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Amount | numeric | Characterization factor linking one unit of the flow to the indicator |

## Demand Vectors

meta - table of demand vectors included in the model
| Item | Type | Description |
| --- | --- | --------- |
| Type | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| Year | int | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| System | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| Location | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| Name | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| ID | str | Year_Location_Type_System |

vectors - list of demand vectors
