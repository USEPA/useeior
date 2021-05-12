## Model Format
A fully constructed USEEIO model contains the following elements. Items listed as data.frame are table-like objects with field headers specified in subsequent tables. Matrices use indices as shown in the [Matrix Indeces format](#matrix-indeces-format)

| Item | Type | Description |
| --- | --- | --------- |
| specs | list | [A list of USEEIO model specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md) |
| crosswalk | data.frame | [The crosswalk table for a given model](#Crosswalk-format), including 1 NAICS code column and 3 BEA code columns (sector, summary, and detail) |
| Commodities | data.frame | [Commodity name table](#Commodities-and-Industries-table-format) |
| Industries | data.frame | [Industry name table](#Commodities-and-Industries-table-format) |
| FinalDemandSectors | data.frame | [Final demand name table](#Final-Demand-table-format) |
| MarginSectors | data.frame | [Margins name table](#Commodities-and-Industries-table-format) |
| ValueAddedSectors | data.frame | [Value Added name table](#Commodities-and-Industries-table-format) |
| x | vector | Total output by industry for a given model |
| q | vector | Total output by commodity for a given model |
| MultiYearIndustryOutput | data.frame | The multi-year (2002-2018) industry output table |
| MultiYearCommodityOutput | data.frame | The multi-year (2002-2018) commodity output table |
| Margins | data.frame | [The final consumer margins table](#Margins-table-format) |
| MultiYearIndustryCPI | data.frame | The multi-year (2002-2018) industry CPI<sup>1</sup> table |
| MultiYearCommodityCPI | data.frame | The multi-year (2002-2018) commodity CPI<sup>1</sup> table |
| DisaggregationSpecs | list | A list containing elements for one or more [disaggregations](https://github.com/USEPA/useeior/tree/master/format_specs/DisaggregationSpecifications.md) |
| SatelliteTables | list | [The satellite tables for a given model](#satellite-tables) |
| Indicators | list | [The indicators for a given model](#indicators) |
| DemandVectors | list | [The demand vectors for a given model](#demand-vectors) |
| TbS | data.frame | [The total Flow-by-Sector table across all satellite tables](#satellite-tables) contains the direct emissions and resource use by industry |
| CbS | data.frame | [The total Coefficient-by-Sector table across all satellite tables](#satellite-tables) contains the direct emissions and resource use by industry per dollar output |
| V | matrix | The Make matrix (industry x commodity)  |
| U | matrix | The Use matrix (commodity x industry) (including final demand and value added) |
| U_d | matrix | The domestic Use matrix (including domestic final demand and value added) |
| A | matrix | The direct requirements matrix (sector x sector) |
| A_d | matrix | The domestic direct requirements matrix |
| B | matrix | The direct emissions and resource use matrix (flow x sector) |
| C | matrix | The characterization factor matrix (indicator x flow) |
| D | matrix | The direct impact matrix (indicator x sector) |
| L | matrix | The Leontief inverse matrix (sector x sector) |
| L_d | matrix | The domestic Leontief inverse matrix |
| M | matrix | The total emissions and resource use matrix (flow x sector) |
| M_d | matrix | The total emissions and resource use (from and by domestic activity) matrix |
| N | matrix | The total impact matrix (indicator x sector) |
| N_d | matrix | The total impact (from domestic activity) matrix |
| Rho | data.frame | The CPI<sup>1</sup> price year ratio table |
| Phi | data.frame | The producer over purchaser price ratio table |

<sup>1</sup> Chain-type Price Index

## Matrix Indeces format
When used in matrix indeces, items below take the following format:
| Item | Format |
| --- | --------- |
| sector (commodity or industry) | [Code_Loc](#commodities-and-industries-format) (e.g. `1111A0/US`) |
| flow | [Flowable/Context/Unit](#satellite-tables) (e.g. `Carbon dioxide/emission/air/kg`) |
| indicator | [Name](#indicators) (e.g. `Greenhouse Gases`) |

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

## Margins table format

| Item | Type | Description |
| --- | --- | --------- |
| SectorCode | str | 6-digit sector code |
| ProducersValue | numeric | Producer's value |
| Transportation | numeric | Transportation delivery cost |
| Wholesale | numeric | Wholesale trade margin |
| Retail | numeric | Retail trade margin  |
| Name | str | Sector name |
| Code_Loc | str | Code plus location (e.g. `1111A0/US`) |
| PurchasersValue | numeric | Purchaser's value (sum of ProducersValue, Transportation, Wholesale, and Retail) |

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
| Indicator | str | Matches the [Name](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) of the indicator |
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
