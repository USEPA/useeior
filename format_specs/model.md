## Model Format
A fully constructed USEEIO model contains the following elements. Items listed as data.frame are table-like objects with field headers specified in subsequent tables. Matrices use indices as shown in the [Matrix Indeces format](#matrix-indeces-format)

| Item | Type | Description |
| --- | --- | --------- |
| specs | list | A list of USEEIO [model specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md) |
| crosswalk | data.frame | [The crosswalk table](#crosswalk-format) |
| Commodities | data.frame | [The commodity name table](#commodities-and-industries-table-format) |
| Industries | data.frame | [The industry name table](#commodities-and-industries-table-format) |
| FinalDemandSectors | data.frame | [The final demand name table](#final-demand-table-format) |
| MarginSectors | data.frame | [The margins name table](#commodities-and-industries-table-format) |
| ValueAddedSectors | data.frame | [The value added name table](#value-added-table-format) |
| MultiYearIndustryOutput | data.frame | [The multi-year industry output table](#multi-year-table-format) |
| MultiYearCommodityOutput | data.frame | [The multi-year commodity output table](#multi-year-table-format) |
| Margins | data.frame | [The final consumer margins table](#margins-table-format) |
| MultiYearIndustryCPI | data.frame | [The multi-year industry CPI<sup>1</sup> table](#multi-year-table-format) |
| MultiYearCommodityCPI | data.frame | [The multi-year commodity CPI<sup>1</sup> table](#multi-year-table-format) |
| DisaggregationSpecs | list | A list containing elements for one or more [disaggregations](https://github.com/USEPA/useeior/tree/master/format_specs/DisaggregationSpecifications.md) |
| SatelliteTables | list | [The satellite tables of resource use and emissions](#satellite-tables) |
| Indicators | list | [The indicators for calculating impacts or aggregate resource use](#indicators) |
| DemandVectors | list | [The demand vectors](#demand-vectors) |
| TbS | data.frame | [The total Flow-by-Sector table across all satellite tables](#satellite-tables) contains the direct emissions and resource use by industry |
| CbS | data.frame | [The total Coefficient-by-Sector table across all satellite tables](#satellite-tables) contains the direct emissions and resource use by industry per dollar output |
| V | matrix | [The Make matrix](#make-matrix-format) |
| C_m | matrix | The Commodity Mix matrix (commodity x industry, transpose of [Make matrix](#make-matrix-format)) |
| V_n | matrix | The Market Shares matrix (industry x commodity, same with [Make matrix](#make-matrix-format)) |
| U | matrix | [The Use matrix](#use-matrix-format) |
| U_d | matrix | [The domestic Use matrix](#use-matrix-format) |
| q | numeric vector | Total output by commodity |
| x | numeric vector | Total output by industry |
| A | matrix | The direct requirements matrix |
| A_d | matrix | The domestic direct requirements matrix |
| L | matrix | The Leontief inverse matrix |
| L_d | matrix | The domestic Leontief inverse matrix |
| B | matrix | The direct emissions and resource use matrix |
| C | matrix | The characterization factor matrix |
| D | matrix | The direct impact matrix |
| M | matrix | The total emissions and resource use matrix |
| M_d | matrix | The total emissions and resource use (from and by domestic activity) matrix |
| N | matrix | The total impact matrix |
| N_d | matrix | The total impact (from domestic activity) matrix |
| Rho | data.frame | The CPI<sup>1</sup> price year ratio table for a given model|
| Phi | data.frame | The producer over purchaser price ratio table for a given model|

<sup>1</sup> Chain-type Price Index

## Multi-year table format
Rows (sector) and columns (year) in multi-year table tabke the following format:

| Item | Format |
| --- | --------- |
| sector (commodity or industry) | [Code_Loc](#commodities-and-industries-table-format) (e.g. `1111A0/US`) |
| year | 2002-2018 |

## Matrix Indices format
When used in matrix indices, items below take the following format:

| Item | Format |
| --- | --------- |
| sector (commodity or industry) | [Code_Loc](#commodities-and-industries-table-format) (e.g. `1111A0/US`) |
| flow | [Flowable/Context/Unit](#satellite-tables) (e.g. `Carbon dioxide/emission/air/kg`) |
| indicator | [Name](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) (e.g. `Greenhouse Gases`) |

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

## Value Added table format

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Final demand name |
| Code_Loc | str | Code plus location (e.g. `V00100/US`) |

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

**totals_by_sector** - list of dataframes, one for each satellite table, which contain the Flow-by-Sector table, based on the [flow-by-sector collapsed format of flowsa](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md#flow-by-sector-collapsed-format) with some fields removed. Also includes an additional field `SectorName`.

**flows** - the unique flows found across all satellite tables with fields sourced from the [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md)

| Item | Type | Description |
| --- | --- | --------- |
| Flowable | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Context | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Unit | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| FlowUUID | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |

## Indicators

**meta** - table of indicators included in the model

| Item | Type | Description |
| --- | --- | --------- |
| Name | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| Code | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| Group | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| Unit | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| SimpleUnit | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |
| SimpleName | str | [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) |

**factors** - table of indicator factors included in the model across all indicators

| Item | Type | Description |
| --- | --- | --------- |
| Indicator | str | Matches the [Name](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) of the indicator |
| Flowable | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Context | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Unit | str | [Federal Elementary Flow List](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Amount | numeric | Characterization factor linking one unit of the flow to the indicator |

## Demand Vectors

**meta** - table of demand vectors included in the model

| Item | Type | Description |
| --- | --- | --------- |
| Type | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| Year | int | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| System | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| Location | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| Name | str | [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#demand-vector-specifications) |
| ID | str | Year_Location_Type_System |

**vectors** - list of demand vectors

## Make matrix format

```
           commodities
            +-------+
 industries |       |
            |  Make |
            +-------+
```
[commodties and industries](#commodities-and-industries-table-format) are in `Code_Loc` format

## Use matrix format

```
    industries, final demand
            +-------+
commodities |       |
value added |   Use |
            +-------+
```
[commodties, industries](#commodities-and-industries-table-format), [final demand](#final-demand-table-format) and [value added](#value-added-table-format) are in `Code_Loc` format
