## model format

| Item | Type | Description |
| --- | --- | --------- |
| specs | metadata | [A list of USEEIO model specifications](#model-specifications-format) |
| crosswalk | table | The crosswalk table for a given model, including 1 NAICS code column and 3 BEA code columns |
| Commodities | table | The commodity code and name table<sup>1</sup> for a given model |
| Industries | table | The industry code and name table<sup>1</sup> for a given model |
| FinalDemandSectors | table | The final demand sector code and name table<sup>1</sup> for a given model |
| MarginSectors | table | The margin sector code and name table<sup>1</sup> for a given model |
| MakeTransactions | matrix | The Make (industry x commodity) matrix for a given model |
| UseTransactions | matrix | The Use (commodity x industry) matrix for a given model |
| DomesticUseTransactions | matrix | The domestic Use (commodity x industry) matrix for a given model |
| UseValueAdded | matrix | The value added (sector x industry) matrix for a given model |
| FinalDemand | matrix | The final demand (commodity x sector) matrix for a given model |
| DomesticFinalDemand | matrix | The domestic final demand (commodity x sector) matrix for a given model |
| IndustryOutput | vector | The industry output for a given model |
| CommodityOutput | vector | The commodity output for a given model |
| MultiYearIndustryOutput | table | The multi-year industry output table for a given model |
| MultiYearCommodityOutput | table | The multi-year commodity output table for a given model |
| FinalConsumerMargins | table | The final consumer margins table for a given model |
| MultiYearIndustryCPI | table | The multiyear industry CPI<sup>2</sup> table for a given model |
| MultiYearCommodityCPI | table | The multiyear commodity CPI<sup>2</sup> table for a given model |
| DisaggregationSpecs | metadata | [The disaggregation specifications for a given model](#disaggregation-specifications-format) |
| SatelliteTables | table | [The satellite tables for a given model](#satellite-tables) |
| Indicators | table | [The indicators for a given model](#indicators) |
| DemandVectors | table | [The demand vectors for a given model](#demand-vectors) |
| V | matrix | The Make matrix for a given model |
| U | matrix | The Use matrix (including final demand and value added) for a given model |
| U_d | matrix | The domestic Use matrix (including final demand and value added) for a given model |
| A | matrix | The direct requirements matrix for a given model |
| A_d | matrix | The domestic direct requirements matrix for a given model |
| TbS | matrix | [The total Flow-by-Sector table for a given model](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md) |
| CbS | matrix | The Coefficient-by-Sector table for a given model |
| B | matrix | The direct emissions and resource use matrix for a given model |
| C | matrix | The characterization factor matrix for a given model |
| D | matrix | The direct impact matrix for a given model |
| L | matrix | The Leontief inverse matrix for a given model |
| L_d | matrix | The domestic Leontief inverse matrix for a given model |
| M | matrix | The total emissions and resource use matrix for a given model |
| M_d | matrix | The total emissions and resource use (from and by domestic activity) matrix for a given model |
| N | matrix | The total impact matrix for a given model |
| N_d | matrix | The total impact (from domestic activity) matrix for a given model |
| Rho | table | The CPI<sup>2</sup> price year ratio table for a given model|
| Phi | table | The producer over purchaser price ratio table for a given model|

<sup>1</sup> Including 1 code column, 1 name column and 1 code_location column

<sup>2</sup> Chain-type Price Index

## model specifications format

| Item | Type | Description |
| --- | --- | --------- |
| Model | metadata | The model name |
| BaseIOSchema | metadata | The base IO schema for a given model |
| BaseIOLevel | metadata | The base IO level of detail for a given model |
| IOYear | metadata | The base IO year for a given model |
| PrimaryRegionAcronym | metadata | The primary region acronym for a given model |
| ModelRegionAcronyms | metadata | The model region acronyms for a given model |
| ModelType | metadata | The model type |
| ModelSource | metadata | The model source |
| BasePriceType | metadata | The model base price type (producer or purchaser) |
| BasewithRedefinitions | metadata | Whether the model is based with redefinitions (yes or no) |
| CommoditybyIndustryType | metadata | The model commodity by industry type (commodity or industry) |
| ScrapIncluded | metadata | Whether the model includes scrap (yes or no) |
| DisaggregationSpecs | metadata | The disaggregation specifications for a given model |
| SatelliteTable | metadata | The satellite table specifications for a given model |
| Indicators | metadata | The indicator specifications for a given model |
| DemandVectors | metadata | The demand vector specifications for a given model |

## disaggregation specifications format

| Item | Type | Description |
| --- | --- | --------- |
| Disaggregation | metadata | The disaggregation object for a given model |

## satellite tables

| Item | Type | Description |
| --- | --- | --------- |
| totals_by_sector | table | [The total Flow-by-Sector table for a given model](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md) |
| flows | metadata | The flow specifications for a given model |

## indicators

| Item | Type | Description |
| --- | --- | --------- |
| meta | metadata | The indicator specifications for a given model |
| factors | table | The indicator factor for a given model |

## demand vectors

| Item | Type | Description |
| --- | --- | --------- |
| vectors | vector | The demand vectors for a given model |
| meta | metadata | The demand specifications for a given model |
