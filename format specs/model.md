## model format

| Item | Type | Description |
| --- | --- | --------- |
| specs | metadata | [A list of USEEIO model specifications](#model-specifications-format) |
| crosswalk | component | The crosswalk table for a given model |
| Commodities | component | The commodities for a given model |
| Industries | component | The industries for a given model |
| FinalDemandSectors | component | The final demand sectors for a given model |
| MarginSectors | component | The margins sectors for a given model |
| MakeTransactions | component | The Make (industry x commodity) matrix for a given model |
| UseTransactions | component | The Use (commodity x industry) matrix for a given model |
| DomesticUseTransactions | component | The domestic Use (commodity x industry) matrix sectors for a given model |
| UseValueAdded | component | The value added (sector x industry) matrix for a given model |
| FinalDemand | component | The final demand (commodity x sector) matrix for a given model |
| DomesticFinalDemand | component | The domestic final demand (commodity x sector) matrix for a given model |
| IndustryOutput | component | The industry output for a given model |
| CommodityOutput | component | The commodity output for a given model |
| MultiYearIndustryOutput | component | The multiyear industry output table for a given model |
| MultiYearCommodityOutput | component | The multiyear commodity output table for a given model |
| FinalConsumerMargins | component | The final consumer margins table for a given model |
| MultiYearIndustryCPI | component | The multiyear industry CPI (chain-type price index) table for a given model |
| MultiYearCommodityCPI | component | The multiyear commodity CPI (chain-type price index) table for a given model |
| DisaggregationSpecs | component | [The disaggregation specifications for a given model](#disaggregation-specifications-format) |
| SatelliteTables | component | [The satellite tables for a given model](#satellite-tables) |
| Indicators | component | [The indicators for a given model](#indicators) |
| DemandVectors | component | [The demand vectors for a given model](#demand-vectors) |
| C_m | component | The commodity mix matrix for a given model |
| V_n | component | The market share matrix for a given model |
| U_n | component | The normalized Use table for a given model |
| U_d_n | component | The normalized domestic Use table for a given model |
| W | component | The Value Added matrix for a given model |
| A | component | The direct requirements matrix for a given model |
| A_d | component | The domestic direct requirements matrix for a given model |
| TbS | component | [The total Flow-by-Sector table for a given model](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md) |
| CbS | component | The Coefficient-by-Sector table for a given model |
| B | component | The direct emissions and resource use matrix for a given model |
| C | component | The characterization factor matrix for a given model |
| D | component | The direct impact matrix for a given model |
| L | component | The Leontief inverse matrix for a given model |
| L_d | component | The domestic Leontief inverse matrix for a given model |
| M | component | The total emissions and resource use matrix for a given model |
| M_d | component | The total emissions and resource use (from and by domestic activity) matrix for a given model |
| N | component | The total impact matrix for a given model |
| N_d | component | The total impact (from domestic activity) matrix for a given model |
| Rho | component | The CPI price year ratio table for a given model|
| Phi | component | The producer over purchaser price ratio table for a given model|

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
| Disaggregation | component | The disaggregation object for a given model |

## satellite tables

| Item | Type | Description |
| --- | --- | --------- |
| totals_by_sector | component | [The total Flow-by-Sector table for a given model](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md) |
| flows | metadata | The flow specifications for a given model |

## indicators

| Item | Type | Description |
| --- | --- | --------- |
| meta | metadata | The indicator specifications for a given model |
| factors | component | The indicator factor for a given model |

## demand vectors

| Item | Type | Description |
| --- | --- | --------- |
| vectors | component | The demand vectors  for a given model |
| meta | metadata | The demand specifications for a given model |
