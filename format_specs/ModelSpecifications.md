# Model Specifications
Model specifications are assigned in a yml file based on the parameters shown below

| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Model | str | Y | The model name |
| BaseIOSchema | int | Y | The base IO schema for a given model (e.g. 2012) |
| BaseIOLevel | str | Y | The base IO level of detail for a given model (e.g. `Detail`) |
| IOYear | int | Y | The base IO year for a given model |
| PrimaryRegionAcronym | str | Y | The primary region acronym for a given model |
| ModelRegionAcronyms | list | Y | The model region acronyms for a given model |
| ModelType | str | Y | The model type |
| ModelSource | str | Y | The model source |
| BasePriceType | str | Y | The model base price type (producer or purchaser) |
| BasewithRedefinitions | bool | Y | Whether the model is based with redefinitions (TRUE or FALSE) |
| CommoditybyIndustryType | str | Y | The model commodity by industry type (`Commodity` or `Industry`) |
| ScrapIncluded | bool | Y | Whether the model includes scrap (TRUE or FALSE) |
| DisaggregationSpecs | str | N | The disaggregation specifications for a given model |
| SatelliteTable | list |  | The [satellite table specifications](#Satellite-Table-Specifications) for a given model |
| Indicators | list |  | The [indicator specifications](#Indicator-Specifications) for a given model |
| DemandVectors | list |  | The [demand vector specifications](#Demand-Vector-Specifications) for a given model |


## Satellite Table Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| FullName | str | Y | The model name |
| Abbreviation | str | Y | The model name |
| StaticSource | str | Y | The model name |
| StaticFile | str | Y | The model name |
| FileLocation | str | Y | The model name |
| DataYears | str | Y | The model name |
| Locations | str | Y | The model name |
| SectorListSource | str | Y | The model name |
| SectorListYear | str | Y | The model name |
| SectorListLevel | str | Y | The model name |
| OriginalFlowSource | str | Y | The model name |
| ScriptFunctionCall | str | Y | The model name |
| ScriptFunctionParameters | str | Y | The model name |
| DataSources | str | Y | The model name |


## Indicator Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Name | str | Y | The model name |
| Code | str | Y | The model name |
| Group | str | Y | The model name |
| Unit | str | Y | The model name |
| SimpleUnit | str | Y | The model name |
| SimpleName | str | Y | The model name |
| StaticSource | str | Y | The model name |
| StaticFile | str | Y | The model name |
| FileLocation | str | Y | The model name |
| ScriptFunctionCall | str | Y | The model name |
| ScriptFunctionParameters | str | Y | The model name |
| DataSources | str | Y | The model name |


## Demand Vector Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Type | str | Y | The model name |
| Year | str | Y | The model name |
| System | str | Y | The model name |
| Location | str | Y | The model name |


## DataSources Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Title | str | Y | The model name |
| Author | str | Y | The model name |
| DataYear | str | Y | The model name |
| URL | str | Y | The model name |
| Primary | str | Y | The model name |

