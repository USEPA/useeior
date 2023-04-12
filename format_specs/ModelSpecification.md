# Model Specification
Model specifications are assigned in a yml file based on the parameters shown below

| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Model | str | Y | The model name |
| BaseIOSchema | int | Y | The base IO schema (e.g. 2012) |
| BaseIOLevel | str | Y | The base IO level of detail (e.g. `Detail`) |
| IOYear | int | Y | The base IO year |
| PrimaryRegionAcronym | str | Y | The primary region acronym |
| ModelRegionAcronyms | list | Y | The model region acronyms |
| ModelType | str | Y | The [model type](#Model-Types) (e.g. `EEIO`) |
| ModelSource | str | Y | The model source |
| BasePriceType | str | Y | The model base price type (producer or purchaser) |
| BasewithRedefinitions | bool | Y | Whether the model is based with redefinitions (TRUE or FALSE) |
| CommodityorIndustryType | str | Y | Define if this is a commodity x commodity (`Commodity`) or industry x industry (`Industry`) model |
| ScrapIncluded | bool | Y | Whether the model includes scrap (TRUE or FALSE) |
| DisaggregationSpecs | str | N | The [disaggregation specifications](ModelCustomization.md#disaggregation) |
| HybridizationSpecs | str | N | The [hybridization specifications](ModelCustomization.md#hybridization-file-specification) |
| SatelliteTable | list |  | The [satellite table specifications](#Satellite-Table-Specifications) |
| Indicators | list |  | The [indicator specifications](#Indicator-Specifications) |
| DemandVectors | list |  | The [demand vector specifications](#Demand-Vector-Specifications) |

## Model Types
- EEIO: (default) Environmentally Extended Input Output
- EEIO-IH: Integrated hybrid model


## Satellite Table Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| FullName | str | Y | The name of the satellite table |
| Abbreviation | str | Y | Abbreviation used for the satellite table |
| StaticSource | bool | Y | If TRUE, indicates the use of a static source file. If FALSE the data will be generated dynamically |
| StaticFile | str | N | Required if StaticSource = TRUE, indicates the path of the source file |
| FileLocation | str | Y | The location of the source file. Options are 'DataCommons' or 'useeior'. If StaticSource = FALSE, set to `None` |
| DataYears | list | Y | Years reprsented by flows included in the satellite table |
| Locations | list | Y | The model name |
| SectorListSource | str | Y | The source category used for sectors in the satellite table (e.g. 'BEA' or 'NAICS') |
| SectorListYear | int | Y | The year of the SectorListSource |
| SectorListLevel | str | Y | The level of detail for the SectorListSource (e.g. 'Detail' or '6' for 6-digit NAICS) |
| OriginalFlowSource | str | Y | Source list for flow names. If not 'FEDEFL', used to identify appropriate flow mapping |
| ScriptFunctionCall | str | N | Function name for additional processing of satellite table |
| ScriptFunctionParameters | list | N | Parameters for ScriptFunctionCall |
| DataSources | list | N | Metadata for the satellite table [sources](#datasources-specifications) |


## Indicator Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Name | str | Y | The indicator name |
| Code | str | Y | The indicator code |
| Group | str | Y | The type of indicator (e.g. 'Impact Potential', 'Resource Use','Chemical Releases','Economic & Social') |
| Unit | str | Y | Unit for the indicator |
| SimpleUnit | str | Y |  |
| SimpleName | str | Y |  |
| StaticSource | bool | Y | If TRUE, indicates the use of a static source file. If FALSE the data will be generated dynamically |
| StaticFile | str | N | Required if StaticSource = TRUE, indicates the path of the source file |
| FileLocation | str | Y | The location of the source file. Options are 'DataCommons' or 'useeior'. |
| ScriptFunctionCall | str | N | Function name for additional processing of indicator |
| ScriptFunctionParameters | list | N | Parameters for ScriptFunctionCall |
| DataSources | list | N | Metadata for the indicator [sources](#datasources-specifications) |


## Demand Vector Specifications
Default demand vectors include `CompleteProduction`, `DomesticProduction`, `CompleteConsumption`, and `DomesticConsumption`.

Customized demand vector list is given a unique `Name`, such as `HouseholdConsumption`.

| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Type | str | Y |  |
| Year | int | Y (customized), N (default) | Use model IO data year (default) |
| System | str | Y |  |
| Location | str | Y (customized), N (default) | Use model region acronym (default) |

## DataSources Specifications
| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| Title | str | Y | Title of the source |
| Author | str | Y | Authors of the source |
| DataYear | int | Y | Year of publication |
| URL | str | Y | Source URL |
| Primary | bool | Y |  |

