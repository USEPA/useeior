# Model and Model Component Formats

A fully constructed USEEIO model is an R named list that contains the following members described in the [Model](#model) table. The format members or components are further described in separate tables. 
 
## Notes 
 
 A _sector_ is either a commodity or industry, depending on the [model CommodityorIndustry Type](https://github.com/USEPA/useeior/blob/master/format_specs/ModelSpecification.md#model-specifications). The _sector_ will be synononmous for that same CommodityorIndustryType for all tables in a given model in which _sector_ is used. 

Unless another year is specifically called out, all economic values are given in US dollars (USD) in the value the [model IOyear](https://github.com/USEPA/useeior/blob/master/format_specs/ModelSpecification.md#model-specifications). 

## Model
Items are listed in the order in which they appear in a built Model object in R.

| Item | Data Structure | Category | Description |
| --- | --- | --------- | ------ |
| specs | list | metadata | [Model specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md) |
| crosswalk | data.frame | metadata | Sector [crosswalk](#crosswalk) |
| Commodities | data.frame | metadata | Commodity metadata in [sector meta format](#sector-meta) |
| Industries | data.frame | metadata | Industry metadata in [sector meta format](#sector-meta) |
| FinalDemandMeta | data.frame | metadata | Final demand metadata in [sector meta with group format](#Sector-Meta-with-Group) |
| InternationalTradeAdjustmentMeta | data.frame | metadata | Metadata for international trade adjusment in [sector meta with group format](#Sector-Meta-with-Group) |
| MarginSectors | data.frame | metadata | Margin sector metadata in [sector meta format](#sector-meta) |
| ValueAddedMeta | data.frame | metadata | Value added metadata in [sector meta format](#sector-meta) |
| MultiYearIndustryOutput | data.frame | supporting data | Multi-year industry output in [sector-by-year format](#sector-by-year) |
| MultiYearCommodityOutput | data.frame | supporting data | Multi-year commodity output  in [sector-by-year format](#sector-by-year) |
| Margins | data.frame | supporting data | [The final consumer margins table](#margins) |
| MultiYearIndustryCPI | data.frame | supporting data | Multi-year industry CPI<sup>1</sup> in [sector-by-year format](#sector-by-year) |
| MultiYearCommodityCPI | data.frame |   supporting data | Multi-year commodity CPI<sup>1</sup> in [sector-by-year format](#sector-by-year) |
| AggregationSpecs | list | metadata | Specifications for one or more aggregations
| DisaggregationSpecs | list | metadata | Specifications for one or more [disaggregation](https://github.com/USEPA/useeior/tree/master/format_specs/ModelCustomization.md#disaggreation-and-aggregation-yml-file-specification) |
| HybridizationSpecs | list | metadata | Specifications for [model hybridization](https://github.com/USEPA/useeior/tree/master/format_specs/ModelCustomization.md#hybridization-file-specification) |
| SatelliteTables | list | component data | [Satellite tables](#satellitetables) |
| Indicators | list | component data | [Indicators](#indicators) |
| DemandVectors | list | component data | [Demand vectors](#demandvectors) |
| TbS | data.frame | component data | [The total Flow-by-Sector table across all satellite tables](#satellitetables) contains the direct emissions and resource use by industry |
| CbS | data.frame | component data | [The total Coefficient-by-Sector table across all satellite tables](#satellitetables) contains the direct emissions and resource use by industry per dollar output |
| V | matrix | component matrix |[The Make matrix](#V) |
| C_m | matrix | component matrix | [The Commodity Mix matrix ](#V)|
| V_n | matrix | component matrix | [The Market Shares matrix](#V) |
| U | matrix | component matrix | [The Use matrix](#U) |
| U_d | matrix | component matrix | [The domestic Use matrix](#U) |
| q | numeric vector | component matrix | [Total output by commodity](#output-vectors) |
| x | numeric vector | component matrix | [Total output by industry](#output-vectors) |
| mu | numeric vector | component matrix | [International trade adjustment by commodity](#international-trade-adjustment-vector) |
| A | matrix | component matrix | [The direct requirements matrix](#A) |
| A_d | matrix | component matrix | [The domestic direct requirements matrix](#A) |
| L | matrix | component matrix | [The Leontief inverse matrix](#L) |
| L_d | matrix | component matrix | [The domestic Leontief inverse matrix](#L) |
| B | matrix | component matrix | [The direct emissions and resource use matrix](#B) |
| C | matrix | component matrix | [The characterization factor matrix](#C) |
| D | matrix | result matrix | [The direct impact matrix](#D) |
| M | matrix | result matrix | [The total emissions and resource use matrix](#M) |
| M_d | matrix |  result matrix | [The total emissions and resource use (from and by domestic activity) matrix](#M) |
| N | matrix | result matrix | [The total impact matrix](#N) |
| N_d | matrix | result matrix | [The total impact (from domestic activity) matrix](#N) |
| Rho | matrix | component matrix | [The CPI<sup>1</sup> price year ratio matrix for a given model](#Rho)|
| Phi | matrix | component matrix | [The producer-to-purchaser price ratio matrix for a given model](#Phi)|

<sup>1</sup> Chain-type Price Index

### Sector by year 
| Item | Format |
| --- | ------- |
| sector | [Code_Loc](#sector-meta) (e.g. `1111A0/US`) |
| year | year (e.g. 2002) |

### Crosswalk

| Item | Type | Description |
| --- | --- | --------- |
| NAICS | str | 2-6 digit [NAICS code](https://www.census.gov/naics/)<sup>2</sup> |
| BEA_Sector | str | Code used at the BEA Sector level |
| BEA_Summary | str | Code used at the BEA Summary level |
| BEA_Detail | str | Code used at the BEA Detail level |
| USEEIO | str | Codes used by the model |

<sup>2</sup> 7-10 digit NAICS code exists for manufacturing and mining industries.

### Sector Meta

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Name of sector or component |
| Code_Loc | str | Code joined with a location acronym by a forward slash (e.g. `1111A0/US`) |
| Unit | str | Typically `USD` for EEIO model types but can include physical units for hybrid models |

Commodity sector meta may also have the following fields:

| Item | Type | Description |
| --- | --- | --------- |
| Category | str | A 2 digit NAICS code and name |
| Subcategory | str | A 4 digit NAICS code and name |
| Description | str | A description of the sector |

### Sector Meta with Group

A [sector meta table](#sector-meta) with an additional group field.

| Item | Type | Description |
| --- | --- | --------- |
| Group | str | Grouping of sector (e.g. Household) | N |

### Margins

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

### SatelliteTables
The SatelliteTables object contains a totals_by_sector list and a flows dataframe.

#### totals_by_sector
 A named list of dataframes, one for each satellite table, which contain the Flow-by-Sector table, based on the [flow-by-sector collapsed format of flowsa](https://github.com/USEPA/flowsa/blob/master/format%20specs/FlowBySector.md#flow-by-sector-collapsed-format) with some fields removed. Also includes an additional field `SectorName`.
Each dataframe has the following format:

Item | Type | Description
----- | ---- | ---- |
Flowable | str | See [flows](#flows)
Context | str | See [flows](#flows)
FlowUUID | str | See [flows](#flows)
SectorName | str | Same as Name as in [Sector Meta](#sector-meta)
Sector | str | Same as Code as in [Sector Meta](#sector-meta)
Location | str | A location acronym
FlowAmount | numeric | The numeric amount of the flow 
Unit | str | SI unit acronym. 'kg' for mass flows; 'MJ' for energy flows.
DistributionType | str | The form of the frequency distribution, if given. Acceptable values are 'NORMAL', 'LOGNORMAL', 'TRIANGULAR', 'UNIFORM'.
Min | numeric | The minimum FlowAmount, if provided for the data range. 
Max | numeric | The maximum FlowAmount, if provided for the data range.
DataReliability | numeric | A score of data reliability based on reporting values associated with the amount. See [Data Quality Pedigree Matrix](https://github.com/USEPA/flowsa/blob/master/DataQualityPedigreeMatrix.md)
TemporalCorrelation |  numeric | A 1-5 score of data collection based on reporting values associated with the amount. See [Data Quality Pedigree Matrix](https://github.com/USEPA/flowsa/blob/master/DataQualityPedigreeMatrix.md).
GeographicalCorrelation |  numeric | A 1-5 score of data collection based on reporting values associated with the amount. See [Data Quality Pedigree Matrix](https://github.com/USEPA/flowsa/blob/master/DataQualityPedigreeMatrix.md).
TechnologicalCorrelation |  numeric | A 1-5 score of data collection based on reporting values associated with the amount. See [Data Quality Pedigree Matrix](https://github.com/USEPA/flowsa/blob/master/DataQualityPedigreeMatrix.md).
DataCollection | numeric | A 1-5 score of data collection based on reporting values associated with the amount. See [Data Quality Pedigree Matrix](https://github.com/USEPA/flowsa/blob/master/DataQualityPedigreeMatrix.md).
Year | int | Year of data, e.g. `2010`
MetaSources | str | The major data source(s) value is based on.

#### flows
The unique flows found across all satellite tables with fields sourced from the [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) format of the Federal LCA Commons Elementary Flow List.

| Item | Type | Description |
| --- | --- | --------- |
| Flowable | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Context | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Unit | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| FlowUUID | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |

### Indicators
The Indicators object contains meta and factors dataframes.

#### meta
 A table of metadata for the indicators included in the model.

| Item | Type | Description |
| --- | --- | --------- |
| Name | str | See [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) |
| Code | str | See [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) |
| Group | str | See [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) |
| Unit | str | See [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) |
| SimpleUnit | str | See [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) |
| SimpleName | str | See [Indicator Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) |

#### factors
 A data table of the characterization factors for indicators included in the model

| Item | Type | Description |
| --- | --- | --------- |
| Indicator | str | Matches the [Name](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) of the indicator |
| Flowable | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Context | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Unit | str | See [FlowList](https://github.com/USEPA/Federal-LCA-Commons-Elementary-Flow-List/blob/master/format%20specs/FlowList.md) |
| Amount | numeric | Characterization factor linking one unit of the flow to the indicator |

## DemandVectors

The DemandVector object contains the demand vectors and a metadata table.

#### vectors
 A data table of the demand vectors included in the model. Each vector is in the format of a named vector with names assigned in [Code_Loc](#sector-meta) (e.g. `1111A0/US`) format.
 
#### meta
 A table of metadata for the demand vectors included in the model.

| Item | Type | Description |
| --- | --- | --------- |
| Type | str | See [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#demand-vector-specifications) |
| Year | int | See [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#demand-vector-specifications) |
| System | str | See [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#demand-vector-specifications) |
| Location | str | See [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#demand-vector-specifications) |
| Name | str | See [Demand Vector Specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#demand-vector-specifications) |
| ID | str | A unique ID for that demand vector|

### Model Component Matrices

When used in matrix indices, items below take the following format:

| Item | Format |
| --- | --------- |
| sector (commodity or industry) | [Code_Loc](#sector-meta) (e.g. `1111A0/US`) |
| flow | [Flowable/Context/Unit](#satellite-tables) (e.g. `Carbon dioxide/emission/air/kg`) |
| indicator | [Name](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecification.md#indicator-specifications) (e.g. `Greenhouse Gases`) |


#### Output vectors

`q` is a commodity output vector and `x` is an industry output vector containing economic output in model year US dollars. 

```
commodities +----q----+

industries +----x----+
```
#### International Trade Adjustment vector

`mu` is an international trade adjustment vector containing value of all transportation and insurance services to import and customs duties in model year US dollars.

```
commodities +----mu----+
```

#### V
The Make matrix, `V`, is an `industry x commodity` matrix with amounts in commodities in year USD being made by industries.
```
           commodities
            +-------+
 industries |       |
            |  Make |
            +-------+
```

The Market Shares matrix, `V_n`, is a `q` normalized form of `V` also in `industry x commodity` format.
The Commodity Mix matrix, `C_m`, is an `x` normalized and transposed form of `V` in `commodity x industry` format.

#### U
The Use matrix, `U`, is a `commodity x industry` matrix with total amounts in model year USD of commodities being used by industries for intermediate production, or being used by final consumers. 
`U` also includes commodity imports, exports and change in inventories as components of final demand, and value added components as inputs to industries.
```
                    industries, final demand
                    +----------------------+
commodities,        |                      |
value added         |         Use          |
                    +----------------------+
```
The related `U_d` matrix provides commodity and value added use totals by industries and final demand that are only from the US.

#### A
The matrix `A` is a `sector x sector` matrix and contains in each column `i` the direct sector inputs that are required to produce 1 USD dollar of output from
sector `i`:

```
         sectors
        +-------+
sectors |       |
        |     A |
        +-------+
```

The related `A_d` matrix provides direct sector inputs per dollar sector output that are only from the US.

#### L
`L` is also a `sector x sector` matrix and contains in each column `i` the
total requirements of the respective sectors inputs per 1 USD of output
from sector `i`:

```
         sectors
        +-------+
sectors |       |
        |     L |
        +-------+
```

The related `L_d` matrix provides direct + indirect sector inputs per dollar output that are only from the US.

#### B
The satellite matrix `B` is a `flow x sector` matrix and contains in
each column `i` the amount of a flow given in the reference
units of the respective flow per 1 USD output from sector `i`:

```
       sectors
      +-------+
flows |       |
      |     B |
      +-------+
```

#### C
In the matrix `C`, each column `k` contains the characterization factors of
the different indicators related to one reference unit of flow `k`:

```
             flows
           +-------+
indicators |       |
           |     C |
           +-------+
```

#### Rho, 
`Rho` is a `sector x year` matrix and contains in each column `y` the price year ratios. Rho ratios are in the form of model IO_year/year (where year is the column).

```
        years
      +-------+
flows |       |
      |   Rho |
      +-------+
```

#### Phi

`Phi` is also a `sector x year` matrix and contains in each column `y` producer-to-purchaser price ratios. Phi ratios are year-specific ratios in the form of value in producer price/value in purchaser price.

```
        years
      +-------+
flows |       |
      |   Phi |
      +-------+
```

### Model Result Matrices


#### D
The matrix `D` contains in each column `i` the direct impact result per USD output from sector `i`:

```
                 sectors
                +-------+
indicators      |       |
                |     D |
                +-------+
```

#### M
The matrix `M` is a `flow x sector` matrix and contains in each column
`i` the direct and indirect emissions and resources per 1 USD output of sector `i`:

```
                 sectors
                +-------+
flows           |       |
                |     M |
                +-------+
```

The related `M_d` matrix provides direct + indirect emissions and resources per dollar output that are only from the US.

#### N
The matrix `N` is a `indicator x sector` matrix and contains in each column
`i` the direct and indirect impact result per 1 USD output of sector `i`:

```
                 sectors
                +-------+
indicators      |       |
                |     N |
                +-------+
```

The related `N_d` matrix provides direct + indirect impact results per dollar output that are only from the US.
