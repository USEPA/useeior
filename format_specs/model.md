## Model Format
A fully constructed USEEIO model contains the following elements. The model itself is an R list. Items listed as data.frame are table-like objects with field headers specified in subsequent tables. A _sector_ is either a commodity or industry, depending on the model [](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md)

| Item | Data Structure | Category | Description |
| --- | --- | --------- | ------ |
| specs | list | metadata | USEEIO [model specifications](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md) |
| crosswalk | data.frame | metadata | [The Sector crosswalk](#crosswalk) |
| Commodities | data.frame | metadata | [The commodity metadata](#sector-meta) |
| Industries | data.frame | metadata | [The industry metadata](#sector-meta) |
| FinalDemandSectors | data.frame | metadata | [The final demand metadata](#final-demand) |
| MarginSectors | data.frame | metadata | [The margins metadata](#sector-meta) |
| ValueAddedSectors | data.frame | metadata | [The value added metadata](#value-added) |
| MultiYearIndustryOutput | data.frame | supporting data | [The multi-year industry output table](#multi-year-table-format) |
| MultiYearCommodityOutput | data.frame | supporting data | [The multi-year commodity output table](#multi-year-table-format) |
| Margins | data.frame | supporting data | [The final consumer margins table](#margins) |
| MultiYearIndustryCPI | data.frame | supporting data | [The multi-year industry CPI<sup>1</sup> table](#multi-year-table-format) |
| MultiYearCommodityCPI | data.frame | supporting data | [The multi-year commodity CPI<sup>1</sup> table](#multi-year-table-format) |
| DisaggregationSpecs | list | metadata | A list containing elements for one or more [disaggregations](https://github.com/USEPA/useeior/tree/master/format_specs/DisaggregationSpecifications.md) |
| SatelliteTables | list | component data | [The satellite tables of resource use and emissions](#satellite-tables) |
| Indicators | list | component data | [The indicators for calculating impacts or aggregate resource use](#indicators) |
| DemandVectors | list | component data | [The demand vectors](#demand-vectors) |
| TbS | data.frame | component data | [The total Flow-by-Sector table across all satellite tables](#satellite-tables) contains the direct emissions and resource use by industry |
| CbS | data.frame | component data | [The total Coefficient-by-Sector table across all satellite tables](#satellite-tables) contains the direct emissions and resource use by industry per dollar output |
| V | matrix | component matrix |[The Make matrix](#make-matrix-format) |
| C_m | matrix | component matrix | The Commodity Mix matrix (commodity x industry, transpose of [Make matrix](#make-matrix-format)) |
| V_n | matrix | component matrix | The Market Shares matrix (industry x commodity, same with [Make matrix](#make-matrix-format)) |
| U | matrix | component matrix | [The Use matrix](#use-matrix-format) |
| U_d | matrix | component matrix | [The domestic Use matrix](#use-matrix-format) |
| q | numeric vector | component matrix | Total output by commodity |
| x | numeric vector | component matrix | Total output by industry |
| A | matrix | component matrix | The direct requirements matrix |
| A_d | matrix | component matrix | The domestic direct requirements matrix |
| L | matrix | component matrix | The Leontief inverse matrix |
| L_d | matrix | component matrix | The domestic Leontief inverse matrix |
| B | matrix | result matrix | The direct emissions and resource use matrix |
| C | matrix | component matrix | The characterization factor matrix |
| D | matrix | result matrix | The direct impact matrix |
| M | matrix | result matrix | The total emissions and resource use matrix |
| M_d | matrix |  result matrix | The total emissions and resource use (from and by domestic activity) matrix |
| N | matrix | result matrix | The total impact matrix |
| N_d | matrix | result matrix | The total impact (from domestic activity) matrix |
| Rho | data.frame | component data | The CPI<sup>1</sup> price year ratio table for a given model|
| Phi | data.frame | component data | The producer-to-purchaser price ratio table for a given model|

<sup>1</sup> Chain-type Price Index

## Multi-year table format
Rows (sector) and columns (year) in multi-year table tabke the following format:

| Item | Format |
| --- | --------- |
| sector (commodity or industry) | [Code_Loc](#commodities-and-industries-table-format) (e.g. `1111A0/US`) |
| year | 2002-2018 |

## Matrix Indices

## Crosswalk

| Item | Type | Description |
| --- | --- | --------- |
| NAICS | str | 6-digit [NAICS code](https://www.census.gov/naics/) |
| BEA_Sector | str | Code used at the BEA Sector level |
| BEA_Summary | str | Code used at the BEA Summary level |
| BEA_Detail | str | Code used at the BEA Detail level |

## Sector Meta

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Commodity or industry name |
| Code_Loc | str | Code plus location (e.g. `1111A0/US`) |

## Final Demand Meta

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Final demand name |
| Group | str | Classification of final demand vector (e.g. Household) |
| Code_Loc | str | Code plus location (e.g. `F01000/US`) |

## Value Added Meta 

| Item | Type | Description |
| --- | --- | --------- |
| Code | str | 6-digit code |
| Name | str | Final demand name |
| Code_Loc | str | Code plus location (e.g. `V00100/US`) |

## Margins

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


## Model component matrices

When used in matrix indices, items below take the following format:

| Item | Format |
| --- | --------- |
| sector (commodity or industry) | [Code_Loc](#commodities-and-industries-table-format) (e.g. `1111A0/US`) |
| flow | [Flowable/Context/Unit](#satellite-tables) (e.g. `Carbon dioxide/emission/air/kg`) |
| indicator | [Name](https://github.com/USEPA/useeior/tree/master/format_specs/ModelSpecifications.md#indicator-specifications) (e.g. `Greenhouse Gases`) |


The Make matrix is an `industry x commodity` matrix where commodities and industries are `Code_Loc` format.
```
           commodities
            +-------+
 industries |       |
            |  Make |
            +-------+
```

The Use matrix is a `commodity x industry` matrix where commodities and industries are `Code_Loc` format.
```
    industries, final demand sectors
                    +-------+
commodities,        |       |
value added sectors |   Use |
                    +-------+
```

 The matrix `A` is a `sector x sector` matrix and contains in each column `i` the direct sector inputs that are required to produce 1 USD dollar of output from
sector `i`:

```
         sectors
        +-------+
sectors |       |
        |     A |
        +-------+
```

The related `A_d` matrix provide direct sector inputs per dollar output that are only from the US.

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

In the matrix `C`, each column `k` contains the characterization factors of
the different indicators related to one reference unit of flow `k`:

```
                  flows
                +-------+
indicators      |       |
                |     C |
                +-------+
```

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



#### Model Result Matrices

The matrix `D` contains in each column `i` the direct impact result per USD output from sector `i`:

```
                 sectors
                +-------+
indicators      |       |
                |     D |
                +-------+
```

The matrix `M` is a `flow x sector` matrix and contains in each column
`i` the direct and indirect emissions and resources per 1 USD output of sector `i`:

```
                 sectors
                +-------+
flows           |       |
                |     M |
                +-------+
```

The matrix `N` is a `indicator x sector` matrix and contains in each column
`i` the direct and indirect impact result per 1 USD output of sector `i`:

```
                 sectors
                +-------+
indicators      |       |
                |     N |
                +-------+
```

#### Data quality matrices
There are also 3 data quality matrices associated with three of the matrices above.
Each matrix consists of data quality etnries for values in the same position of its associated matrix.
 These entries are in the form of 5 data quality scores (values for each ranging from 1 to 5) for the five
 EPA LCA flow data quality indicators in this order: (Data reliability, Temporal correlation, Technological correlation,
         Technological correlation). For example '(3,1,1,4,5)'.

`B_dqi` provides data quality scores for the `B` matrix.

`D_dqi` provides data quality scores for the `D` matrix.

`N_dqi` provides data quality scores for the `N` matrix.

