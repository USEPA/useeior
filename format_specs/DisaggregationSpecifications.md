# Disaggregation File Specifications
Disaggregation lists are named based on the Code/location of the original sector (e.g. `562000/US`)

| Item | Type | Required? | Description |
| --- | --- | --- | --------- |
| OriginalSectorCode | str | Y | Code/location of the sector to be disaggregated |
| OriginalSectorName | str | Y | Name of the sector to be disaggregated |
| DisaggregationType | str | Y |  |
| SectorFile | str | Y | Pointer to a file containing [disaggregated sectors mapping](#disaggregated-sectors-format)  |
| MakeFile | str | N | Pointer to a file containing [make table allocations for disaggregated sectors](#disaggregated-make-format) |
| UseFile | str | N | Pointer to a file containing [use table allocations for disaggregated sectors](#disaggregated-use-format) |
| EnvFile | str | N | Pointer to a file containing satellite table data for [disaggregated sectors](#disaggregated-satellite-table-format) |


## Disaggregated Sectors Format
| Field | Type | Required? | Description |
| --- | --- | --- | ---
| NAICS_2012_Code | string | Y | NAICS 2012 6-digit code |
| USEEIO_Code | string | Y | Code for new sector in the form of Code/location |
| USEEIO_Name | string | Y | Name for new sector |

## Disaggregated Make Format
Field | Type | Required? | Description |
-- | -- | -- | -- |
IndustryCode | string | Y | Code/location for industry |
CommodityCode | string | Y | Code/location for commodity |
PercentMake | numeric | Y | Percent of industry output dedicated to production of the given commodity |
Note | string | N |  |

## Disaggregated Use Format
Field | Type | Required? | Description |
-- | -- | -- | -- |
IndustryCode | string | Y | Code/location for industry |
CommodityCode | string | Y | Code/location for commodity |
PercentUsed | numeric | Y | Percent use of commodity among dissaggregated industries |
Note | string | N | |

## Disaggregated Satellite Table Format
Matches [totals-by-sector](https://github.com/USEPA/useeior/tree/master/format_specs/model.md#Satellite-Tables) but includes an additional field `SatelliteTable` indicating the satellite table to which each record applies.
