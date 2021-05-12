# Disaggregation File Specifications
Disaggregation lists are named based on the Code_Loc of the original sector (e.g. `562000/US`)

| Item | Type | Required? | Description |
| --- | --- | --------- |
| OriginalSectorCode | str | Y |  |
| OriginalSectorName | str | Y |  |
| DisaggregationType | str | Y |  |
| SectorFile | str | Y |  |
| MakeFile | str | N |  |
| UseFile | str | N |  |
| EnvFile | str | N |  |


## Disaggregated Sectors Format
| Field | Type | Required? | Description |
| --- | --- | --- | ---
| NAICS_2012_Code | string | Y | NAICS 2012 6-digit code |
| USEEIO_Code | string | Y | Code for new sector |
| USEEIO_Name | string | Y | Name for new sector |

## Disaggregated Make Format
Field | Type | Required? | Description |
-- | -- | -- | -- |
IndustryCode | string | Y | Code/location for industry |
CommodityCode | string | Y | Code/location for commodity |
PercentUsed | numeric | Y | Percent of industry output dedicated to production of the given commodity |
Note | string | N |  |

## Disaggreated Use Format
Field | Type | Required? | Description |
-- | -- | -- | -- |
IndustryCode | string | Y | Code/location for industry |
CommodityCode | string | Y | Code/location for commodity |
PercentUsed | numeric | Y | Percent use of commodity among dissaggregated industries |
Note | string | N | |

##Disaggregated Satellite Table Format
Matches [totals-by-sector](https://github.com/USEPA/useeior/tree/master/format_specs/model.md#Satellite-Tables) but includes an additional field `SatelliteTable` indicating the satellite table to which each record applies.
