## Model Result Format
A result object from a USEEIO model contains one or more of the following elements.

| Item | Type | Description |
| --- | --- | --------- |
| LCI_d | matrix | Direct resource use and emissions by sector (sector x flow). Calculated from _B_ |
| LCIA_d | matrix | Direct impacts by sector (sector x indicator). Calculated from _D_ |
| LCI_f | matrix | Total (final) resource use and emissions by sector (sector x flow). Calculated from _M_ |
| LCIA_f | matrix | Total (final) impacts by sector (sector x indicator). Calculated from _N_ |

