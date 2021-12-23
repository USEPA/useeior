## Model Result Format
A result object from a USEEIO model contains one or more of the following elements.

| Item | Type | Shape | Description | 
| --- | --- | --------- | ---|
| LCI_d | matrix | sector x flow | Direct + indirect resource use and emissions by sector calculated with the [DIRECT perspective](#calculation-perspectives) |  
| LCIA_d | matrix | sector x indicator | Direct + indirect impacts by sector calculated with the [DIRECT perspective]((#calculation-perspectives))|
| LCI_f | matrix | Direct + indirect resource use and emissions by sector calculated with the [FINAL perspective](#calculation-perspectives) |
| LCIA_f | matrix | sector x indicator | Direct + indirect impacts by sector calculated with the [FINAL perspective](#calculation-perspectives)|


### Calculation Perspectives
|Perspective|Definition|
|---|---|
|DIRECT|Associates results with sector where emissions occur.|
|FINAL|Associates results with sector is a final consumer. Final consumption is use by a final demand component.|
