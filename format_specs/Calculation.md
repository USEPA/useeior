## Calculation Result
A result object from a Model calculation (`calculateEEIOModel()`) contains an LCI and an LCIA matrix. The matrices have a suffix appended to them in the form `_x`, where `x` indicates the calculation perspective that was used to prepare them. 

| Matrix | Shape | Description | 
| --- | ---- | ---|
| LCI |  sector x flow | Direct + indirect resource use and emissions by sector |  
| LCIA |  sector x indicator | Direct + indirect impacts by sector |

## Calculation Perspectives
|Suffix |Perspective|Definition|
|---|---|---|
|_d|DIRECT|Associates results with sector where emissions occur.|
|_f|FINAL|Associates results with sector is a final consumer. Final consumption is use by a final demand component.|
