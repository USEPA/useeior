## Calculation Result
A result object from a Model calculation (`calculateEEIOModel()`) contains an LCI (G) and an LCIA (H) matrix. The matrices have a suffix appended to them in the form `_x`, where `x` indicates the calculation perspective that was used to prepare them. 

| Matrix | Shape | Description | 
| --- | ---- | ---|
| G |  sector x flow | Direct + indirect resource use and emissions by sector (LCI) |  
| H |  sector x indicator | Direct + indirect impacts by sector (LCIA) |

## Calculation Perspectives
|Suffix |Perspective|Definition|
|---|---|---|
|_r|DIRECT|Associates results with sector where emissions occur.|
|_l|FINAL|Associates results with sector is a final consumer. Final consumption is use by a final demand component.|
