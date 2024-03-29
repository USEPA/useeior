---
title: "Compare USEEIOv2.0.1-411 and USEEIOv2.1-422 Model"
date: "2021-12-30"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

This document presents comparison results of USEEIOv2.0.1-411 and USEEIOv2.1-422 model.

#### Compare flow totals between two models

```r
mA <- buildModel(modelname_pair[1])
#> 2021-12-30 13:31:34 INFO::Begin model initialization...
#> 2021-12-30 13:31:34 INFO::Initializing IO tables...
#> 2021-12-30 13:31:35 INFO::Initializing Gross Output tables...
#> 2021-12-30 13:31:37 INFO::Initializing Chain Price Index tables...
#> 2021-12-30 13:31:37 INFO::Loading disaggregation specification file for WasteDisaggregationDetail...
#> 2021-12-30 13:31:38 INFO::Initializing Disaggregation of IO tables...
#> 2021-12-30 13:31:38 INFO::Initializing model satellite tables...
#> 2021-12-30 13:31:38 INFO::Loading Water withdrawals flows from DataCommons...
#> 2021-12-30 13:31:39 INFO::Loading Criteria and Hazardous Air Emissions flows from DataCommons...
#> 2021-12-30 13:31:55 INFO::Loading Point source industrial releases to ground flows from DataCommons...
#> 2021-12-30 13:31:55 INFO::Loading Point source releases to water flows from DataCommons...
#> 2021-12-30 13:31:57 INFO::Loading Greenhouse Gases flows from DataCommons...
#> 2021-12-30 13:31:58 INFO::Loading Land use flows from DataCommons...
#> 2021-12-30 13:31:58 INFO::Loading Mineral extraction flows from DataCommons...
#> 2021-12-30 13:31:58 INFO::Loading Energy extraction flows from DataCommons...
#> 2021-12-30 13:31:58 WARNING::No data found for disaggregation of ENERGY for 562000/US - applying default allocation
#> 2021-12-30 13:31:58 INFO::Loading Nitrogen and Phosphorus Releases from Agriculture flows from DataCommons...
#> 2021-12-30 13:31:58 INFO::Loading Pesticide releases flows from DataCommons...
#> 2021-12-30 13:31:58 INFO::Loading Commercial non-hazardous waste excluding construction activities flows from DataCommons...
#> 2021-12-30 13:31:59 WARNING::No data found for disaggregation of CNHW for 562000/US - applying default allocation
#> 2021-12-30 13:32:00 INFO::Loading Commercial non-hazardous waste from construction activities flows from DataCommons...
#> 2021-12-30 13:32:00 INFO::Loading Commercial RCRA-defined hazardous waste flows from DataCommons...
#> 2021-12-30 13:32:06 INFO::Loading Employment flows from DataCommons...
#> 2021-12-30 13:32:06 INFO::Generating Value Added flows...
#> 2021-12-30 13:32:07 INFO::Initializing model indicators...
#> 2021-12-30 13:32:07 INFO::Getting Greenhouse Gases indicator from DataCommons...
#> 2021-12-30 13:32:08 INFO::Getting Acidification Potential indicator from DataCommons...
#> 2021-12-30 13:32:08 INFO::Getting Eutrophication Potential indicator from DataCommons...
#> 2021-12-30 13:32:08 INFO::Getting Freshwater Ecotoxicity Potential indicator from DataCommons...
#> 2021-12-30 13:32:14 INFO::Getting Human Health - Cancer indicator from DataCommons...
#> 2021-12-30 13:32:16 INFO::Getting Human Health - Noncancer indicator from DataCommons...
#> 2021-12-30 13:32:18 INFO::Getting Human Health Toxicity indicator from DataCommons...
#> 2021-12-30 13:32:21 INFO::Getting Human Health - Respiratory Effects indicator from DataCommons...
#> 2021-12-30 13:32:22 INFO::Getting Ozone Depletion indicator from DataCommons...
#> 2021-12-30 13:32:22 INFO::Getting Smog Formation Potential indicator from DataCommons...
#> 2021-12-30 13:32:23 INFO::Getting Freshwater withdrawals indicator from DataCommons...
#> 2021-12-30 13:32:24 INFO::Getting Land use indicator from DataCommons...
#> 2021-12-30 13:32:24 INFO::Getting Hazardous Air Pollutants indicator from DataCommons...
#> 2021-12-30 13:32:24 INFO::Getting Pesticides indicator from DataCommons...
#> 2021-12-30 13:32:25 INFO::Getting Nonrenewable Energy Use indicator from DataCommons...
#> 2021-12-30 13:32:25 INFO::Getting Renewable Energy Use indicator from DataCommons...
#> 2021-12-30 13:32:25 INFO::Getting Energy Use indicator from DataCommons...
#> 2021-12-30 13:32:26 INFO::Getting Minerals and Metals Use indicator from DataCommons...
#> 2021-12-30 13:32:26 INFO::Getting Value Added indicator from useeior...
#> 2021-12-30 13:32:26 INFO::Getting Jobs Supported indicator from useeior...
#> 2021-12-30 13:32:26 INFO::Getting Commercial RCRA Hazardous Waste indicator from useeior...
#> 2021-12-30 13:32:26 INFO::Getting Commercial Municipal Solid Waste indicator from useeior...
#> 2021-12-30 13:32:27 INFO::Getting Commercial Construction and Demolition Debris indicator from useeior...
#> 2021-12-30 13:32:27 INFO::Loading demand vectors ...
#> 2021-12-30 13:32:27 INFO::Loading CompleteProduction demand vector...
#> 2021-12-30 13:32:27 INFO::Loading DomesticProduction demand vector...
#> 2021-12-30 13:32:27 INFO::Loading CompleteConsumption demand vector...
#> 2021-12-30 13:32:27 INFO::Loading DomesticConsumption demand vector...
#> 2021-12-30 13:32:34 INFO::Building commodity-by-commodity A matrix (direct requirements)...
#> 2021-12-30 13:32:34 INFO::Building commodity-by-commodity A_d matrix (domestic direct requirements)...
#> 2021-12-30 13:32:34 INFO::Calculating L matrix (total requirements)...
#> 2021-12-30 13:32:34 INFO::Calculating L_d matrix (domestic total requirements)...
#> 2021-12-30 13:32:34 INFO::Building B matrix (direct emissions and resource use per dollar)...
#> 2021-12-30 13:32:39 INFO::Building C matrix (characterization factors for model indicators)...
#> 2021-12-30 13:32:50 INFO::Calculating D matrix (direct environmental impacts per dollar)...
#> 2021-12-30 13:32:50 INFO::Calculating M matrix (total emissions and resource use per dollar)...
#> 2021-12-30 13:32:50 INFO::Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...
#> 2021-12-30 13:32:51 INFO::Calculating N matrix (total environmental impacts per dollar)...
#> 2021-12-30 13:32:51 INFO::Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...
#> 2021-12-30 13:32:51 INFO::Calculating Rho matrix (price year ratio)...
#> 2021-12-30 13:32:51 INFO::Calculating Phi matrix (producer over purchaser price ratio)...
#> 2021-12-30 13:32:51 INFO::Model build complete.
mB <- buildModel(modelname_pair[2])
#> 2021-12-30 13:32:51 INFO::Begin model initialization...
#> 2021-12-30 13:32:51 INFO::Initializing IO tables...
#> 2021-12-30 13:32:51 INFO::Initializing Gross Output tables...
#> 2021-12-30 13:32:53 INFO::Initializing Chain Price Index tables...
#> 2021-12-30 13:32:54 INFO::Loading aggregation specification file for ElectricityAggregationDetail...
#> 2021-12-30 13:32:54 INFO::Initializing Aggregation of IO tables...
#> 2021-12-30 13:32:54 INFO::Loading disaggregation specification file for ElectricityDisaggregationDetail...
#> 2021-12-30 13:32:54 INFO::Loading disaggregation specification file for WasteDisaggregationDetail...
#> 2021-12-30 13:32:55 INFO::Initializing Disaggregation of IO tables...
#> 2021-12-30 13:32:56 INFO::Initializing model satellite tables...
#> 2021-12-30 13:32:56 INFO::Loading Water withdrawals flows from DataCommons...
#> 2021-12-30 13:32:57 INFO::Loading Criteria and Hazardous Air Emissions flows from DataCommons...
#> 2021-12-30 13:33:21 INFO::Loading Point source industrial releases to ground flows from DataCommons...
#> 2021-12-30 13:33:21 INFO::Loading Point source releases to water flows from DataCommons...
#> 2021-12-30 13:33:24 INFO::Loading Greenhouse Gases flows from DataCommons...
#> 2021-12-30 13:33:25 INFO::Loading Land use flows from DataCommons...
#> 2021-12-30 13:33:26 INFO::Loading Mineral extraction flows from DataCommons...
#> 2021-12-30 13:33:26 INFO::Loading Energy extraction flows from DataCommons...
#> 2021-12-30 13:33:26 WARNING::No data found for disaggregation of ENERGY for 562000/US - applying default allocation
#> 2021-12-30 13:33:26 INFO::Loading Nitrogen and Phosphorus Releases from Agriculture flows from DataCommons...
#> 2021-12-30 13:33:26 INFO::Loading Pesticide releases flows from DataCommons...
#> 2021-12-30 13:33:26 INFO::Loading Commercial non-hazardous waste excluding construction activities flows from DataCommons...
#> 2021-12-30 13:33:27 WARNING::No data found for disaggregation of CNHW for 562000/US - applying default allocation
#> 2021-12-30 13:33:28 INFO::Loading Commercial non-hazardous waste from construction activities flows from DataCommons...
#> 2021-12-30 13:33:28 INFO::Loading Commercial RCRA-defined hazardous waste flows from DataCommons...
#> 2021-12-30 13:33:34 INFO::Loading Employment flows from DataCommons...
#> 2021-12-30 13:33:34 INFO::Generating Value Added flows...
#> 2021-12-30 13:33:35 INFO::Initializing model indicators...
#> 2021-12-30 13:33:35 INFO::Getting Greenhouse Gases indicator from DataCommons...
#> 2021-12-30 13:33:36 INFO::Getting Acidification Potential indicator from DataCommons...
#> 2021-12-30 13:33:36 INFO::Getting Eutrophication Potential indicator from DataCommons...
#> 2021-12-30 13:33:36 INFO::Getting Freshwater Ecotoxicity Potential indicator from DataCommons...
#> 2021-12-30 13:33:43 INFO::Getting Human Health - Cancer indicator from DataCommons...
#> 2021-12-30 13:33:44 INFO::Getting Human Health - Noncancer indicator from DataCommons...
#> 2021-12-30 13:33:46 INFO::Getting Human Health Toxicity indicator from DataCommons...
#> 2021-12-30 13:33:50 INFO::Getting Human Health - Respiratory Effects indicator from DataCommons...
#> 2021-12-30 13:33:50 INFO::Getting Ozone Depletion indicator from DataCommons...
#> 2021-12-30 13:33:51 INFO::Getting Smog Formation Potential indicator from DataCommons...
#> 2021-12-30 13:33:52 INFO::Getting Freshwater withdrawals indicator from DataCommons...
#> 2021-12-30 13:33:52 INFO::Getting Land use indicator from DataCommons...
#> 2021-12-30 13:33:52 INFO::Getting Hazardous Air Pollutants indicator from DataCommons...
#> 2021-12-30 13:33:53 INFO::Getting Pesticides indicator from DataCommons...
#> 2021-12-30 13:33:53 INFO::Getting Nonrenewable Energy Use indicator from DataCommons...
#> 2021-12-30 13:33:53 INFO::Getting Renewable Energy Use indicator from DataCommons...
#> 2021-12-30 13:33:53 INFO::Getting Energy Use indicator from DataCommons...
#> 2021-12-30 13:33:53 INFO::Getting Minerals and Metals Use indicator from DataCommons...
#> 2021-12-30 13:33:54 INFO::Getting Value Added indicator from useeior...
#> 2021-12-30 13:33:54 INFO::Getting Jobs Supported indicator from useeior...
#> 2021-12-30 13:33:54 INFO::Getting Commercial RCRA Hazardous Waste indicator from useeior...
#> 2021-12-30 13:33:54 INFO::Getting Commercial Municipal Solid Waste indicator from useeior...
#> 2021-12-30 13:33:55 INFO::Getting Commercial Construction and Demolition Debris indicator from useeior...
#> 2021-12-30 13:33:55 INFO::Loading demand vectors ...
#> 2021-12-30 13:33:55 INFO::Loading CompleteProduction demand vector...
#> 2021-12-30 13:33:55 INFO::Loading DomesticProduction demand vector...
#> 2021-12-30 13:33:55 INFO::Loading CompleteConsumption demand vector...
#> 2021-12-30 13:33:55 INFO::Loading DomesticConsumption demand vector...
#> 2021-12-30 13:33:55 INFO::Loading HouseholdConsumption demand vector...
#> 2021-12-30 13:34:03 INFO::Building commodity-by-commodity A matrix (direct requirements)...
#> 2021-12-30 13:34:03 INFO::Building commodity-by-commodity A_d matrix (domestic direct requirements)...
#> 2021-12-30 13:34:03 INFO::Calculating L matrix (total requirements)...
#> 2021-12-30 13:34:03 INFO::Calculating L_d matrix (domestic total requirements)...
#> 2021-12-30 13:34:03 INFO::Building B matrix (direct emissions and resource use per dollar)...
#> 2021-12-30 13:34:10 INFO::Building C matrix (characterization factors for model indicators)...
#> 2021-12-30 13:34:21 INFO::Calculating D matrix (direct environmental impacts per dollar)...
#> 2021-12-30 13:34:21 INFO::Calculating M matrix (total emissions and resource use per dollar)...
#> 2021-12-30 13:34:21 INFO::Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...
#> 2021-12-30 13:34:22 INFO::Calculating N matrix (total environmental impacts per dollar)...
#> 2021-12-30 13:34:22 INFO::Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...
#> 2021-12-30 13:34:22 INFO::Calculating Rho matrix (price year ratio)...
#> 2021-12-30 13:34:22 INFO::Calculating Phi matrix (producer over purchaser price ratio)...
#> 2021-12-30 13:34:22 INFO::Model build complete.
```


```r
# Compare flow totals
model_com <- compareFlowTotals(mA, mB)
cat(paste("Number of flow totals by commodity passing:",model_com$N_Pass))
```

Number of flow totals by commodity passing: 2722

```r
cat(paste("Number of flow totals by commodity failing:",model_com$N_Fail))
```

Number of flow totals by commodity failing: 0

```r
#cat(paste("Sectors with flow totals failing:", paste(model_com$Failure$rownames, collapse = ", ")))
```

There are flow differences between USEEIOv2.0.1-411 and USEEIOv2.1-422 

Flows in USEEIOv2.0.1-411 not in USEEIOv2.1-422 are

character(0)

 Flows in USEEIOv2.1-422 not in USEEIOv2.0.1-411 are

character(0)

