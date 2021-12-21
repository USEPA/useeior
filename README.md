# useeior
[![R CI/CD test](https://github.com/USEPA/useeior/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/useeior/actions/workflows/R-CMD-check.yaml)

`useeior` is an R package for building and using [USEEIO models](https://www.epa.gov/land-research/us-environmentally-extended-input-output-useeio-models). The [model object](format_specs/Model.md) is the primary output that is built according to a given [model specification](format_specs/ModelSpecification.md) and optional hybridization specification, e.g. [disaggregation](format_specs/DisaggregationAndAggregationSpecification.md). [Model specifications](inst/extdata/modelspecs) and associated hybridization specifications, e.g. [disaggregation](inst/extdata/disaggspecs), for EPA-validated models are included in the package. The package offers various functions for validating, calculating, visualizing, and writing out models and/or their components. `useeior` is a core component of the [USEEIO Modeling Framework](https://github.com/USEPA/useeio).

`useeior` is in a stable development state. Users intending to use the package for production purposes and applications should use [Releases](https://github.com/USEPA/useeior/releases).

See the following sections for installation and basic usage of `useeior`. See [Wiki](https://github.com/USEPA/useeior/wiki) for advanced uses, details about built-in data and metadata and how to contribute to `useeior`.

## Installation

```
# Install development version from GitHub
devtools::install_github("USEPA/useeior")
```

```
# Install a previously released version (e.g. v1.0) from GitHub
devtools::install_github("USEPA/useeior@v1.0")
```

See [Releases](https://github.com/USEPA/useeior/releases) for all previously realeased versions.

## Usage

### Build Model

View all models with existing config files that can be built using useeior

```
useeior::seeAvailableModels()
```

Build a model that is available in useeior (e.g. the latest USEEIOv2.0.1 model)

```
model <- useeior::buildModel('USEEIOv2.0.1')
```

To build a customized model, refer to [Advanced Uses](https://github.com/USEPA/useeior/wiki/Using-useeior#advanced-uses) in Wiki.

This generates a complete USEEIO model with components described in the [Model](format_specs/Model.md#model) table.

### Adjust Price Year and Type of Model Results

Adjust model results (e.g. `N` matrix) to user-specified price year (e.g. `2018`) and type (producer's or purchaser's).

```
N_adj <- useeior::adjustResultMatrixPrice("N", 
                                          currency_year = 2018,
                                          purchaser_price = TRUE,
                                          model)
```

### Calculate Model LCI and LCIA

Calculate model life cycle inventory (LCI) and life cycle impact assessment (LCIA) results under user-specified perspective, demand vector (from DemandVectors in the model object or a user-provided vector) and a selected direct requirements matrix (complete or domestic).

```
result <- useeior::calculateEEIOModel(model,
                                      perspective = "DIRECT",
                                      demand = "CompleteProduction",
                                      use_domestic_requirements = FALSE)
```

### Write Model Results to File

Write selected model matrices, demand vectors, and metadata as one `.xlsx` file to a given output folder.
```
useeior::writeModeltoXLSX(model, outputfolder)
```

Write model matrices as `.csv` files to a given output folder.
```
useeior::writeModelMatrices(model, to_format = "csv", outputfolder)
```

### Validate Model (examples)

Validate that flow totals by commodity `E_c` can be recalculated (within 1%) using the model satellite matrix `B`, market shares matrix `V_n`, total requirements matrix `L`, and demand vector `y` for US production.

```
> modelval <- compareEandLCIResult(model, tolerance = 0.01)
> print(paste("Number of flow totals by commodity passing:", modelval$N_Pass))
[1] "Number of flow totals by commodity passing: 1118742"
> print(paste("Number of flow totals by commodity failing:", modelval$N_Fail))
[1] "Number of flow totals by commodity failing: 0"
```

Validate that commodity output can be recalculated (within 1%) with the model total requirements matrix `L` and demand vector `y` for US production.

```
> econval <- compareOutputandLeontiefXDemand(model, tolerance = 0.01)
> print(paste("Number of sectors passing:",econval$N_Pass))
[1] "Number of sectors passing: 409"
> print(paste("Number of sectors failing:",econval$N_Fail))
[1] "Number of sectors failing: 2"
> print(paste("Sectors failing:", paste(econval$Failure$rownames, collapse = ", ")))
[1] "Sectors failing: S00402/US, S00300/US"
```
Note: `S00402/US - Used and secondhand goods` and `S00300/US - Noncomparable imports` are two commodities that are not produced by any industry in the US, therefore their commodity output naturally cannot recalculated with the model total requirements matrix `L` and demand vector `y` for US production.


### Visualize Model Results


A complete list of available functions for calculating, validating, exporting and visualizing model can be found [here](https://github.com/USEPA/useeior/wiki/Using-useeior#calculate-validate-export-visualize-model) in the Wiki.

## Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information.  Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
