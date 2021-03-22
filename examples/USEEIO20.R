# Example for running useeior, see https://github.com/USEPA/useeior for more information on the model
# Run ?functionname to see documentation and function input options for the functions in this example


library(useeior)

# See all available model names
# explanation of model names can be found at https://github.com/USEPA/USEEIO/blob/master/VersioningScheme.md
useeior::seeAvailableModels()

# Build an EEIO model
model <- buildModel("USEEIOv2.0")

# Calculate direct perspective LCI and LCIA
result <- useeior::calculateEEIOModel(model, perspective = "DIRECT", demand = "Consumption")

# Adjust N matrix (total environmental impacts per dollar) to 2018 dollar and purchaser price
adjusted_result <- adjustMultiplierPrice("N", currency_year = 2018, purchaser_price = TRUE, model)

# Write model for API
useeior::writeModelforAPI(model,"../useeio_api/")


