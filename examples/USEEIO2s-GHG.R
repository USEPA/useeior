# Example for running useeior, see https://github.com/USEPA/useeior for more information on the model
# Must install useeior first to run example. See Wiki @ https://github.com/USEPA/useeior/Wiki for install instructions
# Run ?functionname to see documentation and function input options for the functions in this example

library(useeior)

# See all available model names
# explanation of model names can be found at https://github.com/USEPA/USEEIO/blob/master/VersioningScheme.md
seeAvailableModels()

# Build USEEIO v2.0-GHG
model <- buildModel("USEEIOv2.0s-GHG")

# Calculate direct perspective life cycle inventory (LCI) result and life cycle impact assessment (LCIA) 
# result using total US production
result <- calculateEEIOModel(model, perspective = "DIRECT", demand = "Production")

# Calculate a sector by sector result matrix for a specified indicator based on one of the demand
# vectors included in the model
y <- model[["DemandVectors"]][["vectors"]][["2012_US_Consumption_Complete"]]
impact_result <- calculateConsumptionContributiontoImpact(y, model, indicator = "Greenhouse Gases")

# Adjust N matrix (direct + indirect impacts per dollar) to 2018 dollar and purchaser price
N_adj <- adjustResultMatrixPrice("N", currency_year = 2012, purchaser_price = FALSE, model)

