#This is an example of using the new Bioeconomy function
#Start with clean
remove.packages("useeior")
#install
devtools::install_github("modelearth/useeior")


library(useeior)
useeior::seeAvailableModels()

# For now, take one existing sector and use its values.
# This example with Biofuels
# newSectorName: "LignoCelullosic Biofuels"
# newSectorCode: "324110B"
# similarSectorCode: "324110"- BEA code- Petroleum refineries
# percentage: 1%
# Input purchases from commodities: 100 for the first 300 commodities and 0 for the rest
# New environmental data: 0 for all the environmental flows.
# This is an example with the createBioeconomyModel() inside buildEEIOModel().
model2 <- useeior::buildEEIOModel('USEEIOv2.0-GHG')

#Calculate model
result2 <- useeior::calculateEEIOModel(model2, perspective='DIRECT')

#Print results
# Everything is updated to use this function. The only "exception" is that we are not modifying the satellite tables directly, so we cannot update them but
# in consequence, the file that ends in "_sat.csv" doesn't make sense.
useeior::writeModelComponents(model2)
