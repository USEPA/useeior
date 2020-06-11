#This is an example of using the new Bioeconomy function

library(useeior)
useeior::seeAvailableModels()
model <- useeior::buildEEIOModel('USEEIOv2.0-GHG')
#Define the input purchases vector sna newEnvironmentalDataVector
#' For now, take one existing sector and use its values.
#' This example with Biofuels
#' newSectorName: "LignoCelullosic Biofuels"
#' newSectorCode: "324110B"
#' similarSectorCode: "324110"- BEA code- Petroleum refineries
#' percentage: 20%
#' Input purchases from commodities: Same as similar sector.
#' New environmental data: Same as similar sector.

# Obtain Input purchases from similar sector
simSectorCode<-"324110"
inputPurchases<- model$Use[1:405, simSectorCode]
colSim<-which(colnames(model$Use)==simSectorCode)
envVector<-model$B[,colSim] 

#Modify model
source("R/BioeconomyFunctions.R")
debug(createBioeconomyModel)
createBioeconomyModel(model,newSectorCode="324110B",newSectorName="LignoCelullosic Biofuels", similarSectorCode=simSectorCode,percentage=0.2, inputPurchases, newEnvData=envVector)



result <- useeior::calculateEEIOModel(model, perspective='DIRECT')
useeior::writeModelComponents(model)
