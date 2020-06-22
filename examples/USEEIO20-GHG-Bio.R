#This is an example of using the new Bioeconomy function
#Start with clean
remove.packages("useeior")
#install
devtools::install_github("modelearth/useeior")


library(useeior)
useeior::seeAvailableModels()
model2 <- useeior::buildEEIOModel('USEEIOv2.0-GHG')
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
#inputPurchases<- c(model$Use[1:405, simSectorCode],0)
inputPurchases<- c(rep(100,300),rep(0,106))
colSim<-which(colnames(model2$Use)==simSectorCode)
#envVector<-model$B[,colSim] 
envVector<-rep(0,15) 
#Modify model
source("R/BioeconomyFunctions.R")
#debug(createBioeconomyModel)
model2<-createBioeconomyModel(model2,newSectorCode="324110B",newSectorName="LignoCelullosic Biofuels", similarSectorCode=simSectorCode,percentage=0.01, inputPurchases, newEnvData=envVector)

#Calculate model
result2 <- useeior::calculateEEIOModel(model2, perspective='DIRECT')

#for testing
result2$LCIA_d["324110B",1]
result2$LCIA_d["324110",1]
result$LCIA_d["324110",1]

#Print results
# Everything is updated to use this function. The only "exception" is that we are not modifying the satellite tables directly, so we cannot update them but
# in consequence, the file that ends in "_sat.csv" doesn't make sense.
useeior::writeModelComponents(model2)
