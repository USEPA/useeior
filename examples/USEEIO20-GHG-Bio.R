#This is an example of using the new Bioeconomy function
#Start with clean
remove.packages("useeior")
#install
devtools::install_github("modelearth/useeior")


library(useeior)
useeior::seeAvailableModels()


# This example with Biofuels
# newSectorName: "LignoCelullosic Biofuels"
# newSectorCode: "324110B"
# similarSectorCode: "324110"- BEA code- Petroleum refineries
# percentage: 1%
# Input purchases from commodities: 100 for the first 300 commodities and 0 for the rest
# New environmental data: 0 for all the environmental flows.


#Info for new sector
newSectorCode<-"324110B"
newSectorName<-"LignoCelullosic Biofuels"
simSectorCode<-"324110"
percentage<- 0.01
# Obtain Input purchases from similar sector
#inputPurchases<- c(model$Use[1:405, simSectorCode],0)
inputPurchases<- c(rep(0,300),rep(0,106))
#colSim<-which(colnames(model2$Use)==simSectorCode)
#envVector<-model$B[,colSim] 
envVector<-rep(0,15) 


#Create model
model2<-createBioeconomyModel('USEEIOv2.0-GHG',newSectorCode,newSectorName, simSectorCode,percentage, inputPurchases, envVector)

#Calculate model
result2 <- useeior::calculateEEIOModel(model2, perspective='DIRECT')

#See some results
result2$LCIA_d["324110B",1]
result2$LCIA_d["324110",1]

#Print results
# Everything is updated to use this function. The only "exception" is that we are not modifying the satellite tables directly, so we cannot update them but
# in consequence, the file that ends in "_sat.csv" doesn't make sense.
useeior::writeModelComponents(model2)
