#This is an example of using the new Bioeconomy function
#Start with clean
remove.packages("useeior")
#install
devtools::install_github("modelearth/useeior") #This route is probably the wrong one

library(useeior)
useeior::seeAvailableModels()

rm(list=ls())
#-------------------------------------------------------------------------------------
# Parameters
# All prices in $USD/GGE

modelName<- 'USEEIOv2.0-GHG'
newIndustryInfo<- data.frame(Code=c("324110B1","324110B2","324110B3"),Name=c("GasFermentation", "GuerbetReaction", "FischerTropsch"), Primary_product=rep("Biofuel", times=3), Price=c(1.6,1.6,1.6), Percentage_Prod=c(0.33,0.33,1-0.66))
#newIndustryInfo<- data.frame(Code=c("324110B1","324110B2","324110B3"),Name=c("GasFermentation", "Thermochemical", "Technology3"), Primary_product=rep("Biofuel", times=3), Price=c(1.6,1.6,1.6), Percentage_Prod=c(0.999,0.0005,0.0005))

newCommodityInfo<- data.frame(Code="324110B", Name="Biofuel", Primary_producers=paste("324110B1","324110B2","324110B3", sep=","))
# Note this name correspond to BEA names, but not the sector names in useeior
simCommodityInfo<-list(Code="324110", Name="Petroleum refineries", PrimaryProducer_Code="324110", PrimaryProducer_Name="Petroleum refineries", Price=1.6)
percentage<-0.2*0.184

#This is input purchases in $/GGE
#Read input purchases .csv

#inputP_GF<-read.csv("inst/extdata/GF_InputPurchases_V01.csv")
#inputPurchasesNewTech<-matrix(c(inputP_GF[,3],rep(0,100),rep(0,306),rep(0,300),rep(0,106)), nrow=406, ncol=3)
inputP_data<-read.csv("inst/extdata/GF_InputPurchases_V02.csv")
inputPurchasesNewTech<-as.matrix((inputP_data[,(3:5)]))

#inputPurchasesNewTech<-matrix(c(inputP_GF[,3],rep(1,100),rep(0,306),rep(1,300),rep(0,106)), nrow=406, ncol=3) this was was to experiment with
#This is value added in $/GGE (Includes compensation to employes, taxes and gross operating surplus)
valueAdded<-matrix(c(0.08,0.8,2.29,rep(0,3),rep(0,3)), nrow=3, ncol=3) #the gross operating surplus needs to be updated

#Read environmental data
#GF_envData<-read.csv("inst/extdata/GF_EnvFlow_GHG_V01.csv")
envData_read<-read.csv("inst/extdata/GF_EnvFlow_GHG_V02.csv")
envData<- as.matrix(envData_read[,-1]) 
#envData<- matrix(c(GF_envData[,2],rep(0,15),rep(0,15)), nrow=15, ncol=3) 
  
#-------------------------------------------------------------------------------------
#Create model
bioModel<-addBiofuelsSector(modelName, newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage, inputPurchasesNewTech, valueAdded, envData )

#Calculate model
result2 <- useeior::calculateEEIOModel(bioModel, perspective='DIRECT')

bio#Print results
# Everything is updated to use this function. The only "exception" is that we are not modifying the satellite tables directly, so we cannot update them but
# in consequence, the file that ends in "_sat.csv" doesn't make sense.
useeior::writeModelComponents(bioModel)



#FOR VALIDATION

#See some results
result2$LCIA_d["324110B",]
result2$LCIA_d["324110",]

write.table(result2$LCIA_d, "Results3(Jun-18-2021).txt", sep="\t")
colSums(result2$LCIA_d)

#Get petroleum refineries input purchases in $/GGE
totalInOutput_pet_MillionUSD<-bioModel$Use["T008","324110"]
totalInOutput_pet_GGE<-transformMillionUSDtoGGE(totalInOutput_pet_MillionUSD,1.6)
recipeInputPurchases_pet<-(bioModel$Use[1:406,"324110",drop=FALSE]/totalInOutput_pet_GGE)*1000000

#Create dataframe to compare recipes
recipeComparison<- cbind(recipeInputPurchases_pet,inputPurchasesNewTech)

#Total economy million dollars produced in bioEconomy
sum(bioModel$Make[1:408,"T008"])
#Total economy million dollars produced in current economy (have to run the traditional model first)
sum(model$Make[1:405,"T008"])

# Total Commodity Output differences between Make and Use
bioModel$Make[409,1:406]-bioModel$Use[1:406,431]

# Total Industry Output differences between Make and Use
bioModel$Make[1:408,407]-bioModel$Use[412,1:408]


