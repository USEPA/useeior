#This is an example of using the new Bioeconomy function
#Start with clean
remove.packages("useeior")
#install
devtools::install_github("modelearth/useeior") #This route is probably the wrong one

library(useeior)
useeior::seeAvailableModels()


#-------------------------------------------------------------------------------------
# Parameters
# All prices in $USD/GGE

modelName<- 'USEEIOv2.0-GHG'
newIndustryInfo<- data.frame(Code=c("324110B1","324110B2","324110B3"),Name=c("GasFermentation", "Thermochemical", "Biochemical"), Primary_product=rep("Biofuel", times=3), Price=c(5.04,5.04,5.04), Percentage_Prod=c(1/3,1/3,1/3))
newCommodityInfo<- data.frame(Code="324110B", Name="Biofuel", Primary_producers=paste("324110B1","324110B2","324110B3", sep=","))
# Note this name correspond to BEA names, but not the sector names in useeior
simCommodityInfo<-list(Code="324110", Name="Petroleum refineries", PrimaryProducer_Code="324110", PrimaryProducer_Name="Petroleum refineries", Price=3.5)
percentage<-0.9

#This is input purchases in $/GGE
#inputPurchasesNewTech<- matrix(c(rep(0,300),rep(0,106),rep(0,100),rep(0,306),rep(0,300),rep(0,106)), nrow=406, ncol=3)
inputPurchasesNewTech<- matrix(c(rep(0.1,300),rep(0,106),rep(0.1,100),rep(0,306),rep(0,300),rep(0.102,106)), nrow=406, ncol=3)
#This is value added in $/GGE
valueAdded<-matrix(c(rep(1,2),rep(2,2),rep(1.5,2)), nrow=2, ncol=3) 

envData<-matrix(c(rep(0,15),rep(0,15),rep(0,15)), nrow=15, ncol=3) 
  
#-------------------------------------------------------------------------------------
#Create model
bioModel<-addBiofuelsSector(modelName, newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage, inputPurchasesNewTech, valueAdded, envData )

#Calculate model
result2 <- useeior::calculateEEIOModel(bioModel, perspective='DIRECT')

#Print results
# Everything is updated to use this function. The only "exception" is that we are not modifying the satellite tables directly, so we cannot update them but
# in consequence, the file that ends in "_sat.csv" doesn't make sense.
useeior::writeModelComponents(model2)



#FOR VALIDATION

#See some results
result2$LCIA_d["324110B",1]
result2$LCIA_d["324110",1]

# Total Commodity Output differences between Make and Use
bioModel$Make[409,1:406]-bioModel$Use[1:406,431]

# Total Industry Output differences between Make and Use
bioModel$Make[1:408,407]-bioModel$Use[412,1:408]


