# Example for running useeior, see https://github.com/USEPA/useeior for more information on the model
# Must install useeior first to run example. See Wiki @ https://github.com/USEPA/useeior/wiki for install instructions
# Run ?functionname to see documentation and function input options for the functions in this example

library(useeior)

# See all available model names
# explanation of model names can be found at https://github.com/USEPA/USEEIO/blob/master/VersioningScheme.md
seeAvailableModels()

# Build USEEIO v2.0-GHG
model <- buildModel("USEEIOv2.0_nodisagg")
modelNewTech <- buildModel("USEEIOv2.0_nodisagg_newTech")

# Calculate direct perspective life cycle inventory (LCI) result and life cycle impact assessment (LCIA) 
# result using total US production
result <- calculateEEIOModel(model, perspective = "DIRECT", demand = "Production")
resultNewTech <- calculateEEIOModel(modelNewTech, perspective = "DIRECT", demand = "Production")

#-------------------------------------------------------------------------------------------------------------------------------

#Original tables
modelNewTech$BiofuelsData$OriginalBEA$Make["324110", "324110"]
modelNewTech$BiofuelsData$OriginalBEA$Make["324110", "T008"]
modelNewTech$BiofuelsData$OriginalBEA$Make["T007", "324110"]

result$TotalPurchases["324110/US",] # This one only works if you run the model without new tech

# New tech tables

modelNewTech$BiofuelsData$NewBEA$Make["324110", "324110"]
modelNewTech$BiofuelsData$NewBEA$Make["324110", "T008"]
modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110"]

modelNewTech$BiofuelsData$NewBEA$Make["324110B1", "324110B"]
modelNewTech$BiofuelsData$NewBEA$Make["324110B2", "324110B"]
modelNewTech$BiofuelsData$NewBEA$Make["324110B3", "324110B"]
modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110B"]

modelNewTech$BiofuelsData$NewBEA$Use["324110", "T007"]
modelNewTech$BiofuelsData$NewBEA$Use["324110B", "T007"]

resultNewTech$TotalPurchases["324110/US",]
resultNewTech$TotalPurchases["324110B/US",]

#LCI

View(result$LCI_d[c("324110/US"),])
View(resultNewTech$LCI_d[c("324110/US","324110B/US"),])

#LCIA
View(result$LCIA_d[c("324110/US"),])
View(colSums(result$LCIA_d)) #All economy

View(resultNewTech$LCIA_d[c("324110/US","324110B/US"),])
View(colSums(resultNewTech$LCIA_d)) #All economy

#-------------------------------------------------------------------------------------------------------------------------------
#### BROADER IMPACTS ####
#For broader impacts, use resultNewTech$LCIA_d

#-------------------------------------------------------------------------------------------------------------------------------

#### BIO-PRODUCT IMPACTS ####

#The following results are with the same model in which each of the new industries produces 1/3 
#...........................................................................................................
#Change y vector
f<-rep(0,times=406)
names(f)<-modelNewTech$Commodities$Code_Loc 
#---Assign demand of 1 GGE in USD
biofuelWeightedPrice<-sum(modelNewTech$BiofuelsData$NewIndustriesInfo$Price*modelNewTech$BiofuelsData$NewIndustriesInfo$Percentage_Prod)
f[406]<-biofuelWeightedPrice #Since I know biofuels are in the last row 


# Calculate bio-product impacts
bio_results<-calculateEEIOModel(modelNewTech,perspective = "DIRECT", demand =f )

#...........................................................................................................
# Construct additional three scenarios in which only one industry is used.
modelGF<-buildModel("USEEIOv2.0_nodisagg_newTech_allGF")
modelGR<-buildModel("USEEIOv2.0_nodisagg_newTech_allGR")
modelFT<-buildModel("USEEIOv2.0_nodisagg_newTech_allFT")

f_GF<-f
f_GR<-f  
f_FT<-f

# Use the price under each tech
f_GF[406]<-modelNewTech$BiofuelsData$NewIndustriesInfo[(modelNewTech$BiofuelsData$NewIndustriesInfo$Name=="GasFermentation"),"Price"]
f_GR[406]<-modelNewTech$BiofuelsData$NewIndustriesInfo[(modelNewTech$BiofuelsData$NewIndustriesInfo$Name=="GuerbetReaction"),"Price"] 
f_FT[406]<-modelNewTech$BiofuelsData$NewIndustriesInfo[(modelNewTech$BiofuelsData$NewIndustriesInfo$Name=="FischerTropsch"),"Price"]
  
bio_results_GF<-calculateEEIOModel(modelGF,perspective = "DIRECT", demand =f_GF )
bio_results_GR<-calculateEEIOModel(modelGR,perspective = "DIRECT", demand =f_GR )
bio_results_FT<-calculateEEIOModel(modelFT,perspective = "DIRECT", demand =f_FT )


  

