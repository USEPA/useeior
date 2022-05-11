# Example for running useeior, see https://github.com/USEPA/useeior for more information on the model
# Must install useeior first to run example. See Wiki @ https://github.com/USEPA/useeior/wiki for install instructions
# Run ?functionname to see documentation and function input options for the functions in this example

library(useeior)

# See all available model names
# explanation of model names can be found at https://github.com/USEPA/USEEIO/blob/master/VersioningScheme.md
seeAvailableModels()

# Build USEEIO v2.0-GHG
#model <- buildModel("USEEIOv2.0s-GHG")
#model <- buildModel("USEEIOv2.0-GHG_nodisagg")

model <- buildModel("USEEIOv2.0_nodisagg")
modelNewTech <- buildModel("USEEIOv2.0_nodisagg_newTech")

# Calculate direct perspective life cycle inventory (LCI) result and life cycle impact assessment (LCIA) 
# result using total US production
result <- calculateEEIOModel(model, perspective = "DIRECT", demand = "Production")
resultNewTech <- calculateEEIOModel(modelNewTech, perspective = "DIRECT", demand = "Production")

# Adjust N matrix (direct + indirect impacts per dollar) to 2018 dollar and purchaser price
N_adj <- adjustResultMatrixPrice("N", currency_year = 2012, purchaser_price = FALSE, model)

#-------------------------------------------------------------------------------------------------------------------------------

# Export C matrix, relationship between LCI and impacts
write.table(modelNewTech$C,file="C.csv", sep=",",row.names = TRUE, col.names = TRUE)

#---------------------------------------------------
resultNewTech$LCIA_d[c("324110/US","324110B/US"),]

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

#Printing results to Latex table
library("xtable")
xtable(t(resultNewTech$LCIA_d[c("324110/US","324110B/US"),]), type = "latex", file = "LCIA-newTechResults.tex") # First test

#Formatting a table of environmental impacts
library("Hmisc")
LCIA_results_comparative<- t(resultNewTech$LCIA_d[c("324110/US","324110B/US"),])
LCIA_results_comparative_Formatted<- format(LCIA_results_comparative, big.mark=",",  scientific = FALSE, justify= "right", trim=FALSE)
xtable(LCIA_results_comparative_Formatted, type= "latex", align=c("l","r","r"))

#-------------------------------------------------------------------------------------------------------------------------------
#### BROADER IMPACTS ####
#Comparative table current and +NewTech

conversionN<-1/10^9
sigFig<-3
monetaryUnits<-"Billion USD"

newUnitsTable1_environmental<-data.frame(Item=colnames(resultNewTech$LCIA_d),
                                         Units= c("billion kg SO2 eq", "billion kg", "billion kg", "billion kg","EJ","billion kg N eq","billion CTUe","trillion kg","trillion kg CO2 eq","million kg","CTUh", "CTUh", 
                                                  "million kg PM2.5 eq","CTUh","million jobs", "trillion m2*yr","billion kg","EJ","thousand kg CFC-11 eq","ten thousand kg","EJ","10^8 kg O3 eq","trillion USD"),
                                         conversionFactor=c(1/10^9,1/10^9,1/10^9,1/10^9,1/10^12,1/10^9,1/10^9,1/10^12,1/10^12,1/10^6,1,1,1/10^6,1,1/10^(6),1/10^12,1/10^9,1/10^12,1/10^3,1/10^4,1/10^12,1/10^8,1/10^12))



# Format numbers
cur_totalPur_Petroleum<-format(result$TotalPurchases["324110/US",]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
cur_totComOut_Petroleum<- format(modelNewTech$BiofuelsData$OriginalBEA$Make["T007", "324110"]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
cur_totProd_Whole<-format(sum(modelNewTech$BiofuelsData$OriginalBEA$Make[,"T008"])*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)

# Total production of economy
#sum(modelNewTech$BiofuelsData$OriginalBEA$Make[,"T008"])
# Total consumption of economy 
#sum(modelNewTech$BiofuelsData$OriginalBEA$Make["T007",])

cur_EnvImpacts<- format(as.list(colSums(result$LCIA_d)*newUnitsTable1_environmental$conversionFactor), digits=sigFig, nsmall=2,scientific=FALSE)
#----
new_totalPur_Petroleum<- format(resultNewTech$TotalPurchases["324110/US",]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
new_totComOut_Petroleum<- format(modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110"]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
new_totalPur_Bio<-format(resultNewTech$TotalPurchases["324110B/US",]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
new_totComOut_Bio<- format(modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110B"]*conversionN, digits=sigFig,nsmall=2,scientific=FALSE)
new_totProd_Whole<-format(sum(modelNewTech$BiofuelsData$NewBEA$Make[,"T008"])*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
new_EnvImpacts<-format(as.list(colSums(resultNewTech$LCIA_d)*newUnitsTable1_environmental$conversionFactor), digits=sigFig, nsmall=2,scientific=FALSE)

#----
diff_totalPur_Petroleum<- format((resultNewTech$TotalPurchases["324110/US",]-result$TotalPurchases["324110/US",])*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
diff_totComOut_Petroleum<-format((modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110"]-modelNewTech$BiofuelsData$OriginalBEA$Make["T007", "324110"])*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
diff_totalPur_Bio<-format(resultNewTech$TotalPurchases["324110B/US",]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
diff_totalComOut_Bio<-format(modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110B"]*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
diff_totProd_Whole<-format((sum(modelNewTech$BiofuelsData$NewBEA$Make[,"T008"])-sum(modelNewTech$BiofuelsData$OriginalBEA$Make[,"T008"]))*conversionN, digits=sigFig, nsmall=2,scientific=FALSE)
diff_EnvImpacts<- format(as.list((colSums(resultNewTech$LCIA_d)-colSums(result$LCIA_d))*newUnitsTable1_environmental$conversionFactor), digits=sigFig, nsmall=2,scientific=FALSE)

#----
# Percentage change/ percentage of substitution
percentual_change_totalPur_Petroleum<-((resultNewTech$TotalPurchases["324110/US",]-result$TotalPurchases["324110/US",])/result$TotalPurchases["324110/US",])*100
percentual_change_totComOut_Petroleum<-((modelNewTech$BiofuelsData$NewBEA$Make["T007", "324110"]-modelNewTech$BiofuelsData$OriginalBEA$Make["T007", "324110"])/
  modelNewTech$BiofuelsData$OriginalBEA$Make["T007", "324110"])*100
percentual_change_totProd_Whole<-((sum(modelNewTech$BiofuelsData$NewBEA$Make[,"T008"])-sum(modelNewTech$BiofuelsData$OriginalBEA$Make[,"T008"]))/
  sum(modelNewTech$BiofuelsData$OriginalBEA$Make[,"T008"]))*100
percentual_change_EnvImpacts<-((colSums(resultNewTech$LCIA_d)-colSums(result$LCIA_d))/colSums(result$LCIA_d))*100

wrt_per_bio_totalPur_Petroleum<-percentual_change_totalPur_Petroleum/(modelNewTech$BiofuelsData$BiofuelsPercentage*100)
wrt_per_bio_totComOut_Petroleum<-percentual_change_totComOut_Petroleum/(modelNewTech$BiofuelsData$BiofuelsPercentage*100)
wrt_per_bio_totProd_Whole<-percentual_change_totProd_Whole/(modelNewTech$BiofuelsData$BiofuelsPercentage*100)
wrt_per_bio_EnvImpacts<-percentual_change_EnvImpacts/(modelNewTech$BiofuelsData$BiofuelsPercentage*100)

#Format
format_change_totalPur_Petroleum<- format(wrt_per_bio_totalPur_Petroleum, digits=4)
format_change_totComOut_Petroleum<- format(wrt_per_bio_totComOut_Petroleum, digits=4)
format_change_totProd_Whole<- format(wrt_per_bio_totProd_Whole, digits=4)
format_change_EnvImpacts<- format(wrt_per_bio_EnvImpacts, digits = 4)
format_change_EnvImpacts<- round(wrt_per_bio_EnvImpacts, digits = 3) #test to see if this works better
#----
#Comparative table with differences

table_rowNames<- c("Economic", "Total Purchases- Petroleum Refineries", "Total commodity output- Petroleum Refineries", "Total Purchases- Biorefineries","Total commodity output-  Biorefineries", "Total production-Whole Economy"
                   , "Environmental", colnames(resultNewTech$LCIA_d) )

# Re-order indicators by alphabetic order
indicators_info<-model$Indicators$meta[order(model$Indicators$meta$Name),] #This is the original source of units organized alphabetically. Not currently in use
units_vector<-c("---", monetaryUnits, monetaryUnits, monetaryUnits,monetaryUnits, monetaryUnits, "---", newUnitsTable1_environmental$Units)


current_vector<- c("---", cur_totalPur_Petroleum , cur_totComOut_Petroleum,"NA", "NA",cur_totProd_Whole,
                   "---", cur_EnvImpacts)
newTech_vector<- c("---", new_totalPur_Petroleum , new_totComOut_Petroleum, new_totalPur_Bio, new_totComOut_Bio,new_totProd_Whole,
                   "---", new_EnvImpacts)
difference_vector<-c("---", diff_totalPur_Petroleum,diff_totComOut_Petroleum,diff_totalPur_Bio, diff_totalComOut_Bio,diff_totProd_Whole,
                     "---",diff_EnvImpacts)

table<- data.frame("Units"=units_vector,"Current"=current_vector, "With New Tech"=newTech_vector, "Difference"=difference_vector,row.names = table_rowNames)

xtable(table, type= "latex", align=c("l","c","r","r","r"), 
       caption = paste("Whole economy comparative results with ", modelNewTech$BiofuelsData$BiofuelsPercentage*100,"% of substitution"),label="Table:Comparative_results")

#..............................................................................................................................................................................
# Table 1 of document= table but only for economic impacts, eliminating column units and putting it into the caption
table_Economic<-table[2:6,2:4]

xtable(table_Economic, type= "latex", align=c("l","r","r","r"), 
       caption = paste("Whole economy economic comparative results with ", modelNewTech$BiofuelsData$BiofuelsPercentage*100,"% of substitution. In billion USD."),label="Table:Comparative_results_only_economic")
#------------------------------------
# Table without units 

table_rowNames<- c("Economic", "Total Purchases- Petroleum Refineries", "Total commodity output- Petroleum Refineries", "Total production-Whole Economy"
                   , "Environmental", colnames(resultNewTech$LCIA_d) )

difference_rate<-c("---", format_change_totalPur_Petroleum,format_change_totComOut_Petroleum,format_change_totProd_Whole,
                     "---",format_change_EnvImpacts)


table<- data.frame( "% change/ % substitution"=difference_rate,row.names = table_rowNames)

xtable(table, type= "latex", align=c("l","c"), 
       caption = paste("Whole economy changes with respect to ", modelNewTech$BiofuelsData$BiofuelsPercentage*100,"% of substitution"),label="Table:Changes_rate")

#Don't like how the format of the numbers are looking


#To erase the impacts that are excluded
rowsToLeave<-c("Eutrophication Potential", "Greenhouse Gases", "Acidification Potential", "Human Health - Respiratory Effects",
               "Energy Use", "Nonrenewable Energy Use", "Renewable Energy Use", "Land use", "Jobs Supported")
wrt_per_bio_EnvImpacts<-wrt_per_bio_EnvImpacts[rowsToLeave]
format_change_EnvImpacts<- round(wrt_per_bio_EnvImpacts, digits = 3)


impactNames<-names(wrt_per_bio_EnvImpacts)

#Figure
#Open png file
png("per change impact per per substitution.png", width=892, height=611)
#Create plot
par(mar=c(3,20,3,2))
figure<-barplot(wrt_per_bio_EnvImpacts, names.arg = impactNames, main = "% change impact / % substitution", horiz = TRUE, las=1, xlim=c(-1,6.5),xaxp=c(-1,6.5,6))
abline(v=0) #add vertical line in 0
text(wrt_per_bio_EnvImpacts+0.35*sign(wrt_per_bio_EnvImpacts),figure, labels = format_change_EnvImpacts, cex=0.8) #Adding the values as labels for each bar
#Close file
dev.off()


#Graph for poster
png("per change impact per per substitution_Poster.png", width=950, height=611)
par(mar=c(3,30,3,2))
figure<-barplot(wrt_per_bio_EnvImpacts, names.arg = colnames(resultNewTech$LCIA_d), main = "% change impact / % substitution", horiz = TRUE, las=1, xlim=c(-1,6.5),xaxp=c(-1,6.5,6),
                cex.axis=2, cex.main=2, cex=2, col ="#1B70C0" )
abline(v=0) #add vertical line in 0
#text(wrt_per_bio_EnvImpacts+1*sign(wrt_per_bio_EnvImpacts),figure, labels = format_change_EnvImpacts, cex=2) #Adding the values as labels for each bar
dev.off()
#............................................................................................................................
# Commodities associated GHG changes
orig_GHG_existingCommodities<-result$LCIA_d[,"Greenhouse Gases"]
new_GHG_existingCommodities<-resultNewTech$LCIA_d[(rownames(resultNewTech$LCIA_d)!="324110B/US"),"Greenhouse Gases"]
comparison<-data.frame(cbind(orig_GHG_existingCommodities,new_GHG_existingCommodities))
comparison$Code_Loc<-rownames(comparison) #Add row names as a column
comparison<-merge(model$Commodities[,c("Code_Loc", "Name")],comparison, by="Code_Loc") #Analogous to VLOOKUp here
#comparison<-comparison[,c(3,1,2)] #Reorder columns
comparison$Difference<-comparison$orig_GHG_existingCommodities-comparison$new_GHG_existingCommodities

assignChange<-function(row){
  return<-" "
  dif<-as.double(row["Difference"])
  if(dif>0){return<-"Decrease"}
  else if(dif==0){return<-"Unchanged"}
  else{return<-"Increase"}
  return
}
comparison$TypeOfChange<-apply(comparison,1, assignChange)


increasedSectors<-comparison[(comparison$TypeOfChange=="Increase"),c("Code_Loc","Name","Difference")]
decreasedSectors<-comparison[(comparison$TypeOfChange=="Decrease"),c("Code_Loc","Name","Difference")]
unchangedSectors<-comparison[(comparison$TypeOfChange=="Unchanged"),c("Code_Loc","Name","Difference")]

#...Graph for increasing sectors...
#Order from biggest to smallest
increasedSectors$Abs_difference<-abs(increasedSectors$Difference)
increasedSectors<-increasedSectors[order(-increasedSectors$Abs_difference),]

#Graph the biggest ones
n<-6
#Increase margin size
par(mar=c(5,22,4,2))
barplot(increasedSectors[1:6,"Abs_difference"]/10^6, names.arg = increasedSectors[1:6,"Name"], main = "Top 6 commodities that increased GHG", xlab="Increased emissions (Million kg of CO2e) ", horiz = TRUE, las=1)

#...Graph for decreasing sectors...
#Order from smallest to biggest, then I take the last 6 and strangely the graph is order as I wanted
decreasedSectors<-decreasedSectors[order(decreasedSectors$Difference),]

#Graph the biggest ones
n<-6
#Increase margin size
par(mar=c(5,22,4,2))
barplot(decreasedSectors[222:227,"Difference"]/10^6, names.arg = decreasedSectors[222:227,"Name"], main = "Top 6 commodities that decreased GHG", xlab="Decreased emissions (Million kg of CO2e) ", horiz = TRUE, las=1)

#...Graph all together in the same graph...
#Open png file
png("Top6_up_down.png", width=700, height=350)
#Create plot
par(mar=c(5,22,4,2))
barplot(c(increasedSectors[1:6,"Abs_difference"]/10^6,-decreasedSectors[222:227,"Difference"]/10^6), names.arg = c(increasedSectors[1:6,"Name"],decreasedSectors[222:227,"Name"]), main = "Top 6 commodities with increased and decreased GHG", xlab="Change in emissions (Million kg of CO2e) ", horiz = TRUE, las=1, 
        xlim=c(-15500,700), xaxp=c(-15300,700,10))
abline(v=0) #add vertical line in 0

#Close file
dev.off()

#Same graph but with bigger letter for poster
#Open png file
decreasedSectorsNames<-decreasedSectors[222:227,"Name"]
decreasedSectorsNames[5]<- "Gasoline,fuels,and by-products"

png("Top6_up_down_poster.png", width=950, height=600)
#Create plot
par(mar=c(5,27,4,2))
#Note I used \n in the title to divide in two lines
barplot(c(increasedSectors[1:6,"Abs_difference"]/10^6,-decreasedSectors[222:227,"Difference"]/10^6), names.arg = c(increasedSectors[1:6,"Name"],decreasedSectorsNames), main = "Top 6 commodities with \n increased and decreased GHG", xlab="Change in emissions (Million kg of CO2e) ", horiz = TRUE, las=1, 
        xlim=c(-15500,700), xaxp=c(-15300,700,10), cex.main=2, cex.names = 2, cex.axis = 2, cex.lab=2, col ="#1B70C0")
abline(v=0) #add vertical line in 0

#Close file
dev.off()
#-------------------------------------------------------------------------------------------------------------------------------

#### BIO-PRODUCT IMPACTS ####

#The following results are with the same model in which each of the new industries produces 1/3 
#Change y vector
f<-rep(0,times=406)
names(f)<-modelNewTech$Commodities$Code_Loc 
#---Assign demand of 1 GGE in USD
biofuelWeightedPrice<-sum(modelNewTech$BiofuelsData$NewIndustriesInfo$Price*modelNewTech$BiofuelsData$NewIndustriesInfo$Percentage_Prod)
f[406]<-biofuelWeightedPrice #Since I know biofuels are in the last row 

#...........................................................................................................
# Calculate bio-product impacts

#bio_results<-calculateEEIOModifiedDemand(modelNewTech,perspective = "DIRECT", demand = "Production", f )
bio_results<-calculateEEIOModel(modelNewTech,perspective = "DIRECT", demand =f )
#...........................................................................................................
bio_EnvImpacts<- format(colSums(bio_results$LCIA_d), digits=4, scientific=TRUE)

table1<-data.frame("Units"=indicators_info$Unit, "Impacts per GGE"=bio_EnvImpacts,row.names = colnames(bio_results$LCIA_d))
xtable(table1, type="latex", align=c("l","c","r"), 
       caption = paste("Biofuels impacts with ", modelNewTech$BiofuelsData$BiofuelsPercentage*100,"% of substitution"),label="Table:biofuels_results")  
#Recall to add the \ before the % in overleaf

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

# The way in which I obtained the numbers in my original table
bio_EnvImpacts_GF<-format(colSums(bio_results_GF$LCIA_d), digits=4, scientific=TRUE)
bio_EnvImpacts_GR<-format(colSums(bio_results_GR$LCIA_d), digits=4, scientific=TRUE)
bio_EnvImpacts_FT<-format(colSums(bio_results_FT$LCIA_d), digits=4, scientific=TRUE)

table2<-data.frame("Units"=indicators_info$Unit, "All technologies"=bio_EnvImpacts, "Gas Fermentation"=bio_EnvImpacts_GF, "Guerbet Reaction"=bio_EnvImpacts_GR,
                   "Fischer Tropsch"=bio_EnvImpacts_FT, row.names = colnames(bio_results$LCIA_d))


#.....Improve format- here units.......
allImpactNames<-rownames(table2)
# The conversion factor is the number that I have to multiple to get the new units
newUnitsTable2<-data.frame(Impact=allImpactNames,
                           Units= c("g SO2 eq", "g", "g", "g","MJ","g N eq","CTUe","kg","kg CO2 eq","g","10E-9 CTUh", "10e-9 CTUh", 
                                    "g PM2.5 eq","10E-9 CTUh","10E-6 jobs", "m2*yr","kg","MJ","mg CFC-11 eq","mg","MJ","g O3 eq","$"),
                           conversionFactor=c(10^3,10^3,10^3,10^3,1,10^3,1,1,1,10^3,1/10^(-9),1/10^(-9),10^3,1/10^(-9),1/10^(-6),1,1,1,10^6,10^6,1,10^3,1))

# Update the units of table 2
table2$Units<-newUnitsTable2$Units

#I believe the justify is not working
bio_EnvImpacts<- format(as.list(colSums(bio_results$LCIA_d)*newUnitsTable2$conversionFactor), digits=3,nsmall=2, scientific=FALSE, justify = "right")

bio_EnvImpacts_GF<-format(as.list(colSums(bio_results_GF$LCIA_d)*newUnitsTable2$conversionFactor), digits=3,nsmall=2, scientific=FALSE, justify = "right")
bio_EnvImpacts_GR<-format(as.list(colSums(bio_results_GR$LCIA_d)*newUnitsTable2$conversionFactor), digits=3, nsmall=2,scientific=FALSE, justify = "right")
bio_EnvImpacts_FT<-format(as.list(colSums(bio_results_FT$LCIA_d)*newUnitsTable2$conversionFactor), digits=3, nsmall=2,scientific=FALSE, justify = "right")

#Test of formatting via lapply

# bio_EnvImpacts_GF<-lapply(colSums(bio_results_GF$LCIA_d)*newUnitsTable2$conversionFactor,format,digits=3, nsmall=2)
# bio_EnvImpacts_GR<-format(colSums(bio_results_GR$LCIA_d)*newUnitsTable2$conversionFactor, digits=3, scientific=FALSE)
# bio_EnvImpacts_FT<-format(colSums(bio_results_FT$LCIA_d)*newUnitsTable2$conversionFactor, digits=3, scientific=FALSE)

#This one is the Table 2 including all- this is the one in Annex
table2<-data.frame("Units"=newUnitsTable2$Units, "All technologies"=bio_EnvImpacts, "Gas Fermentation"=bio_EnvImpacts_GF, "Guerbet Reaction"=bio_EnvImpacts_GR,
                   "Fischer Tropsch"=bio_EnvImpacts_FT, row.names = colnames(bio_results$LCIA_d))
xtable(table2, type="latex", align=c("l","c","r", "r", "r", "r"), 
       caption = paste("Biofuels impacts per GGE with ", modelNewTech$BiofuelsData$BiofuelsPercentage*100,"% of substitution"),label="Table:ALLbiofuels_results")  
#Recall to add the \ before the % in overleaf


#Here I eliminate from Table 2 the impacts that I want to exclude and I order the rows as Dr. Thomas suggested

rowsToLeave<-c("Greenhouse Gases","Eutrophication Potential","Acidification Potential",
               "Human Health - Respiratory Effects","Energy Use", "Nonrenewable Energy Use", 
               "Renewable Energy Use","Land use","Jobs Supported","Freshwater withdrawals",
               "Value Added" )
table2<-table2[rowsToLeave,]
xtable(table2, type="latex", align=c("l","c","r", "r", "r", "r"), 
       caption = paste("Biofuels impacts per GGE with ", modelNewTech$BiofuelsData$BiofuelsPercentage*100,"% of substitution"),label="Table:biofuels_results")  
#Recall to add the \ before the % in overleaf


# #Reorder rows as Dr.Thomas suggested (Another way to do it)
# rowsOrder<-c(3,5,2,10,1,4,9,8,6,7)
# table2<-table2[order(rowsOrder),] #To reorder rows according to the order I established
#-------------------------------------------------------------------------------------------------------------------------------
#Attempt to create pretty tables in R

library(tidyverse)
library(gt)
library(webshot)

tableTest<-table2
tableTest<-cbind(rownames(tableTest),tableTest)
colnames(tableTest)<-c("Impact", "Units", "All Technologies", "Gas \n Fermentation", "Guerbet \n Reaction", "Fischer \n Tropsch")
rownames(tableTest)<-NULL


objectTableTest<-tableTest %>% gt()%>%
  tab_header(title = md("**Biofuels impacts per GGE with 3.69% of subtitution**")) %>%
  cols_align(align="right", columns=c(3:6))%>%
  cols_align(align="center", columns=2)%>%
  tab_options(
    #column_labels.font.size=24, #Change size of the column labels
    container.width=px(1500), #922 pixeles, equivalent to apro 9.6 inches
    table.width = px(1500),
    column_labels.font.weight = "bold",
    table.font.size = 36
  )
gtsave(objectTableTest,"Table2.png")

#...........................................................................................................
#Same for Table 1

tableTest<-table
tableTest<-cbind(rownames(tableTest),tableTest)
colnames(tableTest)<-c("Economic Criterion \n (Million USD)", "Units", "Current", "With New technology", "Difference")
tableTest<-tableTest[2:6,c(1,3:5)] #Leave only economic rows and erase units column


objectTableTest<-tableTest %>% gt()%>%
  tab_header(title = md("**Whole economy economic results with 3.68% of substitution**")) %>%
  cols_align(align="right", columns=c(2:4))%>%
  tab_options(
    #column_labels.font.size=24, #Change size of the column labels
    #container.width=px(1500), #922 pixeles, equivalent to apro 9.6 inches
    #table.width = px(1500),
    column_labels.font.weight = "bold",
    table.font.size = 36
  )
gtsave(objectTableTest,"Table1.png")


#-------------------------------------------------------------------------------------------------------------------------------
# Checking for impacts of only the production part, ommiting the supply chain, to answer question from EPA reviewer

#Basically, I'll take the external information matrix and multiply it by C

#Read environmental data
envData_read<-read.csv(modelNewTech$newTechSpecs$NewTechnologies$EnvironmentalFlows)
envData<- as.matrix(envData_read[,-1]) 

impactsPlantNewTechs<-model$C%*%envData
  

