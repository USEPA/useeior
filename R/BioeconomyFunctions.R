# This script contains the functions that modificate the Make and Use tables, the A and B matrices and the Y vector
# to recalculate the USEEIO model with an additional biofuels sector (3 new industries that produce 1 new commodity-biofuels)
# More infromation about the methodology can be found at "LCA New tech Methodology (v01 CGA 2020-07-23).pdf".

#' Modify Make BEA Table.
#' 
#' This function modifies the pre-saved Make table for adding the new biofuels sector.
#' 
#' @param modModel the model under modification after initial adjustments for Use Table.
#' @param UseTransactions dataframe with the input purchases/use transactions for each industry of each commodity after modifying Use Table.
#' @param TotalIndustryOutputNewInd  a dataframe with the Total Industry Output for the 3 new industries based on the modified Use Table.
#' @return a dataframe with the original Make table modified.
#' 
#' Example: modifyMakeTable(modModel,modUse[1:406,1:408],modUse[412,406:408])
modifyMakeTable <- function(modModel, UseTransactions, TotalIndustryOutputNewInd){
  
  originalMake<-modModel$Make
  modMake<-originalMake
  
  #Determine number of commodities and industries in originalMake
  nCommodities<- ncol(originalMake)-1
  nIndustries<- nrow(originalMake)-1 
  
  
  #Add column of zeros and assign col name(the code)
  colZeros<- rep(0, times=nIndustries+1)
  modMake<-cbind(modMake[,1:nCommodities],colZeros,modMake[,-(1:nCommodities)])
  colnames(modMake)[nCommodities+1]<- modModel$BiofuelsData$NewCommodityInfo$Code
  #Not clear why it changes the name, but I put it again
  colnames(modMake)[nCommodities+2]<-"T008"
  
  #Add 3 rows of zeros and assign row names(the code)
  rowZeros<- matrix(rep(0, times=(nCommodities+2)*3), nrow=3, ncol=nCommodities+1+1)
  #For the column names to match
  colnames(rowZeros)<-colnames(modMake)
  modMake<-rbind(modMake[1:nIndustries,],rowZeros,modMake[-(1:nIndustries),])
  rownames(modMake)[(nIndustries+1): (nIndustries+3)]<- modModel$BiofuelsData$NewIndustriesInfo$Code
  
  #Get row and column of similar sector
  similarSectorCode<-modModel$BiofuelsData$SimilarCommodityInfo$Code
  rowS<-getRowIndex(modMake,similarSectorCode)
  colS<-getColIndex(modMake,similarSectorCode)
  similarSectorPrice<-modModel$BiofuelsData$SimilarCommodityInfo$Price


  #Fill rows
  
  #For existing sectors
  for(i in 1:(nIndustries+3)){
    for(j in 1:(nCommodities+1)){
      if(i==nIndustries+1 | i==nIndustries+2 | i==nIndustries+3){ #the rows for the new industries
        if(j==nCommodities+1){
          modMake[i,j]<-TotalIndustryOutputNewInd[i-(nIndustries)]
        }
        else{
          modMake[i,j]<-0
        }
      }
      else if(i==rowS){ #the row for the similar industry
        if(j==colS){ # For the primary product
          modMake[i,j]<-transformGGEtoMillionUSD(modModel$BiofuelsData$FutureFuelGGE, similarSectorPrice)+UseTransactions[similarSectorCode,rownames(modMake)[nIndustries+1]]+UseTransactions[similarSectorCode,rownames(modMake)[nIndustries+2]]+UseTransactions[similarSectorCode,rownames(modMake)[nIndustries+3]]
        }
        else if(j==nCommodities+1){
          modMake[i,j]<-0
        }
        else{
          #Nothing, the same as before
        }
      }
      else{ #All other existing sectors
        cIndex<-getColIndex(modMake,rownames(modMake)[i])
        
        if(j==nCommodities+1){
          modMake[i,j]<-0
        }
        else if(length(cIndex)==0){ #It has no diagonal element/no "own commodity"
          #print(rownames(modMake)[i])
          #Do nothing
        }
        else if(length(cIndex)!=0){ #Has a "diagonal" element
          if(j==cIndex){ #For primary product
          CX<-modMake[i,j]
          modMake[i,j]<-CX+UseTransactions[colnames(modMake)[j],rownames(modMake)[nIndustries+1]]+UseTransactions[colnames(modMake)[j],rownames(modMake)[nIndustries+2]]+UseTransactions[colnames(modMake)[j],rownames(modMake)[nIndustries+3]]-(modModel$Use[colnames(modMake)[j],similarSectorCode]-UseTransactions[colnames(modMake)[j],similarSectorCode])
          }
        }
        else{
          #Nothing, remains the same
        }
      }
    }
  }
  
  # Recalculate totals
  modMake[nIndustries+3+1,]<-colSums(modMake[1:(nIndustries+3),]) #sum over rows for each column
  modMake[,nCommodities+1+1]<-rowSums(modMake[,1:(nCommodities+1)]) #sum over columns for each row
  
  modMake
}

#' Modify Make and Use Tables.
#' 
#' This function modifies the pre-saved Make and Use tables for adding the new biofuels sector.It first modify the industry (intermediate) uses and
#' the final users uses, including those for the new sectors. Then it obtains the Total Industry Output for the new industries. Then it calls
#' modifyMakeTable() to modify the Make table and then ends modifying the Use table, adjusting the Value Added components.
#' 
#' @param modModel the model under modification after initial adjustments for Use Table.
#' @param inputPurchases (nComm+1)x 3 matrix with the amount spent in each of the nComm existing commodities to produce 1 GGE of biofuel in each of the new industries.
#' @param valueAdded 2x3 matrix with the first 2 components of value added (Compensation of employees and Taxes on production and imports, less subsidies) required 
#' to produce 1 GGE of biofuel in each of the new industries.
#' @return the model with Make and Use tables updated.
#' 
#' Example: modifyMakeandUseTable(modModel,matrix(c(rep(0.1,300),rep(0,106),rep(0.1,100),rep(0,306),rep(0,300),rep(0.102,106)), nrow=406, ncol=3),matrix(c(rep(1,2),rep(2,2),rep(1.5,2)), nrow=2, ncol=3))
modifyMakeandUseTables <- function(modModel,inputPurchases, valueAdded){
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #MODIFY USE TABLE-INPUT PURCHASES
  
  logging::loginfo(paste("Updating Use Table ..."))
  
  originalUse<- modModel$Use
  modUse<-originalUse
  
  #Determine number of commodities and industries in originalUse
  nCommodities<- nrow(originalUse)-6 # 3 value added components + 3 totals
  nIndustries<- ncol(originalUse)-23 # 20 final users demand + 3 totals
  
  #Add 3 columns of zeros and assign col names(the code)
  colZeros<- matrix(rep(0, times=(nCommodities+6)*3),nrow = nCommodities+6, ncol=3)
  modUse<-cbind(modUse[,1:nIndustries],colZeros,modUse[,-(1:nIndustries)])
  colnames(modUse)[(nIndustries+1):(nIndustries+3)]<- modModel$BiofuelsData$NewIndustriesInfo$Code
  
  #Add row of zeros and assign row name(the code)
  rowZeros<- rep(0, times=(nIndustries+3)+23)
  modUse<-rbind(modUse[1:nCommodities,],rowZeros,modUse[-(1:nCommodities),])
  rownames(modUse)[nCommodities+1]<- modModel$BiofuelsData$NewCommodityInfo$Code
  
  #Get row and column of similar sector
  similarSectorCode<-modModel$BiofuelsData$SimilarCommodityInfo$Code
  rowS<-getRowIndex(modUse,similarSectorCode)
  colS<-getColIndex(modUse,similarSectorCode)
  similarSectorPrice<-modModel$BiofuelsData$SimilarCommodityInfo$Price 
  similarPrimaryPercentage<-modModel$BiofuelsData$PercentageFuelProducedByPrimary
  
  percentage<-modModel$BiofuelsData$BiofuelsPercentage
  biofuelWeightedPrice<-sum(modModel$BiofuelsData$NewIndustriesInfo$Price*modModel$BiofuelsData$NewIndustriesInfo$Percentage_Prod)
  
  #-------------------------------------------------------------------------------------------------------------------------------
  #FILL ROWS
  
  #Fill row nCommodities+1- cycle over columns
  for(j in 1:nIndustries){
    if(j!=colS){
      CY_j<-modUse[rowS,j]
      CGU_j<-transformMillionUSDtoGGE(CY_j,similarSectorPrice)
      
      FBU_j<- CGU_j*similarPrimaryPercentage*percentage
      FGU_j<-CGU_j-FBU_j
      
      Y_Fuel_j<- transformGGEtoMillionUSD(FGU_j,similarSectorPrice)
      Y_Biofuel_j<-transformGGEtoMillionUSD(FBU_j,biofuelWeightedPrice)
      
      modUse[nCommodities+1,j]<-Y_Biofuel_j
      modUse[rowS,j]<-Y_Fuel_j
    }
  }
  
  # Change final users demand
  # Assuming the same % for all the 20 categories that compose final users demand
  
  initialCol<-(nIndustries+3)+2
  for(j in initialCol:(initialCol+19)){
    
    Cdem_j<-modUse[rowS,j]
    CFU_j<-transformMillionUSDtoGGE(Cdem_j,similarSectorPrice)
    
    FFB_j<- CFU_j*similarPrimaryPercentage*percentage
    FFU_j<-CFU_j-FFB_j
    
    Y_Fuel_j<- transformGGEtoMillionUSD(FFU_j,similarSectorPrice)
    Y_Biofuel_j<-transformGGEtoMillionUSD(FFB_j,biofuelWeightedPrice)
    
    modUse[nCommodities+1,j]<-Y_Biofuel_j
    modUse[rowS,j]<-Y_Fuel_j
  }
  
  #-------------------------------------------------------------------------------------------------------------------------------
  #COLUMNS
  
  #Fill new columns nIndustries+1, nIndustries+2 and nIndustries+3- cycle over rows
  
  newTechGGEProd<-c(modModel$BiofuelsData$FutureBiofuelTech1, modModel$BiofuelsData$FutureBiofuelTech2,modModel$BiofuelsData$FutureBiofuelTech3)
  for(j in (nIndustries+1):(nIndustries+3)){
    for(i in 1:(nCommodities+1)){
      modUse[i,j]<-inputPurchases[i,j-nIndustries]*newTechGGEProd[j-nIndustries] #the input purchases $/GGE times the GGE produced by each tech
    }
  }
  
  #Update column for similar industry
  
  for(i in 1:(nCommodities+1)){
    CY_s<-originalUse[rowS,colS]
    CGU_s<-transformMillionUSDtoGGE(CY_s,similarSectorPrice)
    
    FBU_s<- CGU_s*similarPrimaryPercentage*percentage
    FGU_s<-CGU_s-FBU_s
    use_Fuelpercentage_s<- FGU_s/CGU_s
    if(i==rowS){ #Fuels use of Petroleum refineries
      Y_Fuel_s<-CY_s*(1-percentage)*use_Fuelpercentage_s
      modUse[i,colS]<-Y_Fuel_s
    }
    else if(i==nCommodities+1){ #Biofuels use of Petroleum refineries
      Y_Biofuel_s<-transformGGEtoMillionUSD(FBU_s*(1-percentage), biofuelWeightedPrice)
      modUse[i,colS]<-Y_Biofuel_s
    }
    else{
      Y_s<- originalUse[i,colS]*(1-percentage)
      modUse[i,colS]<-Y_s
    }
  }
  
  #-------------------------------------------------------------------------------------------------------------------------------
  
  # Recalculate totals
  
  #Total intermediate inputs
  modUse[nCommodities+1+1,1:(nIndustries+3)]<-colSums(modUse[1:(nCommodities+1),1:(nIndustries+3)]) #sum over rows for each column
  #Total intermediate use
  modUse[1:(nCommodities+1),nIndustries+3+1]<-rowSums(modUse[1:(nCommodities+1),1:(nIndustries+3)]) #sum over columns for each row
  #Total final uses
  modUse[1:(nCommodities+1),(nIndustries+3)+2+20]<-rowSums(modUse[1:(nCommodities+1),((nIndustries+3)+2):((nIndustries+3)+2+19)]) #sum over user demand columns for each row
  #Total commodity output
  modUse[1:(nCommodities+1),(nIndustries+3+2+21)]<-modUse[1:(nCommodities+1),(nIndustries+3+1)]+modUse[1:(nCommodities+1),(nIndustries+3+2+20)]
  #-------------------------------------------------------------------------------------------------------------------------------
  # ROWS- FIRST 2 ELEMENTS OF VALUE ADDED (COMPENSATION TO EMPLOYEES AND TAXES)
  # External cycle on rows (2 rows), internal on columns (nIndustries+3)
  
  for(i in (nCommodities+1+2):(nCommodities+1+3)){
    for(j in 1: (nIndustries+3)){
      if(j==colS){
        CVA_s<-modUse[i,j]
        FVA_s<-CVA_s*(1-percentage)
        modUse[i,j]<-FVA_s
      }
      else if(j==nIndustries+1 | j==nIndustries+2 | j==nIndustries+3)
      {
        modUse[i,j]<-valueAdded[i-(nCommodities+1+1),j-nIndustries]*newTechGGEProd[j-nIndustries] #the value added components in $/GGE times the GGE produced by each tech
      }
      else
      {
        #Nothing, the same value as before
      }
    }
  }
  
  # 3rd component for new industries. We know from initial calculations how much biofuel must be produced, so we just adjust the Gross operating surplus
  # to make the new industries literally produce that amount
  
  for(j in (nIndustries+1): (nIndustries+3)){
    expectedProdInMoney<- transformGGEtoMillionUSD(newTechGGEProd[j-(nIndustries)],biofuelWeightedPrice)
    grossOpSurplus<- expectedProdInMoney-modUse[nCommodities+1+1,j]-modUse[nCommodities+1+2,j]-modUse[nCommodities+1+3,j]
    modUse[nCommodities+1+4,j]<- grossOpSurplus
    
    #Recalculate Total value added
    modUse[nCommodities+1+5,j]<-modUse[nCommodities+1+2,j]+modUse[nCommodities+1+3,j]+modUse[nCommodities+1+4,j]
    #Recalculate Total Industry Output for new industries
    modUse[nCommodities+1+6,j]<-modUse[nCommodities+1+1,j]+ modUse[nCommodities+1+5,j]
  }
  
  #Note: The 3rd component (Gross operating surplus) for all existing sectors is missing. It will be updated after updating the Make table.
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # MODIFY MAKE TABLE
  logging::loginfo(paste("Updating Make Table ..."))
  newMake<- modifyMakeTable(modModel, modUse[1:(nCommodities+1), 1:(nIndustries+3)],modUse[nCommodities+1+6,(nIndustries+1):(nIndustries+3)])
  modModel$Make <- newMake
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # USE TABLE- ROWS- 3rd ELEMENTS OF VALUE ADDED (Gross operating surplus)
  for(j in 1: (nIndustries)){
    expectedProdInMoney<- newMake[colnames(modUse)[j],nCommodities+1+1]
    grossOpSurplus<- expectedProdInMoney-modUse[nCommodities+1+1,j]-modUse[nCommodities+1+2,j]-modUse[nCommodities+1+3,j]
    modUse[nCommodities+1+4,j]<- grossOpSurplus
    
    #Recalculate Total value added
    modUse[nCommodities+1+5,j]<-modUse[nCommodities+1+2,j]+modUse[nCommodities+1+3,j]+modUse[nCommodities+1+4,j]
    #Recalculate Total Industry Output for new industries
    modUse[nCommodities+1+6,j]<-modUse[nCommodities+1+1,j]+ modUse[nCommodities+1+5,j]
  }
  #------------------------------------------------------------------------------------------------------------------------------- 
  modModel$Use<- modUse
  modModel
}

#' This function modifies the B table created in buildEEIOModel() for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices.
#' @param newEnvData (# environmental flows x 1) column vector with the data for all the environmental flows per dollar of output for the new sector.
#' @param model the list of USEEIO model components and attributes under modification.
#' @return B matrix modified.
modifyBmatrix <- function(newEnvData, model){
  # This is not a pretty version, a prettier version is probably to modify the satellite tables.
  # Here I require that the untransformed B matrix is saved in model in buildEEIOModel().
  
  #Obtain data from original model construction
  primaryRegionAcronym<- model$specs$PrimaryRegionAcronym
  commodityByIndustryType<- model$specs$CommoditybyIndustryType
  modB<-model$B_untransformed
  
  # Since at the point this function is called, model$Industries is already updated, the original buildEEIOModel()
  # has already added a new column with zeros for this new sector. Therefore, now it is only necessary to fill it and not to add the column.
  
  #Determine number of industries in original B
  n<- model$BiofuelsData$OriginalIndustriesNum
  
  #Fill newEnvDataColumn and assign col name (the code)
  #modB<-cbind(modB,newEnvData)
  modB[,-(1:n)]<-newEnvData
  
  # If commodity model, transform B into a flowxcommodity matrix using market shares matrix
  if(commodityByIndustryType == "Commodity") {
    modB <- modB %*% model$V_n
    colnames(modB) <- tolower(paste(colnames(modB), primaryRegionAcronym, sep = "/"))
  }
  
  modB
}

#' Obtains the row index of a given industry or commodity in the Make or Use table
#' @param table the Make or the Use table
#' @param code the identifier BEA code (string)
#' @return the row index of the commodity/industry indicated through the code.
getRowIndex<-function (table, code){
  row<-which(rownames(table)==code)
  row
}

#' Obtains the column index of a given industry or commodity in the Make or Use table
#' @param table the Make or the Use table
#' @param code the identifier BEA code (string)
#' @return the column index of the commodity/industry indicated through the code.
getColIndex<-function (table, code){
  column<-which(colnames(table)==code)
  column
}

transformMillionUSDtoGGE <- function(MillionUSD, price){
  GGE<- MillionUSD*1/price*1000000
  GGE
}

transformGGEtoMillionUSD <- function(GGE, price){
  millionUSD<- GGE*price*1/1000000
  millionUSD
}

#' Do initial calculations to determine the amounts of GGE produced in future bio-economy by each of the 3 new industries.
#'
#' @param modelname Name of the model from a config file.
#' @param newIndustryInfo dataframe with 3 different technologies on the rows and the following variables (Code,Name, Primary_product, Price, Percentage_Prod)
#' @param newCommodityInfo dataframe with the information of the new commodity. Column variables (Code, Name, Primary_producers)
#' @param simCommodityInfo list with the information of the similar commodity. Elements (Code, Name, PrimaryProducer_Code, PrimaryProducer_Name, Price) 
#' @param percentage numeric (0,1] that refers to the \% of the original commodity (fuels) that the new commodity (biofuels) will replace.
#' @return The model with basic information added and initial calculations, and print the amount of biofuel in GGE produced by each technology/industry in the future bio-economy.

initialCalculations<- function(modelName,newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage){
  # Prepare the model based on original BEA data / obtain original Make and Use
  model<- prepareEEIOModel(modelName)
  
  #Obtain original Make and use tables
  originalMake<- model$Make
  originalUse<- model$Use
  
  #Get number of rows in Make
  nRowsMake<- nrow(originalMake)
  
  #Determine number of commodities and industries in originalMake, this will be the original number of commodities and industries
  nCommodities<- ncol(originalMake)-1
  nIndustries<- nrow(originalMake)-1 
  
  # Create modModel
  modModel<-model
  #---------------------------------------------------------------------------------------------------------------------------------
  
  # Obtain indeces for similar sector
  rowS_Make<-getRowIndex(originalMake,simCommodityInfo$Code)
  cols_Make<-getColIndex(originalMake,simCommodityInfo$Code)
  
  #---------------------------------------------------------------------------------------------------------------------------------  
  # CURRENT
  
  # How much does the 324110 industry produce of 324110 commodity in $ million USD? 
  CXP<- originalMake[rowS_Make,cols_Make]
  # How much does the 324110 industry produce of 324110 commodity in GGE? 
  CGP<- transformMillionUSDtoGGE(CXP, simCommodityInfo$Price)
  #% of total 324110 commodity produced by primary producer 324110?
  primaryFuelPercentage<- CXP/originalMake[nRowsMake,cols_Make]
  
  # FUTURE
  
  # How much will the bio-product replace in GGE? 
  FBP<-percentage*CGP
  # How much will the 324110 industry produce of 324110 commodity in GGE?
  FGP_petr<- (1-percentage)*CGP
  # How much will the 324110 industry produce of 324110 commodity in $million USD? 
  FX<- transformGGEtoMillionUSD(FGP_petr,simCommodityInfo$Price) 
  # How much will the Gas fermentation industry produce of bio-product in GGE? 
  FGP_tech1<-FBP*newIndustryInfo[1,5]
  # How much will the Thermochemical industry produce of bio-product in GGE? 
  FGP_tech2<-FBP*newIndustryInfo[2,5]
  # How much will the Biochemical industry produce of bio-product in GGE? 
  FGP_tech3<-FBP*newIndustryInfo[3,5]
  
  # Save data in model
  
  modModel$BiofuelsData<- list(PercentageFuelProducedByPrimary=primaryFuelPercentage,FutureBiofuelGGE=FBP,FutureFuelGGE=FGP_petr,FutureBiofuelTech1= FGP_tech1,FutureBiofuelTech2=FGP_tech2, FutureBiofuelTech3=FGP_tech3,NewIndustriesInfo= newIndustryInfo,NewCommodityInfo= newCommodityInfo, SimilarCommodityInfo=simCommodityInfo, BiofuelsPercentage=percentage, OriginalCommoditiesNum=nCommodities, OriginalIndustriesNum=nIndustries)
  print(paste("The data you need to gather for input purchases and value added must correspond to the following GGE gallons produced for each new industry:", "Technology 1 (GGE)=",FGP_tech1,"Technology 2 (GGE)=", FGP_tech2, "Technology 3 (GGE)=", FGP_tech3 ))
  modModel
}



#' Adds a new biofuels sector to an existing model. This includes 3 new industries and 1 new commodity.
#' 
#' This function adds 3 new industries (that correspond to 3 different technologies) to produce 1 new commodity (biofuels).
#' This new commodity (biofuels) will be a perfect substitute of fuels (commodity) produced by Petroleum Refineries.
#' This function prepares (read and organize data from the BEA tables), modify the Make and Use table to add the new biofuels sector, 
#' build the model (create the required matrices) and update B and W matrices after its construction to return a model including the new sector
#' ready for calculation.See details of the methodology on "LCA New tech Methodology (v01 CGA 2020-07-23).pdf".
#' 
#' @param modelname Name of the model from a config file.
#' @param newIndustryInfo dataframe with 3 different technologies on the rows and the following variables (Code,Name, Primary_product, Price, Percentage_Prod) on the columns.
#' @param newCommodityInfo dataframe with the information of the new commodity. Column variables (Code, Name, Primary_producers).
#' @param simCommodityInfo list with the information of the similar commodity. Elements: (Code, Name, PrimaryProducer_Code, PrimaryProducer_Name, Price). 
#' @param percentage numeric (0,1] that refers to the \% of the original commodity (fuels) that the new commodity (biofuels) will replace.
#' @param inputPurchases (nComm+1)x 3 matrix with the amount spent in each of the nComm existing commodities to produce 1 GGE of biofuel in each of the new industries.
#' @param valueAdded 2x3 matrix with the first 2 components of value added (Compensation of employees and Taxes on production and imports, less subsidies) required 
#' to produce 1 GGE of biofuel in each of the new industries.
#' @param newEnvData (# environmental flows x 3) matrix with the data for all the environmental flows per dollar of output for the new industries.
#' @export
#' @return A list with USEEIO model components and attributes modified and ready for calculation.
addBiofuelsSector<- function(modelName, newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage, inputPurchases, valueAdded, newEnvData ){
  
  # Perfom initial calculations
  modModel<-initialCalculations(modelName,newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage)
  
  # Modify Make and Use Tables
  logging::loginfo(paste("Updating Make and Use Tables ..."))
  modModel<-modifyMakeandUseTables(modModel,inputPurchases, valueAdded)

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # OTHER MODEL VARIABLES UPDATES
  
  #Update model Industries, Commodities
  modModel$Commodities= append(modModel$Commodities,modModel$BiofuelsData$NewCommodityInfo$Code, after= length(modModel$Commodities))
  modModel$Industries= append(modModel$Industries,modModel$BiofuelsData$NewIndustriesInfo$Code, after= length(modModel$Industries))
  
  # Update model MakeTransactions, UseTransactions, FinalDemand and UseCommodityOutput for normalization
  modModel$MakeTransactions <- modModel$Make[modModel$Industries, modModel$Commodities] * 1E6 # data frame, values are in dollars ($)
  modModel$UseTransactions <- modModel$Use[modModel$Commodities, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  
  #updatedMakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($) DON'T REMEMBER IF I DIDN'T ERASE THIS
  modModel$FinalDemand <- modModel$Use[modModel$Commodities, modModel$BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  
  #This is their original version, obtaining the number from the use table
  updatedUseCommodityOutput <- as.data.frame(rowSums(cbind(modModel$UseTransactions, modModel$FinalDemand))) # data frame, values are in dollars ($)
  #This is my version obtaining the number for the make table, seems that it is not convenient to use this because sectors S00402 and S00300 has zero Total commodity Output and then there is a problem when dividing by zero
    #updatedUseCommodityOutput <-modModel$Make[nrow(modModel$Make),1:newNumSec]
  #update model$BEA$UseCommodityOutput because is the one used in generateMarketSharesfromMake()
  modModel$BEA$UseCommodityOutput <- updatedUseCommodityOutput
  
  #This is their original version, obtaining the number from the make table
  updatedMakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($)
  #This is my version obtaining the number for the use table
    #updatedMakeIndustryOutput <-modModel$Use[nrow(modModel$Use),1:newNumSec]
  #update model$BEA$MakeIndustryOutput because is the one used in generateDirectRequirementsfromUse()
  modModel$BEA$MakeIndustryOutput<-updatedMakeIndustryOutput
  #---------------------------------------------------------------------------------------------------------------------------------
  #Build the model with the modified tables
  modModel<- buildEEIOModel(modModel)
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # Update B and W matrices
  
  #Modify B matrix
  logging::loginfo(paste("Updating B matrix ..."))
  newB<- modifyBmatrix(newEnvData, modModel)
  modModel$B<-newB
  
  # Update W matrix for Bioeconomy new sectors 
  updatedUseValueAdded<- modModel$Use[modModel$BEA$ValueAddedCodes, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  modModel$W <- as.matrix(updatedUseValueAdded)
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  logging::loginfo("Bioeconomy model correctly created.")
  
  return(modModel)
  
}


#' Function that adds one new industry and one new commodity. (V1 of methodology-Legacy) 
#' 
#' This function prepares (read and organize data from the BEA tables), modify the Make and Use table to add the new sector, 
#' build the model (create the required matrices) and update B and W matrices after its construction to return a bioeconomy model
#' ready for calculation. 
#' HERE FOR REFERENCE, WON'T WORK BECAUSE I'VE CHANGE FUNCTIONS THAT MODIFY MAKE, USE AND B
#' 
#' @param modelname Name of the model from a config file.
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices.
#' @param newSectorName string/character that refers to the name given to the new sector.
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric (0,1] that refers to the \% of output of the original sector that new sector will produce.
#' @param inputPurchases (n+1)x 1 column vector with the amount spent in each of the n existing commodities to produce "Total Industry Output" of the new sector.
#' @param newEnvData (# environmental flows x 1) column vector with the data for all the environmental flows per dollar of output for the new sector.
#' @export
#' @return A list with USEEIO model components and attributes modified and ready for calculation.
createBioeconomyModel<- function(modelname,newSectorCode,newSectorName, similarSectorCode,percentage, inputPurchases, newEnvData) {
  
  # Prepare the model based on original BEA data
  model<- useeior::prepareEEIOModel(modelname)
  
  #---------------------------------------------------------------------------------------------------------------------------------
  # Modify the original Tables
  
  modModel<-model
  
  #Obtain original Make and use tables
  originalMake<- modModel$Make
  originalUse<- modModel$Use
  

  
  #Modify Make Table
  logging::loginfo(paste("Updating Make Table ..."))
  newMake<- modifyMakeTable(newSectorCode, similarSectorCode, percentage, originalMake, inputPurchases)
  modModel$Make <- newMake
  newNumSec<- nrow(newMake)-1
  newSectorTotalIndustryOutput<-newMake[newNumSec+1,newNumSec]
  
  #Modify Use Table
  logging::loginfo(paste("Updating Use Table ..."))
  newUse<- modifyUseTable(newSectorCode, similarSectorCode, percentage, inputPurchases, originalUse, newSectorTotalIndustryOutput)
  modModel$Use<- newUse
  
  #Update model Industries, Commodities
  modModel$Commodities= append(modModel$Commodities,newSectorCode, after= length(modModel$Commodities))
  modModel$Industries= append(modModel$Industries,newSectorCode, after= length(modModel$Industries))
  
  # Update model MakeTransactions, UseTransactions, FinalDemand and UseCommodityOutput for normalization
  modModel$MakeTransactions <- modModel$Make[modModel$Industries, modModel$Commodities] * 1E6 # data frame, values are in dollars ($)
  modModel$UseTransactions <- modModel$Use[modModel$Commodities, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  
  #updatedMakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($) DON'T REMEMBER IF I DIDN'T ERASE THIS
  modModel$FinalDemand <- modModel$Use[modModel$Commodities, modModel$BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  
  #This is their original version, obtaining the number from the use table
  updatedUseCommodityOutput <- as.data.frame(rowSums(cbind(modModel$UseTransactions, modModel$FinalDemand))) # data frame, values are in dollars ($)
  #This is my version obtaining the number for the make table, seems that it is not convenient to use this because sectors S00402 and S00300 has zero Total commodity Output and then there is a problem when dividing by zero
  #updatedUseCommodityOutput <-modModel$Make[nrow(modModel$Make),1:newNumSec]
  #update model$BEA$UseCommodityOutput because is the one used in generateMarketSharesfromMake()
  modModel$BEA$UseCommodityOutput <- updatedUseCommodityOutput
  
  #This is their original version, obtaining the number from the make table
  updatedMakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($)
  #This is my version obtaining the number for the use table
  #updatedMakeIndustryOutput <-modModel$Use[nrow(modModel$Use),1:newNumSec]
  #update model$BEA$MakeIndustryOutput because is the one used in generateDirectRequirementsfromUse()
  modModel$BEA$MakeIndustryOutput<-updatedMakeIndustryOutput
  #---------------------------------------------------------------------------------------------------------------------------------
  #Build the model with the modified tables
  modModel<- buildEEIOModel(modModel)
  
  # #Re-generate matrices: Not updating for Domestic
  # modModel$V_n <- generateMarketSharesfromMake(modModel) # normalized Make
  # modModel$U_n <- generateDirectRequirementsfromUse(modModel, domestic = FALSE) #normalized Use , Warning: Not updated for domestic!
  # 
  # updatedUseValueAdded<- modModel$Use[modModel$BEA$ValueAddedCodes, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  # modModel$W <- as.matrix(updatedUseValueAdded)
  # 
  # # Assuming CommoditybyIndustryType == "Commodity"
  # logging::loginfo(paste("Updating commodityxcommodity direct requirement matrix ..."))
  # modModel$A <- modModel$U_n %*% modModel$V_n
  # 
  # #Modify B matrix
  # logging::loginfo(paste("Updating B matrix ..."))
  # newB<- modifyBmatrix(newSectorCode,newEnvData, originalB, modModel$specs$PrimaryRegionAcronym)
  # modModel$B<-newB
  # 
  # # Transform B into a flowxcommodity matrix using market shares matrix for commodity models
  # modModel$B <- modModel$B %*% modModel$V_n
  # colnames(modModel$B) <- tolower(paste(colnames(modModel$B), modModel$specs$PrimaryRegionAcronym, sep = "/"))
  # 
  # #Re-calculate Total Requirements Matrix L=(I-A)^(-1)
  # logging::loginfo("Re-calculating total requirements matrix...")
  # I <- diag(nrow(modModel$A))
  # modModel$L <- solve(I - modModel$A)
  # 
  # # Re-calculate total emissions/resource use per dollar (M)
  # logging::loginfo("Re-calculating total emissions per dollar matrix...")
  # modModel$M <- modModel$B %*% modModel$L
  # colnames(modModel$M) <- tolower(paste(colnames(modModel$M), modModel$specs$PrimaryRegionAcronym, sep = "/"))
  # 
  # # Re-calculate total impacts per dollar (U), impact category x sector
  # modModel$U <- modModel$C %*% modModel$M
  # 
  # #Update model$SectorNames
  # modModel$SectorNames<-rbind(modModel$SectorNames,c(newSectorCode,newSectorName))
  # 
  #--------------------------------------------------------------------------------------------------------------------------------- 
  # Update B and W matrices
  
  #Modify B matrix
  logging::loginfo(paste("Updating B matrix ..."))
  newB<- modifyBmatrix(newSectorCode,newEnvData, modModel)
  modModel$B<-newB
  
  # Update W matrix for Bioeconomy new sectors 
  updatedUseValueAdded<- modModel$Use[modModel$BEA$ValueAddedCodes, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  modModel$W <- as.matrix(updatedUseValueAdded)
  
  logging::loginfo("Bioeconomy model correctly created.")
  
  return(modModel)
}