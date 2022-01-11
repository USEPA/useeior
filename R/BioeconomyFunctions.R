# This script contains the functions that modificate the Make and Use tables, the A and B matrices and the Y vector
# to recalculate the USEEIO model with an additional sector (3 new industries that produce 1 new commodity)
# More information about the methodology can be found at manuscript.

#' Modify Make BEA Table.
#' 
#' This function modifies the pre-saved Make table for adding the new sector.
#' 
#' @param modModel the model under modification after initial adjustments for Use Table.
#' @param totalF_UseSimComm the modified total commodity output of the similar commodity in the Use table, 
#' this will be the future production of the similar commodity.
#' @return a list with a dataframe with the modified Make table, and the modified model. In that order.
#' 
#' Example: modifyMakeTable(modModel,newProd)
modifyMakeTable <- function(modModel, totalF_UseSimComm){
  
  originalMake<-modModel$BiofuelsData$OriginalBEA$Make
  modMake<-originalMake
  
  nNewI<-modModel$BiofuelsData$nNewIndustries
  whichI<-modModel$BiofuelsData$whichNewIn
  
  #Determine number of commodities and industries in originalMake
  nCommodities<- ncol(originalMake)-1
  nIndustries<- nrow(originalMake)-1 
  
  
  #Add column of zeros and assign col name(the code)
  colZeros<- rep(0, times=nIndustries+1)
  modMake<-cbind(modMake[,1:nCommodities],colZeros,modMake[,-(1:nCommodities)])
  colnames(modMake)[nCommodities+1]<- modModel$BiofuelsData$NewCommodityInfo$Code
  #Not clear why it changes the name, but I put it again
  colnames(modMake)[nCommodities+2]<-"T008"
  
  #Add nNewI rows of zeros and assign row names(the code)
  rowZeros<- matrix(rep(0, times=(nCommodities+2)*nNewI), nrow=nNewI, ncol=nCommodities+1+1)
  #For the column names to match
  colnames(rowZeros)<-colnames(modMake)
  modMake<-rbind(modMake[1:nIndustries,],rowZeros,modMake[-(1:nIndustries),])
  rownames(modMake)[(nIndustries+1): (nIndustries+nNewI)]<- modModel$BiofuelsData$NewIndustriesInfo$Code[whichI]
  
  #Get row and column of similar sector
  similarSectorCode<-modModel$BiofuelsData$SimilarCommodityInfo$Code
  rowS<-getRowIndex(modMake,similarSectorCode)
  colS<-getColIndex(modMake,similarSectorCode)
  similarSectorPrice<-modModel$BiofuelsData$SimilarCommodityInfo$Price
  
  #-------------------------------------------------------------------------------------------------------------------------------
  # FUTURE PRODUCTION OF SIMILAR-COMMODITY BY THE SIMILAR-INDUSTRY IN GGE
  
  # How much is currently produced of 324110 commodity in $ USD by secondary industries?
  
  CX_sec<-sum(originalMake[1:(nIndustries),colS])-originalMake[rowS,colS]
  
  # How much is currently produced of 324110 commodity in GGE by secondary industries?
  CGP_sec<-transformUSDtoGGE(CX_sec,similarSectorPrice)
  # How much will the 324110 industry produce of 324110 commodity in GGE?
  FGP_petr<- transformUSDtoGGE(totalF_UseSimComm,similarSectorPrice)-CGP_sec
  
  # How much will the 324110 industry produce of 324110 commodity in $ USD? 
  FX<- transformGGEtoUSD(FGP_petr,similarSectorPrice) 
  
  # Save data in model
  #browser()
  modModel$BiofuelsData["FutureFuelGGE"]<-FGP_petr
  
  #-------------------------------------------------------------------------------------------------------------------------------
  
  #Fill rows
  
  FGP_cols<-as.matrix(modModel$BiofuelsData[c(3,4,5)])[whichI]
  newIPricesCols<-as.matrix(modModel$BiofuelsData$NewIndustriesInfo[whichI,4])
  #For existing sectors
  for(i in 1:(nIndustries+nNewI)){
    for(j in 1:(nCommodities+1)){
      if(i==nIndustries+1 | i==nIndustries+2 | i==nIndustries+3){ #the rows for the new industries
        if(j==nCommodities+1){#The column of the new commodity
          FGP_tech_i<-as.numeric(FGP_cols[i-nIndustries])
          FX_tech_i<-transformGGEtoUSD(FGP_tech_i,newIPricesCols[i-nIndustries])
          modMake[i,j]<-FX_tech_i
        }
        else{ #All existing commodities
          modMake[i,j]<-0
        }
      }
      else if(i==rowS){ #the row for the similar industry
        if(j==colS){ # For the primary product/similar commodity
          modMake[i,j]<-transformGGEtoUSD(modModel$BiofuelsData$FutureFuelGGE, similarSectorPrice)
        }
        else if(j==nCommodities+1){# for the new bio-commodity
          modMake[i,j]<-0
        }
        else{
          #Nothing, the same as before
        }
      }
      else{ #All other existing sectors
        cIndex<-getColIndex(modMake,rownames(modMake)[i])
        
        if(j==nCommodities+1){# for the new bio-commodity
          modMake[i,j]<-0
        }
        # else if(length(cIndex)==0){ #It has no diagonal element/no "own commodity"
        #   #print(rownames(modMake)[i])
        #   #Do nothing
        # }
        # else if(length(cIndex)!=0){ #Has a "diagonal" element
        #   if(j==cIndex){ #For primary product
        #   CX<-modMake[i,j]
        #   modMake[i,j]<-CX
        #   }
        # }
        # else{
        #   #Nothing, remains the same
        #}
      }
    }
  }
  
  # Recalculate totals
  
  # Total commodity output
  modMake[nIndustries+nNewI+1,]<-colSums(modMake[1:(nIndustries+nNewI),]) #sum over rows for each column
  # Total industry output
  modMake[,nCommodities+1+1]<-rowSums(modMake[,1:(nCommodities+1)]) #sum over columns for each row
  
  list(modMake, modModel)
}

#' Augments Use table with information for the new industries producing the new commodity.
#' 
#' @param modModel the model under modification after initial calculations.
#' @param inputPurchases (nComm+1)x #newTech matrix with the amount spent in each of the nComm existing commodities to produce 1 physical unit of the new commodity in each of the new industries.
#' @param valueAdded 3x #newTech matrix with the 3 components of value added (Compensation of employees; Taxes on production and imports, less subsidies; Gross operating surplus) required 
#' to produce 1 physical unit of the new commodity in each of the new industries.
#' @param isDomestic boolean that takes TRUE if the modification is for domestic use table.
#' @result List that includes the modified Use table, the model with updates and total future use of the similar commodity, in that order.
modifyUseTable<-function(modModel, inputPurchases, valueAdded, isDomestic){
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #MODIFY USE TABLE-INPUT PURCHASES
  
  #browser()
  
  originalUse<- modModel$BiofuelsData$OriginalBEA$Use
  if(isDomestic==TRUE){
    originalUse<- modModel$BiofuelsData$OriginalBEA$DomesticUse
  }
  modUse<-originalUse
  
  nNewI<-modModel$BiofuelsData$nNewIndustries
  whichI<-modModel$BiofuelsData$whichNewIn
  #Determine number of commodities and industries in originalUse
  nCommodities<- nrow(originalUse)-6 # 3 value added components + 3 totals
  nIndustries<- ncol(originalUse)-23 # 20 final users demand + 3 totals
  
  #Add nNewI columns of zeros and assign col names(the code)
  colZeros<- matrix(rep(0, times=(nCommodities+6)*nNewI),nrow = nCommodities+6, ncol=nNewI)
  modUse<-cbind(modUse[,1:nIndustries],colZeros,modUse[,-(1:nIndustries)])
  colnames(modUse)[(nIndustries+1):(nIndustries+nNewI)]<- modModel$BiofuelsData$NewIndustriesInfo$Code[whichI]
  
  #Add row of zeros and assign row name(the code)
  rowZeros<- rep(0, times=(nIndustries+nNewI)+23)
  modUse<-rbind(modUse[1:nCommodities,],rowZeros,modUse[-(1:nCommodities),])
  rownames(modUse)[nCommodities+1]<- modModel$BiofuelsData$NewCommodityInfo$Code
  
  #Get row and column of similar sector
  similarSectorCode<-modModel$BiofuelsData$SimilarCommodityInfo$Code
  rowS<-getRowIndex(modUse,similarSectorCode)
  colS<-getColIndex(modUse,similarSectorCode)
  similarSectorPrice<-modModel$BiofuelsData$SimilarCommodityInfo$Price 
  
  
  percentage<-modModel$BiofuelsData$BiofuelsPercentage
  biofuelWeightedPrice<-sum(modModel$BiofuelsData$NewIndustriesInfo$Price*modModel$BiofuelsData$NewIndustriesInfo$Percentage_Prod)
  
  #-------------------------------------------------------------------------------------------------------------------------------
  #FILL ROWS
  
  totalUseBiofuel_G<-0
  
  #Fill row nCommodities+1- cycle over columns
  for(j in 1:nIndustries){
    
    CY_j<-modUse[rowS,j]
    CGU_j<-transformUSDtoGGE(CY_j,similarSectorPrice)
    
    FBU_j<- CGU_j*percentage
    FGU_j<-CGU_j-FBU_j
    
    Y_Fuel_j<- transformGGEtoUSD(FGU_j,similarSectorPrice)
    Y_Biofuel_j<-transformGGEtoUSD(FBU_j,biofuelWeightedPrice)
    
    modUse[nCommodities+1,j]<-Y_Biofuel_j
    modUse[rowS,j]<-Y_Fuel_j
    
    totalUseBiofuel_G<- totalUseBiofuel_G+FBU_j #Adding uses in GGE for all existing industries
    
  }
  
  # Change final users demand
  # Assuming the same % for all the 20 categories that compose final users demand
  
  initialCol<-(nIndustries+nNewI)+2
  for(j in initialCol:(initialCol+19)){
    
    Cdem_j<-modUse[rowS,j]
    CFU_j<-transformUSDtoGGE(Cdem_j,similarSectorPrice)
    
    FFB_j<- CFU_j*percentage
    FFU_j<-CFU_j-FFB_j
    
    Y_Fuel_j<- transformGGEtoUSD(FFU_j,similarSectorPrice)
    Y_Biofuel_j<-transformGGEtoUSD(FFB_j,biofuelWeightedPrice)
    
    modUse[nCommodities+1,j]<-Y_Biofuel_j
    modUse[rowS,j]<-Y_Fuel_j
    
    totalUseBiofuel_G<- totalUseBiofuel_G+FFB_j #Adding uses in GGE for all final users
  }
  
  #-------------------------------------------------------------------------------------------------------------------------------
  #Here using the use of the biofuel, we calculate the future amounts that will be produced
  
  # FUTURE PRODUCTION
  #browser()
  # How much will be produced of the bio-product in GGE? 
  FBP<-totalUseBiofuel_G
  # How much will the Gas fermentation industry produce of bio-product in GGE? 
  FGP_tech1<-FBP*modModel$BiofuelsData$NewIndustriesInfo$Percentage_Prod[1]
  # How much will the Guerbet Reaction industry produce of bio-product in GGE? 
  FGP_tech2<-FBP*modModel$BiofuelsData$NewIndustriesInfo$Percentage_Prod[2]
  # How much will the Fischer Tropsch industry produce of bio-product in GGE? 
  FGP_tech3<-FBP*modModel$BiofuelsData$NewIndustriesInfo$Percentage_Prod[3]
  
  # Save data in model
  
  modModel$BiofuelsData["FutureBiofuelGGE"]<-FBP
  modModel$BiofuelsData["FutureBiofuelTech1"]<-FGP_tech1
  modModel$BiofuelsData["FutureBiofuelTech2"]<-FGP_tech2
  modModel$BiofuelsData["FutureBiofuelTech3"]<-FGP_tech3
  
  #-------------------------------------------------------------------------------------------------------------------------------
  #COLUMNS
  
  #Fill new columns nIndustries+1, nIndustries+2 and nIndustries+3- cycle over rows
  
  newTechGGEProd<-c(modModel$BiofuelsData$FutureBiofuelTech1, modModel$BiofuelsData$FutureBiofuelTech2,modModel$BiofuelsData$FutureBiofuelTech3)
  
  #browser()
  inputPurchasesCols<-as.matrix(inputPurchases[,whichI])
  newTechGGEProdCols<-newTechGGEProd[whichI]
  for(j in (nIndustries+1):(nIndustries+nNewI)){
    for(i in 1:(nCommodities+1)){
      
      
      modUse[i,j]<-(inputPurchasesCols[i,j-nIndustries]*newTechGGEProdCols[j-nIndustries]) #the input purchases $/GGE times the GGE produced by each tech (in dollars)
    }
  }
  #-------------------------------------------------------------------------------------------------------------------------------
  # VALUE ADDED
  #For existing infustries remain the same, for new industries based on external data
  
  # ROWS- ALL 3 ELEMENTS OF VALUE ADDED (COMPENSATION TO EMPLOYEES, TAXES AND GROSS OPERATING SURPLUS)
  # External cycle on rows (3 rows), internal on columns (nIndustries+3)
  
  valueAddedCols<-as.matrix(valueAdded[,whichI])
  for(i in (nCommodities+1+2):(nCommodities+1+4)){
    for(j in (nIndustries+1): (nIndustries+nNewI)){
      
      modUse[i,j]<-(valueAddedCols[i-(nCommodities+1+1),j-nIndustries]*newTechGGEProdCols[j-nIndustries]) #the value added components in $/GGE times the GGE produced by each tech (in dollars)
    }
  }
  
  #-------------------------------------------------------------------------------------------------------------------------------
  
  # Recalculate totals
  
  #Total intermediate inputs
  modUse[nCommodities+1+1,1:(nIndustries+nNewI)]<-colSums(modUse[1:(nCommodities+1),1:(nIndustries+nNewI)]) #sum over rows for each column
  #Total Value Added
  modUse[nCommodities+1+5,1:(nIndustries+nNewI)]<-colSums(modUse[((nCommodities+1+2):(nCommodities+1+4)),1:(nIndustries+nNewI)])#sum over rows of value added for each column
  #Total Industry Output
  modUse[nCommodities+1+6,1:(nIndustries+nNewI)]<-modUse[nCommodities+1+1,1:(nIndustries+nNewI)]+ modUse[nCommodities+1+5,1:(nIndustries+nNewI)] #sum the total intermediate inputs and the total value added
  #Total intermediate use
  modUse[1:(nCommodities+1),nIndustries+nNewI+1]<-rowSums(modUse[1:(nCommodities+1),1:(nIndustries+nNewI)]) #sum over columns for each row
  #Total final uses
  modUse[1:(nCommodities+1),(nIndustries+nNewI)+2+20]<-rowSums(modUse[1:(nCommodities+1),((nIndustries+nNewI)+2):((nIndustries+nNewI)+2+19)]) #sum over user demand columns for each row
  #Total commodity output
  modUse[1:(nCommodities+1),(nIndustries+nNewI+2+21)]<-modUse[1:(nCommodities+1),(nIndustries+nNewI+1)]+modUse[1:(nCommodities+1),(nIndustries+nNewI+2+20)]
  #-------------------------------------------------------------------------------------------------------------------------------
  totalF_UseSimComm<-modUse[rowS,(nIndustries+nNewI+2+21)] #The total commodity output of the similar industry
  
  list(modUse, modModel, totalF_UseSimComm)
}

#' Function that implements the analytical methodology proposed in paper to rebalance Make and Use augmented tables.
#' 
#' @param modModel the list of USEEIO model components and attributes under modification.
#' @param newMake Augmented Make matrix.
#' @param modUse Augmented Use matrix.
#' @result A list with balanced Make (1st position) and Use tables (2nd position).
rebalanceMakeUseAnalytical<-function(modModel, newMake, modUse){
  
  nNewI<-modModel$BiofuelsData$nNewIndustries
  
  #Determine number of commodities and industries in originalUse
  nCommodities<-modModel$BiofuelsData$OriginalBEA$nCommodities
  nIndustries<-modModel$BiofuelsData$OriginalBEA$nIndustries
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #REBALANCE MAKE AND USE TABLE, VIA ANALYTICAL FRAMEWORK, SCALING COLUMNS TO KEEP RECIPE UNCHANGED
  nMake_Commodities<- ncol(modModel$BiofuelsData$OriginalBEA$Make)-1 # Number of commodities in original Make
  nMake_Industries<- nrow(modModel$BiofuelsData$OriginalBEA$Make)-1 # Number of commodities in original Use
  
  
  #Obtain matrices and vector needed
  U1<-as.matrix(modUse[1:(nCommodities+1), 1:(nIndustries+nNewI)]) #Use transactions
  v1<-modUse[(nCommodities+1+2):(nCommodities+1+4),1:(nIndustries+nNewI)]#Value added 3x(nIndustries+3)
  V1<-as.matrix(newMake[1:(nMake_Industries+nNewI),1:(nMake_Commodities+1)]) #Make transactions
  y1_c<-modUse[1:(nCommodities+1),(nIndustries+nNewI)+2+20] 
  z_I<-modUse[nrow(modUse),1:(nIndustries+nNewI)] 
  z_I_diag<-diag(as.vector(z_I))
  z_I_diag_inverse<-solve(z_I_diag)
  U1_n<-U1%*%z_I_diag_inverse
  z_P<-newMake[nrow(newMake),1:(nMake_Commodities+1)] 
  #Since S00402 and S00300 have 0 output it generates problems when inverting matrix z_P-diag. Then I replace the 0 with 1 to avoid that.
  z_P['S00402']<-1
  z_P['S00300']<-1
  z_P_diag<-diag(as.vector(z_P))
  z_P_diag_inverse<-solve(z_P_diag)
  V1_n<-V1%*%z_P_diag_inverse
  y1_I<-V1_n%*%y1_c
  
  #Obtain Unbalanced A and L matrices
  
  A_cxc_Un<-U1_n%*%V1_n
  A_IxI_Un<-V1_n%*%U1_n
  
  I_c <- diag(nrow(A_cxc_Un))
  I_I <- diag(nrow(A_IxI_Un))
  
  L_cxc_Un<-solve(I_c - A_cxc_Un)
  L_IxI_Un<-solve(I_I - A_IxI_Un)
  
  #Obtain total requirements for demand vectors y1_c and y1_I
  x1<-L_cxc_Un%*%y1_c
  z1<-L_IxI_Un%*%y1_I
  
  #browser()
  #Obtain scaling multipliers
  m_Use<-z_I_diag_inverse%*%diag(as.vector(z1))
  m_Make<-z_P_diag_inverse%*%diag(as.vector(x1))
  
  #---Obtain balanced Use table
  #Obtain balanced Use transactions
  U2<-U1%*%m_Use
  #Obtain balanced Value added
  v2<-as.matrix(v1)%*%m_Use
  
  #Reconstruct/Refill use table
  modUse2<-modUse
  modUse2[1:(nCommodities+1), 1:(nIndustries+nNewI)]<-U2
  modUse2[(nCommodities+1+2):(nCommodities+1+4),1:(nIndustries+nNewI)]<-v2
  
  #Recalculate all totals
  
  #Total intermediate inputs
  modUse2[nCommodities+1+1,1:(nIndustries+nNewI)]<-colSums(modUse2[1:(nCommodities+1),1:(nIndustries+nNewI)]) #sum over rows for each column
  #Total intermediate use
  modUse2[1:(nCommodities+1),nIndustries+nNewI+1]<-rowSums(modUse2[1:(nCommodities+1),1:(nIndustries+nNewI)]) #sum over columns for each row
  #Total final uses
  modUse2[1:(nCommodities+1),(nIndustries+nNewI)+2+20]<-rowSums(modUse2[1:(nCommodities+1),((nIndustries+nNewI)+2):((nIndustries+nNewI)+2+19)]) #sum over user demand columns for each row
  #Total commodity output
  modUse2[1:(nCommodities+1),(nIndustries+nNewI+2+21)]<-modUse2[1:(nCommodities+1),(nIndustries+nNewI+1)]+modUse2[1:(nCommodities+1),(nIndustries+nNewI+2+20)]
  
  for(j in 1: (nIndustries+nNewI)){
    #Recalculate Total value added
    modUse2[nCommodities+1+5,j]<-modUse2[nCommodities+1+2,j]+modUse2[nCommodities+1+3,j]+modUse2[nCommodities+1+4,j]
    #Recalculate Total Industry Output 
    modUse2[nCommodities+1+6,j]<-modUse2[nCommodities+1+1,j]+ modUse2[nCommodities+1+5,j]
  }
  
  #---Obtain balanced Make table
  V2<-V1%*%m_Make
  
  #Reconstruct/Refill Make table
  modMake2<-newMake
  modMake2[1:(nMake_Industries+nNewI),1:(nMake_Commodities+1)]<-V2
  
  #Recalculate totals
  
  modMake2[nMake_Industries+nNewI+1,]<-colSums(modMake2[1:(nMake_Industries+nNewI),]) #sum over rows for each column - Total Commodity Output
  modMake2[,nMake_Commodities+1+1]<-rowSums(modMake2[,1:(nMake_Commodities+1)]) #sum over columns for each row- Total Industry Output
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  list(modMake2, modUse2) #return a list with the rebalanced Make and Use tables
  
}
#' Modify Make and Use Tables.
#' 
#' This function modifies the pre-saved Make and Use tables for adding the new sector.
#' First calls modifyUseTable(), then modifyMakeTable() and then rebalance the augmented Make and Use Tables. 
#' 
#' @param modModel the model under modification after initial calculations.
#' @param inputPurchases (nComm+1)x #newTech matrix with the amount spent in each of the nComm existing commodities to produce 1 physical unit of the new commodity in each of the new industries.
#' @param valueAdded 3x #newTech matrix with the 3 components of value added (Compensation of employees; Taxes on production and imports, less subsidies; Gross operating surplus) required 
#' to produce 1 physical unit of the new commodity in each of the new industries.
#' @param isDomestic boolean that takes TRUE if the modification is for domestic use table.
#' @return the model with Make and Use tables updated.
modifyMakeandUseTables <- function(modModel,inputPurchases, valueAdded, isDomestic){
  #browser()

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # MODIFY USE TABLE
  
  logging::loginfo(paste("Updating Use Table ..."))
  resultModifyUse<-modifyUseTable(modModel,inputPurchases,valueAdded, isDomestic)
  modUse<-resultModifyUse[[1]]
  modModel<-resultModifyUse[[2]]
  totalF_UseSimComm<-resultModifyUse[[3]]
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # MODIFY MAKE TABLE

  logging::loginfo(paste("Updating Make Table ..."))
  resultModifyMake<-modifyMakeTable(modModel,totalF_UseSimComm)
  newMake<-resultModifyMake[[1]] 
  modModel<-resultModifyMake[[2]]
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #See make and use tables after augmenting, but before balancing
  #browser()
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #REBALANCE MAKE AND USE TABLES AFTER AUGMENTATION
  rebalancingResults<-rebalanceMakeUseAnalytical(modModel, newMake, modUse)
  balancedMake<-rebalancingResults[[1]]
  balancedUse<-rebalancingResults[[2]]
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #---UPDATE MODEL MAKE AND USE TABLE WITH BALANCED MAKE AND USE
  
  if(isDomestic==FALSE){ #Configured thinking that FALSE will be run before TRUE
    modModel$BiofuelsData$NewBEA<-list(Make=balancedMake, Use=balancedUse)
  }
  else{
    modModel$BiofuelsData$NewBEA$DomesticMake=balancedMake
    modModel$BiofuelsData$NewBEA$DomesticUse=balancedUse
  }
  
  modModel
}

#' This function modifies the B table created in buildEEIOModel() for adding the new sector.
#' @param envData (# environmental flows x #new Tech) matrix with the data for all the environmental flows per dollar of output for the each new industry.
#' @param B unmodified B matrix. (# environmental flows x(405 + #newTech)) with the columns for the new technologies in zeros.
#' @param model the list of USEEIO model components and attributes under modification.
#' @return B matrix modified.
modifyBmatrix <- function(envData, B, model){
  
  whichI<-model$BiofuelsData$whichNewIn
  modB<-B
  
  # Since at the point this function is called, model$Industries is already updated, the B matrix already has the proper dimension, i.e, 
  # has already added a new column with zeros for this new sector. Therefore, now it is only necessary to fill it and not to add the column.
  
  #Determine number of industries in original B
  n<- model$BiofuelsData$OriginalIndustriesNum
  
  #CAREFUL!! DATA ROWS IMPORTED MUST BE IN EXACTLY THE SAME ORDER AS IN B MATRIXDEV
  newrow<-as.matrix(envData[,whichI])
  
  #-----Add value added for new industries----
  
  newIndustriesVA<-model$UseValueAdded[,-(1:n)]
  newIndustriesTotalProduction<-model$IndustryOutput[-(1:n)]
  
  newIndustriesVA_perUSD<-as.matrix(newIndustriesVA/newIndustriesTotalProduction)
  
  compensationEmployeesRowIndex<-which(rownames(B)=="Compensation of employees//USD")
  TaxesRowIndex<-which(rownames(B)=="Taxes on production and imports, less subsidies//USD")
  GrossOpSurplusRowIndex<-which(rownames(B)=="Gross operating surplus//USD")  
  
  newrow[compensationEmployeesRowIndex,]<-as.matrix(newIndustriesVA_perUSD[1,])
  newrow[TaxesRowIndex,]<-as.matrix(newIndustriesVA_perUSD[2,])
  newrow[GrossOpSurplusRowIndex,]<-as.matrix(newIndustriesVA_perUSD[3,])
  #----------------------------------------------
  
  #Fill newEnvDataColumn 
  modB[,-(1:n)]<-newrow
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


#' Unit transformation from USD to GGE (Gallon of Gasoline Equivalent).
#' 
#' @param USD amount of dollars to transform.
#' @param price price in dollars per GGE.
#' @return Amount of GGE.
transformUSDtoGGE <- function(USD, price){
  GGE<- USD*1/price
  GGE
}

#' Unit transformation from GGE(Gallon of Gasoline Equivalent) to USD.
#' 
#' @param GGE amount of gallons to transform.
#' @param price price in dollars per GGE.
#' @return Amount of dollars.
transformGGEtoUSD <- function(GGE, price){
  USD<- GGE*price
  USD
}

#' Reconstruct Make table using model$V/model$MakeTransactions, model$Commodities and model$Industries.
#' Based on 2012 BEA tables.
#'
#' @param MakeTransactions dataframe 405 x 405 with transactions between industries and commodities from the Make table. 
#' Excludes Total commodity output and Total industry output.
#' @param Commodities dataframe 405 x 3 with commodities names.
#' @param Industries dataframe 405 x3 with information about industries names.
#' @return A dataframe with the Make table reconstructed.
reconstructMake<-function(MakeTransactions, Commodities, Industries){
  MakeTransactions1<-MakeTransactions
  #Reassign names for it to not have the acronym /US
  colnames(MakeTransactions1)<-Commodities[,1]
  rownames(MakeTransactions1)<-Industries[,1]
  #......................................................................................................
  # Reconstruct original Make (being original the one with which the model was input to this function)
  
  #Add column of Total Industry Output
  MakeTransactions1$T008<-rowSums(MakeTransactions1)
  #Add row with Total Commodity Output
  totalCommodityOutput<-data.frame(t(colSums(MakeTransactions1)), row.names = "T007")
  names(totalCommodityOutput)<-names(MakeTransactions1)
  MakeTransactions1<- rbind(MakeTransactions1,totalCommodityOutput)
  originalMake<- MakeTransactions1
  originalMake
}

#' Reconstruct Use table using model$U/model$UseTransactions or model$DomesticUseTransactions,model$UseValueAdded and
#' model$FinalDemand or model$DomesticFinalDemand. Based on 2012 BEA tables.
#'
#' @param UseTransactions dataframe 405 x 405 with transactions between commodities and industries from the Use table. 
#' Excludes Total commodity output, Total industry output, value added components and final demand.
#' @param FinalDemand dataframe 405 x 20 with final demand for each commodity.
#' @param ValueAdded dataframe 3x 405 with the 3 value added components for each industry.
#' @param Commodities dataframe 405 x 3 with commodities names.
#' @param Industries dataframe 405 x3 with information about industries names.
#' @param FinalDemandMeta dataframe 20 x 4 with Code, Name, Group and Code_Loc for each element of final demand.
#' @param ValueAddedMeta dataframe 3 x 3 with Code, Name, Code_Loc for each element of value added.
#' @return A dataframe with the Use table reconstructed.
reconstructUse<-function(UseTransactions, FinalDemand, ValueAdded, Commodities, Industries, FinalDemandMeta, ValueAddedMeta){
  
  UseTransactions1<-UseTransactions
  finalDemand1<-FinalDemand
  valueAdded1<-ValueAdded
  
  nIndUse<-ncol(UseTransactions1)
  
  #Reassign names for it to not have the acronym /US
  
  colnames(UseTransactions1)<-Industries[,1]
  rownames(UseTransactions1)<-Commodities[,1]
  colnames(finalDemand1)<-FinalDemandMeta[,1]
  rownames(finalDemand1)<-Commodities[,1]
  colnames(valueAdded1)<-Industries[,1]
  rownames(valueAdded1)<-ValueAddedMeta[,1]
  #......................................................................................................
  #Reconstruct original Use
  T005<-data.frame(t(colSums(UseTransactions1)), row.names="T005")
  T006<-data.frame(t(colSums(valueAdded1)), row.names = "T006")
  T008<-data.frame(T005+T006, row.names = "T008")
  names(T005)<-names(UseTransactions1)
  names(T006)<-names(UseTransactions1)
  names(T008)<-names(UseTransactions1)
  
  UseTransactions1$T001<-rowSums(UseTransactions1)
  finalDemand1$T004<-rowSums(finalDemand1)
  T007<-UseTransactions1$T001+finalDemand1$T004
  UseTransactions1<-cbind(UseTransactions1,finalDemand1,T007)
  
  piece2<-rbind(T005,valueAdded1, T006,T008)
  blockZeros<-matrix(rep(0,nrow(piece2)*(ncol(UseTransactions1)-nIndUse)), nrow = nrow(piece2), ncol = ncol(UseTransactions1)-nIndUse)
  piece2<-cbind(piece2,blockZeros)
  names(piece2)<-names(UseTransactions1)
  
  UseTransactions1<-rbind(UseTransactions1,piece2)
  originalUse<-UseTransactions1
  originalUse
}

modifyMultiYearIndustryOutput<-function(modModel){
  
  #Assumption: The new industries output for all industries will be the same as the IO year 
  ori_MultiYearIndustryOutput<-modModel$MultiYearIndustryOutput

  # Add the number of rows corresponding to the number of new industries
  nNewI<-modModel$BiofuelsData$nNewIndustries
  
  #....Create the rows
  nYears<-ncol(ori_MultiYearIndustryOutput)
  nOriginalIndustries<- modModel$BiofuelsData$OriginalIndustriesNum
  newIndustryOutput<-modModel$IndustryOutput[(nOriginalIndustries+1):length(modModel$IndustryOutput)]
  newRows<-matrix(rep(newIndustryOutput,nYears), nrow = nNewI, ncol = nYears)
  
  #....Add the new rows
  colnames(newRows)<-colnames(ori_MultiYearIndustryOutput)
  mod_MultiYearIndustryOutput<-rbind(ori_MultiYearIndustryOutput,newRows)
  rownames(mod_MultiYearIndustryOutput)<-modModel$Industries$Code_Loc
  mod_MultiYearIndustryOutput[, as.character(modModel$specs$IOYear)] <- modModel$IndustryOutput #re-update it since it could have change with the rebalancing
  
  mod_MultiYearIndustryOutput
}

modifyMultiYearCommodityOutput<-function(model){
  
  #This is literally the same function used in loadNationalIOData() in LoadIOTables.R
  
  # Transform multi-year industry output to commodity output
  model$MultiYearCommodityOutput <- as.data.frame(model$CommodityOutput)[, FALSE]
  for (year_col in colnames(model$MultiYearIndustryOutput)) {
    model$MultiYearCommodityOutput[, year_col] <- transformIndustryOutputtoCommodityOutputforYear(as.numeric(year_col), model)
  }
  model$MultiYearCommodityOutput[, as.character(model$specs$IOYear)] <- model$CommodityOutput
  
  model$MultiYearCommodityOutput
}



#' Do initial calculations required for augmentation of Make and Use tables. Include, reconstructing Make and Use tables (Usual and Domestic) from 
#' model$V/model$MakeTransactions, model$U/model$UseTransactions , model$DomesticUseTransactions,model$UseValueAdded,
#' model$FinalDemand,model$DomesticFinalDemand and gather other information necessary.Creates the list model$BiofuelsData that saves all information
#' about the new sector.
#'
#' @param model the list of USEEIO model components and attributes under modification.
#' @param newIndustryInfo dataframe with # different technologies on the rows and the following variables on the columns (Code,Name, PrimaryProduct, Price, Percentage_Prod)
#' @param newCommodityInfo dataframe with the information of the new commodity (1 row). Column variables (Code, Name, PercentageSubstitution)
#' @param simCommodityInfo list with the information of the similar commodity. Elements (Code, Name, PrimaryProducerCode, PrimaryProducerName, Price) 
#' @param percentage double (0,1] that refers to the \% of the similar commodity that the new commodity will replace.
#' @return The model with basic information about new sector added and initial calculations.

initialCalculations<- function(model,newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage){
  
  #' In here, I am going to augment the Make and Use tables after loadNationalIOData(). Therefore, I have no access
  #' to BEA$Make or BEA$Use nor BEA at all, but to model$V/model$MakeTransactions, model$U/model$UseTransactions , model$DomesticUseTransactions,model$UseValueAdded,
  #' model$FinalDemand,model$DomesticFinalDemand . The reason of this change is that at this point, domestic matrices have been
  #' calculated and therefore I can augment them without involving in how they were constructed

  
  #Reconstruct original Make and use tables
  
  #browser()
  #......................................................................................................
  # Reconstruct original Make (being original the one with which the model was input to this function)
  originalMake<-reconstructMake(model$MakeTransactions,model$Commodities, model$Industries)

  #......................................................................................................
  #Reconstruct original Use
  
  originalUse<-reconstructUse(model$UseTransactions,model$FinalDemand, model$UseValueAdded, model$Commodities, model$Industries, model$FinalDemandMeta,model$ValueAddedMeta)
  
  #Reconstruct original DomesticUse
  originalDomesticUse<-reconstructUse(model$DomesticUseTransactions,model$DomesticFinalDemand, model$UseValueAdded, model$Commodities, model$Industries, model$FinalDemandMeta,model$ValueAddedMeta)
  
  #......................................................................................................
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
  
  # # How much does the 324110 industry produce of 324110 commodity in $ USD? 
  # CXP<- originalMake[rowS_Make,cols_Make]
  # # How much does the 324110 industry produce of 324110 commodity in GGE? 
  # CGP<- transformUSDtoGGE(CXP, simCommodityInfo$Price)
  # #% of total 324110 commodity produced by primary producer 324110?
  # primaryFuelPercentage<- CXP/originalMake[nRowsMake,cols_Make]
  
  # FUTURE
  # Here this values will be initialized in 0, this will be filled up while modifying the make and use tables
  
  # How much will the bio-product replace in GGE? 
  FBP<-0
  # How much will the 324110 industry produce of 324110 commodity in GGE?
  FGP_petr<- 0
  # How much will the 324110 industry produce of 324110 commodity in $ USD? 
  # FX<- transformGGEtoUSD(FGP_petr,simCommodityInfo$Price) 
  # How much will the Gas fermentation industry produce of bio-product in GGE? 
  FGP_tech1<-FBP*newIndustryInfo[1,5]
  # How much will the Thermochemical industry produce of bio-product in GGE? 
  FGP_tech2<-FBP*newIndustryInfo[2,5]
  # How much will the Biochemical industry produce of bio-product in GGE? 
  FGP_tech3<-FBP*newIndustryInfo[3,5]
  
  #---------------------------------------------------------------------------------------------------------------------------------
  # HOW MANY  AND WHICH INDUSTRIES WILL BE ADDED?
  nNewI<-0
  whichI<-NULL
  
  for(i in 1:3){
    if(newIndustryInfo[i,5]>0){
      nNewI<-nNewI+1
      whichI<-c(whichI,i)
    }
  }
  #browser()
  originalBEA_data<-list(Make=originalMake, Use=originalUse, DomesticUse=originalDomesticUse, nCommodities=nCommodities, nIndustries=nIndustries)
  # Save data in model
  
  modModel$BiofuelsData<- list(FutureBiofuelGGE=FBP,FutureFuelGGE=FGP_petr,FutureBiofuelTech1= FGP_tech1,FutureBiofuelTech2=FGP_tech2, FutureBiofuelTech3=FGP_tech3,NewIndustriesInfo= newIndustryInfo,NewCommodityInfo= newCommodityInfo, SimilarCommodityInfo=simCommodityInfo, BiofuelsPercentage=percentage, OriginalCommoditiesNum=nCommodities, OriginalIndustriesNum=nIndustries, nNewIndustries=nNewI, whichNewIn=whichI, OriginalBEA=originalBEA_data)
  #print(paste("The data you need to gather for input purchases and value added must correspond to the following GGE gallons produced for each new industry:", "Technology 1 (GGE)=",FGP_tech1,"Technology 2 (GGE)=", FGP_tech2, "Technology 3 (GGE)=", FGP_tech3 ))
  modModel
}



#' Adds a new sector to an existing model by augmenting the Make and Use tables and updating all necessary components in the mdoel.
#' This includes 3 new industries and 1 new commodity.
#' 
#' This function adds 3 new industries (that correspond to 3 different technologies) to produce 1 new commodity.
#' This new commodity will be a perfect substitute of a commodity in baseline economy, referred as similar commodity.
#' This function calls modifyMakeandUseTables() and update other elements in the model for consistency.
#' 
#' @param model the list of USEEIO model components and attributes for modification.
#' @param inputPurchases (nComm+1)x #newTech matrix with the amount spent in each of the nComm existing commodities to produce 1 physical unit of the new commodity in each of the new industries.
#' @param valueAdded 3x #newTech matrix with the 3 components of value added (Compensation of employees; Taxes on production and imports, less subsidies; Gross operating surplus) required 
#' to produce 1 physical unit of the new commodity in each of the new industries.
#' @param newEnvData (# environmental flows x #newTech) matrix with the data for all the environmental flows per dollar of output for the new industries.
#' @export
#' @return A list with USEEIO model components and attributes modified and ready for calculation.
augmentTablesWithNewSector<- function(model,inputPurchases, valueAdded, newEnvData){
  #browser()
  # Perfom initial calculations
  newIndustryInfo<-data.frame(Code=rep("",3),Name=rep("",3),PrimaryProduct=rep("",3),Price=rep(0,3),Percentage_Prod=rep(0,3))
  
  i<-1
  for(tech in model$newTechSpecs$NewTechnologies$NewIndustries){
    newIndustryInfo[i,]<-tech
    i<-i+1
  }
  
  newCommodityInfo<-data.frame(model$newTechSpecs$NewTechnologies$NewCommodity)
  simCommodityInfo<-model$newTechSpecs$NewTechnologies$SimilarCommodity
  percentage<-model$newTechSpecs$NewTechnologies$NewCommodity$PercentageSubstitution
  modModel<-initialCalculations(model,newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage)
  
  #browser()
  
  # Modify Make and Use Tables
  logging::loginfo(paste("Updating Make and Use Tables ..."))
  modModel<-modifyMakeandUseTables(modModel,inputPurchases, valueAdded, isDomestic = FALSE)
  
  #browser()
  # Modify Make and Domestic Use Tables
  
  #Note that by running this, I rewrite BiofuelsData$
  logging::loginfo(paste("Updating Make and Domestic Use Tables ..."))
  modModel<-modifyMakeandUseTables(modModel,inputPurchases, valueAdded, isDomestic = TRUE)
  
  #Indeed I verified and the Make is different from Domestic Make. The biggest difference is Total Industry Output "T008"
  #browser()

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # OTHER MODEL VARIABLES UPDATES
  whichI<-modModel$BiofuelsData$whichNewIn
  #nNewI<-modModel$BiofuelsData$nNewIndustries
  
  logging::loginfo(paste("Updating BEA information..."))
  
  #Update model Industries, Commodities
  newCommodityCode_Loc<-paste0(modModel$BiofuelsData$NewCommodityInfo$Code,"/",model$specs$ModelRegionAcronyms)
  newCommodityRow<-c(modModel$BiofuelsData$NewCommodityInfo$Code,modModel$BiofuelsData$NewCommodityInfo$Name,newCommodityCode_Loc)
  modModel$Commodities= rbind(modModel$Commodities,newCommodityRow)
  
  newIndustryMatrix<-newIndustryInfo[,1:2]
  newIndustryMatrix$Code_Loc <- apply(cbind(newIndustryMatrix[,1], model$specs$ModelRegionAcronyms), 1, FUN = joinStringswithSlashes)
  modModel$Industries= rbind(modModel$Industries,newIndustryMatrix[whichI,])
  
  #.....................................................................................................................
  # Update model MakeTransactions, UseTransactions, UseFinalDemand, UseValueAdded, UseCommodityOutput, MakeIndustryOutput
  newMake<-modModel$BiofuelsData$NewBEA$Make
  newUse<-modModel$BiofuelsData$NewBEA$Use
  newDomesticUse<-modModel$BiofuelsData$NewBEA$DomesticUse
  
  modModel$MakeTransactions <- newMake[modModel$Industries[,1], modModel$Commodities[,1]]  # data frame, values are in dollars ($)
  modModel$MakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($)
  modModel$UseTransactions <- newUse[modModel$Commodities[,1], modModel$Industries[,1]]  # data frame, values are in dollars ($)
  modModel$FinalDemand <- newUse[modModel$Commodities[,1], modModel$FinalDemandMeta$Code]  # data frame, values are in dollars ($)
  modModel$UseValueAdded <- newUse[modModel$ValueAddedMeta$Code, modModel$Industries[,1]]  # data frame, values are in dollars ($)
  modModel$UseCommodityOutput <- as.data.frame(rowSums(cbind(modModel$UseTransactions, modModel$FinalDemand))) # data frame, values are in dollars ($)
  
  #Update domestic matrices
  modModel$DomesticUseTransactions <- newDomesticUse[modModel$Commodities[,1], modModel$Industries[,1]]
  modModel$DomesticFinalDemand <- newDomesticUse[modModel$Commodities[,1], modModel$FinalDemandMeta$Code]
  
  #browser()
  # Change again the names of the Make and Use to have the acronyms
  #.....................................................................................................................
  ## Modify row and column names in the IO tables
  # Use model$Industries
  rownames(modModel$MakeTransactions) <- colnames(modModel$UseTransactions)  <-colnames(modModel$UseValueAdded) <- modModel$Industries$Code_Loc
  colnames(modModel$DomesticUseTransactions)<- modModel$Industries$Code_Loc
  # Use model$Commodities
  colnames(modModel$MakeTransactions) <- rownames(modModel$UseTransactions) <- rownames(modModel$FinalDemand)<- modModel$Commodities$Code_Loc
    
  rownames(modModel$DomesticUseTransactions) <- rownames(modModel$DomesticFinalDemand)<- modModel$Commodities$Code_Loc
     
  # Apply joinStringswithSlashes based on original row/column names
  rownames(modModel$UseValueAdded) <- apply(cbind(rownames(modModel$UseValueAdded), modModel$specs$ModelRegionAcronyms),
                                         1, FUN = joinStringswithSlashes)
  

  colnames(modModel$FinalDemand) <- colnames(modModel$DomesticFinalDemand) <- apply(cbind(colnames(modModel$FinalDemand),
                                                                                    modModel$specs$ModelRegionAcronyms),
                                                                              1, FUN = joinStringswithSlashes)
  
  #.....................................................................................................................
  #Update IndustryOutput and CommodityOutput from new Use table
  modModel$IndustryOutput <- colSums(modModel$UseTransactions) + colSums(modModel$UseValueAdded)
  modModel$CommodityOutput <- rowSums(modModel$UseTransactions) + rowSums(modModel$FinalDemand)
  
  #.....................................................................................................................
  # Modify MultiYearIndustry Output and MultiYearCommodityOutput
  
  # modModel$MultiYearIndustryOutput<- modifyMultiYearIndustryOutput(modModel)
  # modModel$MultiYearCommodityOutput<- modifyMultiYearCommodityOutput(modModel)

  # #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # 
  # # Update B and W matrices
  # 
  # #Modify B matrix
  # logging::loginfo(paste("Updating B matrix ..."))
  # newB<- modifyBmatrix(newEnvData, modModel)
  # modModel$B<-newB
  # 
  # # Update W matrix for Bioeconomy new sectors 
  # updatedUseValueAdded<- modModel$Use[modModel$BEA$ValueAddedCodes, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  # modModel$W <- as.matrix(updatedUseValueAdded)
  # #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
 
  
  return(modModel)
  
}

#' Add new sector by augmenting Make and Use tables.
#' 
#' This function reads data from the specs files, organize it in the proper format and call augmentTablesWithNewSector() function.
#' 
#' @param model the model under modification.
#' @return the model with Make and Use tables updated.
#' 
#' Example: addNewSector(model)
addNewSector<-function(model){
  
  #Obtain specs file
  newTechConfigFile <- model$specs$NewTechSpecs
  logging::loginfo(paste("Reading new technology for", newTechConfigFile, sep=" "))
  model$newTechSpecs <- getConfiguration(newTechConfigFile, "newTech") 
  
  #Extract data from specs
  inputP_data<-read.csv(model$newTechSpecs$NewTechnologies$InputPurchases)
  inputPurchasesNewTech<-as.matrix((inputP_data[,(3:5)]))
  
  #This is value added in $/GGE (Includes compensation to employees, taxes and gross operating surplus)
  valueAdded_data<-read.csv(model$newTechSpecs$NewTechnologies$ValueAdded, row.names = 1, header = TRUE)
  valueAddedNewTech<-as.matrix(valueAdded_data)
  
  #Read environmental data
  envData_read<-read.csv(model$newTechSpecs$NewTechnologies$EnvironmentalFlows)
  envData<- as.matrix(envData_read[,-1]) 
  
  model<-augmentTablesWithNewSector(model,inputPurchasesNewTech, valueAddedNewTech, envData)
  model
  #browser()
}

