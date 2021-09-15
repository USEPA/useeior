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

  
  #-------------------------------------------------------------------------------------------------------------------------------
  
  #Fill rows
  
  FGP_cols<-as.matrix(modModel$BiofuelsData[c(4,5,6)])[whichI]
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
  browser()
  originalUse<- modModel$Use
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
  similarPrimaryPercentage<-modModel$BiofuelsData$PercentageFuelProducedByPrimary
  
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
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # MODIFY MAKE TABLE
  
  
  #Get row and column of similar sector for original make table
  rowS_M<-getRowIndex(modModel$Make,similarSectorCode)
  colS_M<-getColIndex(modModel$Make,similarSectorCode)
  
  #-------------------------------------------------------------------------------------------------------------------------------
  
  # FUTURE PRODUCTION OF SIMILAR-COMMODITY BY THE SIMILAR-INDUSTRY IN GGE
  
  # How much is currently produced of 324110 commodity in $ USD by secondary industries?
  
  CX_sec<-sum(modModel$Make[1:(nIndustries),colS_M])-modModel$Make[rowS_M,colS_M]
  
  # How much is currently produced of 324110 commodity in GGE by secondary industries?
  CGP_sec<-transformUSDtoGGE(CX_sec,similarSectorPrice)
  # How much will the 324110 industry produce of 324110 commodity in GGE?
  FGP_petr<- transformUSDtoGGE(totalF_UseSimComm,similarSectorPrice)-CGP_sec
  
  # How much will the 324110 industry produce of 324110 commodity in $ USD? 
  FX<- transformGGEtoUSD(FGP_petr,similarSectorPrice) 
  
  # Save data in model
  
  modModel$BiofuelsData["FutureFuelGGE"]<-FGP_petr
  #-------------------------------------------------------------------------------------------------------------------------------
  
  logging::loginfo(paste("Updating Make Table ..."))
  newMake<- modifyMakeTable(modModel, modUse[1:(nCommodities+1), 1:(nIndustries+nNewI)],modUse[nCommodities+1+6,(nIndustries+1):(nIndustries+nNewI)])
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #See make and use tables after augmenting, but before balancing
  #browser()
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #REBALANCE MAKE AND USE TABLE, VIA ANALYTICAL FRAMEWORK, SCALING COLUMNS TO KEEP RECIPE UNCHANGED
  nMake_Commodities<- ncol(modModel$Make)-1 # Number of commodities in original Make
  nMake_Industries<- nrow(modModel$Make)-1 # Number of commodities in original Use
  
  
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

  #---UPDATE MODEL MAKE AND USE TABLE WITH BALANCED MAKE AND USE
  modModel$Make <- modMake2 
  modModel$Use<- modUse2 
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
  whichI<-model$BiofuelsData$whichNewIn
  
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
  modB[,-(1:n)]<-newEnvData[,whichI]
  
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

transformUSDtoGGE <- function(USD, price){
  GGE<- USD*1/price
  GGE
}

transformGGEtoUSD <- function(GGE, price){
  USD<- GGE*price
  USD
}

addRowsColsZeros<-function(matrix, numRows,numCols){
  #browser()
  origRows<-nrow(matrix)
  origCols<-ncol(matrix)
  
  #Add columns of zeros
  colZeros<- matrix(rep(0, times=(origRows)*numCols),nrow = origRows, ncol=numCols)
  matrixRet<-cbind(matrix,colZeros)
  
  #add rows of zeros
  rowZeros<- matrix(rep(0, times=(origCols+numCols)*numRows), nrow =numRows, ncol = origCols+numCols) 
  colnames(rowZeros)<-colnames(matrixRet)
  matrixRet<-rbind(matrixRet,rowZeros)
  
  matrixRet
}

#' Do initial calculations to determine the amounts of GGE produced in future bio-economy by each of the 3 new industries.
#'
#' @param model the list of USEEIO model components and attributes under modification.
#' @param newIndustryInfo dataframe with 3 different technologies on the rows and the following variables (Code,Name, Primary_product, Price, Percentage_Prod)
#' @param newCommodityInfo dataframe with the information of the new commodity. Column variables (Code, Name, Primary_producers)
#' @param simCommodityInfo list with the information of the similar commodity. Elements (Code, Name, PrimaryProducer_Code, PrimaryProducer_Name, Price) 
#' @param percentage numeric (0,1] that refers to the \% of the original commodity (fuels) that the new commodity (biofuels) will replace.
#' @return The model with basic information added and initial calculations, and print the amount of biofuel in GGE produced by each technology/industry in the future bio-economy.

initialCalculations<- function(model,newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage, isDomestic){
  
  #' In here, I am going to augment the Make and Use tables after loadNationalIOData(). Therefore, I have no access
  #' to BEA$Make or BEA$Use nor BEA at all, but to model$V/model$MakeTransactions, model$U/model$UseTransactions , model$DomesticUseTransactions,model$UseValueAdded,
  #' model$FinalDemand,model$DomesticFinalDemand . The reason of this change is that at this point, domestic matrices have been
  #' calculated and therefore I can augment them without involving in how they were constructed
  
 
  # NEED TO TAKE OUT THE ACRONYM /US FROM THE COLNAMES AND ROWNAMES ------ TODO!!!!
  
  #Reconstruct original Make and use tables
  
  # Obtain Make and Use transactions
  #browser()
  MakeTransactions1<-model$MakeTransactions
  UseTransactions1<-model$UseTransactions
  finalDemand1<-model$FinalDemand
  valueAdded1<-model$UseValueAdded
  

  if(isDomestic ==TRUE){
    UseTransactions1<-model$DomesticUseTransactions
    finalDemand1<-model$DomesticFinalDemand
  }
  
  #Reassign names for it to not have the acronym /US
  colnames(MakeTransactions1)<-model$Commodities[,1]
  rownames(MakeTransactions1)<-model$Industries[,1]
  colnames(UseTransactions1)<-model$Industries[,1]
  rownames(UseTransactions1)<-model$Commodities[,1]
  colnames(finalDemand1)<-model$FinalDemandMeta[,1]
  rownames(finalDemand1)<-model$Commodities[,1]
  colnames(valueAdded1)<-model$Industries[,1]
  rownames(valueAdded1)<-model$ValueAddedMeta[,1]
  #......................................................................................................
  # Reconstruct original Make (being original the one with which the model was input to this function)
  
  #Determine number of commodities and industries from UseTransactions
  nCommUse<-nrow(UseTransactions1)
  nIndUse<-ncol(UseTransactions1)
  
  #Add column of Total Industry Output
  MakeTransactions1$T008<-rowSums(MakeTransactions1)
  #Add row with Total Commodity Output
  totalCommodityOutput<-data.frame(t(colSums(MakeTransactions1)), row.names = "T007")
  names(totalCommodityOutput)<-names(MakeTransactions1)
  MakeTransactions1<- rbind(MakeTransactions1,totalCommodityOutput)
  originalMake<- MakeTransactions1
  model$Make<-originalMake
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
  model$Use<-originalUse
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
  
  # How much does the 324110 industry produce of 324110 commodity in $ USD? 
  CXP<- originalMake[rowS_Make,cols_Make]
  # How much does the 324110 industry produce of 324110 commodity in GGE? 
  CGP<- transformUSDtoGGE(CXP, simCommodityInfo$Price)
  #% of total 324110 commodity produced by primary producer 324110?
  primaryFuelPercentage<- CXP/originalMake[nRowsMake,cols_Make]
  
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
  
 
  # Save data in model
  
  modModel$BiofuelsData<- list(PercentageFuelProducedByPrimary=primaryFuelPercentage,FutureBiofuelGGE=FBP,FutureFuelGGE=FGP_petr,FutureBiofuelTech1= FGP_tech1,FutureBiofuelTech2=FGP_tech2, FutureBiofuelTech3=FGP_tech3,NewIndustriesInfo= newIndustryInfo,NewCommodityInfo= newCommodityInfo, SimilarCommodityInfo=simCommodityInfo, BiofuelsPercentage=percentage, OriginalCommoditiesNum=nCommodities, OriginalIndustriesNum=nIndustries, nNewIndustries=nNewI, whichNewIn=whichI)
  #print(paste("The data you need to gather for input purchases and value added must correspond to the following GGE gallons produced for each new industry:", "Technology 1 (GGE)=",FGP_tech1,"Technology 2 (GGE)=", FGP_tech2, "Technology 3 (GGE)=", FGP_tech3 ))
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
#' @param model the list of USEEIO model components and attributes for modification.
#' @param inputPurchases (nComm+1)x 3 matrix with the amount spent in each of the nComm existing commodities to produce 1 GGE of biofuel in each of the new industries.
#' @param valueAdded 2x3 matrix with the first 2 components of value added (Compensation of employees and Taxes on production and imports, less subsidies) required 
#' to produce 1 GGE of biofuel in each of the new industries.
#' @param newEnvData (# environmental flows x 3) matrix with the data for all the environmental flows per dollar of output for the new industries.
#' @export
#' @return A list with USEEIO model components and attributes modified and ready for calculation.
addBiofuelsSector<- function(model,inputPurchases, valueAdded, newEnvData, isDomestic ){
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
  modModel<-initialCalculations(model,newIndustryInfo, newCommodityInfo,simCommodityInfo, percentage, isDomestic)
  
  # Modify Make and Use Tables
  logging::loginfo(paste("Updating Make and Use Tables ..."))
  modModel<-modifyMakeandUseTables(modModel,inputPurchases, valueAdded)
  
  #browser()

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # OTHER MODEL VARIABLES UPDATES
  whichI<-modModel$BiofuelsData$whichNewIn
  
  logging::loginfo(paste("Updating BEA information..."))
  
  #Update model Industries, Commodities
  newCommodityCode_Loc<-paste0(modModel$BiofuelsData$NewCommodityInfo$Code,"/",model$specs$ModelRegionAcronyms)
  newCommodityRow<-c(modModel$BiofuelsData$NewCommodityInfo$Code,modModel$BiofuelsData$NewCommodityInfo$Name,newCommodityCode_Loc)
  modModel$Commodities= rbind(modModel$Commodities,newCommodityRow)
  
  newIndustryMatrix<-newIndustryInfo[,1:2]
  newIndustryMatrix$Code_Loc <- apply(cbind(newIndustryMatrix[,1], model$specs$ModelRegionAcronyms), 1, FUN = joinStringswithSlashes)
  modModel$Industries= rbind(modModel$Industries,newIndustryMatrix)
  
  #.....................................................................................................................
  # Update model MakeTransactions, UseTransactions, UseFinalDemand, UseValueAdded, UseCommodityOutput, MakeIndustryOutput
  modModel$MakeTransactions <- modModel$Make[modModel$Industries[,1], modModel$Commodities[,1]]  # data frame, values are in dollars ($)
  modModel$MakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($)
  modModel$UseTransactions <- modModel$Use[modModel$Commodities[,1], modModel$Industries[,1]]  # data frame, values are in dollars ($)
  browser()
  modModel$FinalDemand <- modModel$Use[modModel$Commodities[,1], modModel$FinalDemandMeta$Code]  # data frame, values are in dollars ($)
  modModel$UseValueAdded <- modModel$Use[modModel$ValueAddedMeta$Code, modModel$Industries[,1]]  # data frame, values are in dollars ($)
  modModel$UseCommodityOutput <- as.data.frame(rowSums(cbind(modModel$UseTransactions, modModel$FinalDemand))) # data frame, values are in dollars ($)
  
  #!!!TO DO: the updates if all of these is being used for the domestic matrices!!!!!!
  
  # Change again the names of the Make and Use to have the acronyms
  #.....................................................................................................................
  ## Modify row and column names in the IO tables
  # Use model$Industries
  rownames(modModel$MakeTransactions) <- colnames(modModel$UseTransactions)  <-colnames(modModel$UseValueAdded) <- modModel$Industries$Code_Loc
  #colnames(modModel$DomesticUseTransactions)<- modModel$Industries$Code_Loc
  # Use model$Commodities
  colnames(modModel$MakeTransactions) <- rownames(modModel$UseTransactions) <- rownames(modModel$FinalDemand)<- modModel$Commodities$Code_Loc
    
  #rownames(modModel$DomesticUseTransactions) <- rownames(modModel$DomesticFinalDemand)<- modModel$Commodities$Code_Loc
     
  # Apply joinStringswithSlashes based on original row/column names
  rownames(modModel$UseValueAdded) <- apply(cbind(rownames(modModel$UseValueAdded), modModel$specs$modModelRegionAcronyms),
                                         1, FUN = joinStringswithSlashes)
  colnames(modModel$FinalDemand)<- apply(cbind(colnames(modModel$FinalDemand),
                                                                                    modModel$specs$modModelRegionAcronyms),
                                                                              1, FUN = joinStringswithSlashes)
  #colnames(modModel$DomesticFinalDemand)<- apply(cbind(colnames(modModel$FinalDemand),
  #                                                    modModel$specs$modModelRegionAcronyms),
  #                                              1, FUN = joinStringswithSlashes)
  #.....................................................................................................................
  #Update IndustryOutput and CommodityOutput
  modModel$IndustryOutput <- colSums(modModel$UseTransactions) + colSums(modModel$UseValueAdded)
  modModel$CommodityOutput <- rowSums(modModel$UseTransactions) + rowSums(modModel$FinalDemand)
  
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


