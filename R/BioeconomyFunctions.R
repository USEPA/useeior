# This script contains the modifications of the Make and Use tables, teh A and B matrices and the Y vector
# to recalculate the USEEIO model with an additional Bio-economy sector

#' This function modifies the pre-saved Make table for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric that refers to the % of output of the original sector that new sector will produce.
#' @param originalMake dataframe with the Make table obtained in buildEEIOModel().
#' @return a dataframe with the originalMake table modified.
#' 
#' Example: modifyMakeTable("prueba","324110",0.2,model$Make)
modifyMakeTable <- function(newSectorCode, similarSectorCode, percentage, originalMake){
  
  modMake<-originalMake
  #Determine number of sectors in originalMake
  n<- ncol(modMake)-1
  
  #Add column of zeros and assign col name(the code)
  colZeros<- rep(0, times=n+1)
  modMake<-cbind(modMake[,1:n],colZeros,modMake[,-(1:n)])
  colnames(modMake)[n+1]<- newSectorCode
  #Not clear why it changes the name, but I put it again
  colnames(modMake)[n+2]<-"T008"

  #Add row of zeros and assign row name(the code)
  rowZeros<- rep(0, times=n+2)
  modMake<-rbind(modMake[1:n,],rowZeros,modMake[-(1:n),])
  rownames(modMake)[n+1]<- newSectorCode
  
  #Get row and column of similar sector
  rowS<-which(rownames(modMake)==similarSectorCode)
  colS<-which(colnames(modMake)==similarSectorCode)
  
  #Fill row
  
  valToDist<-modMake[rowS,colS]
  modMake[n+1,n+1]<- valToDist*percentage
  modMake[rowS,colS]<- valToDist*(1-percentage)

  # Recalculate totals
  modMake[n+2,]<-colSums(modMake[1:(n+1),]) #sum over rows for each column
  modMake[,n+2]<-rowSums(modMake[,1:(n+1)]) #sum over columns for each row
  
  modMake
}

#' This function modifies the pre-saved Use table for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric that refers to the % of output of the original sector that new sector will produce.
#' @param inputPurchases (n+1)x 1 column vector with the amount spent in each of the n existing commodities to produce "Total Industry Output"
#' of the new sector.
#' @param originalUse dataframe with the Make table obtained in buildEEIOModel().
#' @return a dataframe with the originalUse table modified.
#' 
#' Example: modifyUseTable("prueba","324110",0.2,rep(1,times=406),model$Use)
modifyUseTable <- function(newSectorCode, similarSectorCode, percentage, inputPurchases, originalUse, newTotalIndustryOutput){
  
  modUse<-originalUse
  #Determine number of sectors in originalUse
  n<- nrow(modUse)-6
  
  #Add column of zeros and assign col name(the code)
  colZeros<- rep(0, times=n+6)
  modUse<-cbind(modUse[,1:n],colZeros,modUse[,-(1:n)])
  colnames(modUse)[n+1]<- newSectorCode
  
  #Add row of zeros and assign row name(the code)
  rowZeros<- rep(0, times=(n+1)+23)
  modUse<-rbind(modUse[1:n,],rowZeros,modUse[-(1:n),])
  rownames(modUse)[n+1]<- newSectorCode
  
  #Get row and column of similar sector
  rowS<-which(rownames(modUse)==similarSectorCode)
  colS<-which(colnames(modUse)==similarSectorCode)
  
  #Fill row n+1- cycle over columns
  for(j in 1:n){
    useSim<- modUse[rowS,j]
    modUse[n+1,j]<-useSim*percentage
    modUse[rowS,j]<-useSim*(1-percentage)
  }
  
  # Change final users demand
  # For now, assuming the same % for all the 20 categories that compose final users demand
  
  initialCol<-(n+1)+2
  for(j in initialCol:(initialCol+19)){
    demSim<-modUse[rowS,j]
    modUse[(n+1),j]<-demSim*percentage
    modUse[rowS,j]<-demSim*(1-percentage)
  }
  
  #Fill column n+1- cycle over rows
  
  for(i in 1:(n+1)){
    modUse[i,(n+1)]<-inputPurchases[i]
  }
  
  # Recalculate totals
  
  #Total intermediate inputs
  modUse[n+2,]<-colSums(modUse[1:(n+1),]) #sum over rows for each column
  #Total intermediate use
  modUse[,n+2]<-rowSums(modUse[,1:(n+1)]) #sum over columns for each row
  #Total final uses
  modUse[,(n+1)+22]<-rowSums(modUse[,((n+1)+2):((n+1)+2+19)]) #sum over user demand columns for each row
  #Total commodity output
  modUse[,(n+1+23)]<-modUse[,(n+1+1)]+modUse[,(n+1+22)]
  
  #Fill value added
  #' ASSUMPTION: Since the 3 components of value added are not explicitly used nowhere, just calculate the total VA
  #' for balance purposes and then divide it in 3 for each category
  
  #Calculate total value added
  
  totalIntermediateInputsNewSector<- modUse[n+2,n+1]
  totalNewIndustryOutput<- newTotalIndustryOutput
  totalValueAddedNewSector<- totalNewIndustryOutput-totalIntermediateInputsNewSector
  
  #Update Total Value Added
  modUse[(n+1)+5,n+1]<-totalValueAddedNewSector
  #Assign 1/3 of Total Value Added for each of the 3 VA categories
  modUse[(n+1)+2,n+1]<- totalValueAddedNewSector*1/3
  modUse[(n+1)+3,n+1]<- totalValueAddedNewSector*1/3
  modUse[(n+1)+4,n+1]<- totalValueAddedNewSector*1/3

  #Total industry output
  modUse[(n+1)+6,1:(n+1+1)]<-modUse[(n+1)+1,1:(n+1+1)]+modUse[(n+1)+5,1:(n+1+1)]
  
  modUse
}

#' This function modifies the B table created in buildEEIOModel() for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices.
#' @param newEnvData (# environmental flows x 1) column vector with the data for all the environmental flows per dollar of output for the new sector.
#' @param originalB B matrix obtained in buildEEIOModel().
#' @return B matrix modified.
modifyBmatrix <- function(newsectorCode, newEnvData, originalB, primaryRegionAcronym){
  modB<-originalB
  
  #Determine number of sectors in originalB
  n<- ncol(modB)
  
  #Add newEnvDataColumn and assign col name (the code)
  modB<-cbind(modB,newEnvData)
  colnames(modB)[n+1]<- tolower(paste(newsectorCode, primaryRegionAcronym, sep = "/")) #Unnecesary
 
  modB
}


#This function updates/modifies the components in model that need to be modified based on user administered data
#' @param model refers to the model constructed via buildEEIOModel().
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices.
#' @param newSectorName string/character that refers to the name given to the new sector.
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric (0,1] that refers to the % of output of the original sector that new sector will produce.
#' @param inputPurchases (n+1)x 1 column vector with the amount spent in each of the n existing commodities to produce "Total Industry Output"
#' of the new sector.
#' @param newEnvData (# environmental flows x 1) column vector with the data for all the environmental flows per dollar of output for the new sector.
#' When the function runs succesfully, it prints a message that says, "model correctly modyfied. Bioeconomy model correctly created".
createBioeconomyModel<- function(model,newSectorCode,newSectorName, similarSectorCode,percentage, inputPurchases, newEnvData) {
  modModel<-model

  #Obtain original Make and use tables
  originalMake<- modModel$Make
  originalUse<- modModel$Use
  
  #Obtain original B matrix
  originalB<- modModel$B
  
  #Modify Make Table
  logging::loginfo(paste("Updating Make Table ..."))
  newMake<- modifyMakeTable(newSectorCode, similarSectorCode, percentage, originalMake)
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
  
  updatedMakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($)
  modModel$FinalDemand <- modModel$Use[modModel$Commodities, modModel$BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  
  updatedUseCommodityOutput <- as.data.frame(rowSums(cbind(modModel$UseTransactions, modModel$FinalDemand))) # data frame, values are in dollars ($)
  #update model$BEA$UseCommodityOutput because is the one used in generateMarketSharesfromMake()
  modModel$BEA$UseCommodityOutput <- updatedUseCommodityOutput
  
  updatedMakeIndustryOutput <- as.data.frame(rowSums(modModel$MakeTransactions)) # data frame, values are in dollars ($)
  #update model$BEA$MakeIndustryOutput because is the one used in generateDirectRequirementsfromUse()
  modModel$BEA$MakeIndustryOutput<-updatedMakeIndustryOutput
  
  
  #Re-generate matrices: Not updating for Domestic
  modModel$V_n <- generateMarketSharesfromMake(modModel) # normalized Make
  modModel$U_n <- generateDirectRequirementsfromUse(modModel, domestic = FALSE) #normalized Use , Warning: Not updated for domestic!
  
  updatedUseValueAdded<- modModel$Use[modModel$BEA$ValueAddedCodes, modModel$Industries] * 1E6 # data frame, values are in dollars ($)
  modModel$W <- as.matrix(updatedUseValueAdded)
  
  # Assuming CommoditybyIndustryType == "Commodity"
  logging::loginfo(paste("Updating commodityxcommodity direct requirement matrix ..."))
  modModel$A <- modModel$U_n %*% modModel$V_n
  
  #Modify B matrix
  logging::loginfo(paste("Updating B matrix ..."))
  newB<- modifyBmatrix(newSectorCode,newEnvData, originalB, modModel$specs$PrimaryRegionAcronym)
  modModel$B<-newB
  
  # Transform B into a flowxcommodity matrix using market shares matrix for commodity models
  modModel$B <- modModel$B %*% modModel$V_n
  colnames(modModel$B) <- tolower(paste(colnames(modModel$B), modModel$specs$PrimaryRegionAcronym, sep = "/"))
  
  #Re-calculate Total Requirements Matrix L=(I-A)^(-1)
  logging::loginfo("Re-calculating total requirements matrix...")
  I <- diag(nrow(modModel$A))
  modModel$L <- solve(I - modModel$A)
  
  # Re-calculate total emissions/resource use per dollar (M)
  logging::loginfo("Re-calculating total emissions per dollar matrix...")
  modModel$M <- modModel$B %*% modModel$L
  colnames(modModel$M) <- tolower(paste(colnames(modModel$M), modModel$specs$PrimaryRegionAcronym, sep = "/"))
  
  # Re-calculate total impacts per dollar (U), impact category x sector
  modModel$U <- modModel$C %*% modModel$M
  
  #Update model$SectorNames
  modModel$SectorNames<-rbind(modModel$SectorNames,c(newSectorCode,newSectorName))
  
  logging::loginfo("model correctly modyfied. Bioeconomy model correctly created.")
 
  return(modModel)
}