# This script contains the modifications of the Make and Use tables, teh A and B matrices and the Y vector
# to recalculate the USEEIO model with an additional Bio-economy sector

#' This function modifies the pre-saved Make table for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric that refers to the % of output of the original sector that new sector will produce.
#' @param originalMake dataframe with the Make table obtained in buildEEIOModel().
#' @return a dataframe with the originalMake table modified.
modifyMakeTable <- function(newSectorCode, similarSectorCode, percentage, originalMake){
  
  originalMake
}

#' This function modifies the pre-saved Use table for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric that refers to the % of output of the original sector that new sector will produce.
#' @param inputPurchases (n+1)x 1 column vector with the amount spent in each of the n existing commodities to produce "Total Industry Output"
#' of the new sector.
#' @param originalUse dataframe with the Make table obtained in buildEEIOModel().
#' @return a dataframe with the originalUse table modified.
modifyUseTable <- function(newSectorCode, similarSectorCode, percentage, inputPurchases, originalUse){
  
  originalUse
}

#' This function modifies the B table created in buildEEIOModel() for adding one new bioeconomy sector
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices.
#' @param newEnvData (# environmental flows x 1) column vector with the data for all the environmental flows per dollar of output for the new sector.
#' @param originalB B matrix obtained in buildEEIOModel().
#' @return B matrix modified.
modifyBmatrix <- function(newsectorCode, newEnvData, originalB){
  originalB
}


#This function updates/modifies the components in model that need to be modified based on user administered data
#' @param model refers to the model constructed via buildEEIOModel().
#' @param newSectorCode string/character that refers to the code/identifier that will be used for the new sector in the matrices.
#' @param newSectorName string/character that refers to the name given to the new sector.
#' @param similarSectorCode string/character that refers to the code/identifier for the similar existing sector.
#' @param percentage numeric that refers to the % of output of the original sector that new sector will produce.
#' @param inputPurchases (n+1)x 1 column vector with the amount spent in each of the n existing commodities to produce "Total Industry Output"
#' of the new sector.
#' @param newEnvData (# environmental flows x 1) column vector with the data for all the environmental flows per dollar of output for the new sector.
#' When the function runs succesfully, it prints a message that says, "model correctly modyfied. Bioeconomy model correctly created".
createBioeconomyModel<- function(model,newSectorCode,newSectorName, similarSectorCode,percentage, inputPurchases, newEnvData) {
  
  #Obtain original Make and use tables
  originalMake<- model$Make
  originalUse<- model$Use
  
  #Obtain original B matrix
  originalB<- model$B
  
  #Modify Make Table
  newMake<- modifyMakeTable(newSectorCode, similarSectorCode, percentage, originalMake)
  model$Make <- newMake
  
  #Modify Use Table
  newUse<- modifyUseTable(newSectorCode, similarSectorCode, percentage, inputPurchases, originalUse)
  model$Use<- newUse
  
  #Update model Industries, Commodities
  model$Commodities= append(model$Commodities,newSectorCode, after= length(model$Commodities))
  model$Industries= append(model$Industries,newSectorCode, after= length(model$Industries))
  
  # Update model MakeTransactions, UseTransactions, FinalDemand and UseCommodityOutput for normalization
  model$MakeTransactions <- model$Make[model$Industries, model$Commodities] * 1E6 # data frame, values are in dollars ($)
  model$UseTransactions <- model$Use[model$Commodities, model$Industries] * 1E6 # data frame, values are in dollars ($)
  
  updatedMakeIndustryOutput <- as.data.frame(rowSums(model$MakeTransactions)) # data frame, values are in dollars ($)
  model$FinalDemand <- model$Use[model$Commodities, BEA$FinalDemandCodes] * 1E6 # data frame, values are in dollars ($)
  
  updatedUseCommodityOutput <- as.data.frame(rowSums(cbind(model$UseTransactions, model$FinalDemand))) # data frame, values are in dollars ($)
  #update model$BEA$UseCommodityOutput because is the one used in generateMarketSharesfromMake()
  model$BEA$UseCommodityOutput <- updatedUseCommodityOutput
  
  updatedMakeIndustryOutput <- as.data.frame(rowSums(model$MakeTransactions)) # data frame, values are in dollars ($)
  #update model$BEA$MakeIndustryOutput because is the one used in generateDirectRequirementsfromUse()
  model$BEA$MakeIndustryOutput<-updatedMakeIndustryOutput
  
  
  #Re-generate matrices: Not updating for Domestic
  
  model$V_n <- generateMarketSharesfromMake(model) # normalized Make
  model$U_n <- generateDirectRequirementsfromUse(model, domestic = FALSE) #normalized Use , Warning: Not updated for domestic!
  
  updatedUseValueAdded<- model$Use[BEA$ValueAddedCodes, model$Industries] * 1E6 # data frame, values are in dollars ($)
  model$W <- as.matrix(updatedUseValueAdded)
  
  # Assuming CommoditybyIndustryType == "Commodity"
  logging::loginfo(paste("Updating commodityxcommodity direct requirement matrix ..."))
  model$A <- model$U_n %*% model$V_n
  
  #Modify B matrix
  newB<- modifyBmatrix(newSectorCode,newEnvData, originalB)
  model$B<-newB
  
  # Transform B into a flowxcommodity matrix using market shares matrix for commodity models
  model$B <- model$B %*% model$V_n
  colnames(model$B) <- tolower(paste(colnames(model$B), model$specs$PrimaryRegionAcronym, sep = "/"))
  
  #Re-calculate Total Requirements Matrix L=(I-A)^(-1)
  logging::loginfo("Re-calculating total requirements matrix...")
  I <- diag(nrow(model$A))
  model$L <- solve(I - model$A)
  
  logging::loginfo("model correctly modyfied. Bioeconomy model correctly created.")
}