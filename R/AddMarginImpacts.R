
deriveMarginSectorImpacts < function(model) {
  ##Add in impacts of margin sectors
  
  #Get fractions of producer price for each margin
  MarginCoefficients <- as.matrix(Margins[, c("Transportation", "Wholesale", "Retail")]/Margins[,c("ProducersValue")])
  row.names(MarginCoefficients) <- Margins$CommodityCode
  
  
  #Get Margin Allocation matrix to allocate these by margin sector
  
  #Get allocation factors for margin sectors based on total output
  #not shown here
  
  #These need to be in the model schema 
  model$BEA$TransportationCodes 
  model$BEA$WholesaleCodes 
  model$BEA$RetailCodes 
  
  #Create matrix where rows are three margin types and columns are margin sectors
  all_margin_sectors <- c(model$BEA$TransportationCodes,model$BEA$WholesaleCodes,model$BEA$RetailCodes)
  
  margin_allocation <- matrix(nrow=3,ncol=length(all_margin_sectors))
  row.names(margin_allocation) = colnames(MarginCoefficients)
  colname(margin_allocation)=
  #multiply fractions by allocation matrix to get a fraction per margin sector for each commodity
  
  margins_by_sector <- MarginCoefficients %*% margin_allocation
  
  #multiply D,U by margins_by_sector
  #put margins_by_sector into a matrix in the form of A
  A_margin <- matrix(nrow=nrow(model$A),ncol=ncol(model$A),rep(0))
  
  #Derive an M_margin as an transformation of M_margin
  result <- list()
  result$M_margin = M %*% A_margin
  result$U_margin = U %*% A_margin
  return(result)
  
}





