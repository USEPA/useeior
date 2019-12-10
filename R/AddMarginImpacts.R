#' Prepare M and U matrices with sector margin impacts
#' @param model A complete EEIO model
#' @param margin_type A character value: can be "industry" or "final consumer".
#' @export
#' @return A list with M_margin and U_margin
deriveMarginSectorImpacts <- function(model,margin_type="industry") {

  if (margin_type=="industry") {
    Margins <- model$IndustryMargins
  } else {
    Margins <- model$FinalConsumerMargins
  }
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
  
  margin_allocation <- matrix(nrow=3,ncol=length(all_margin_sectors),0)
  row.names(margin_allocation) <- colnames(MarginCoefficients)
  colnames(margin_allocation) <- all_margin_sectors
  
  
  #Just for testing - assign 1 to first margin sectors by type
  margin_allocation["Transportation","423100"] <- 1
  margin_allocation["Wholesale","423100"] <- 1
  margin_allocation["Retail","441000"] <- 1
  
  #multiply fractions by allocation matrix to get a fraction per margin sector for each commodity
  margins_by_sector <- MarginCoefficients %*% margin_allocation
  
  #Need to drop extra sectors from margins_by_sector and order to be same a A
  margins_by_sector <- margins_by_sector[-which(rownames(margins_by_sector) %in% model$BEA$ValueAddedCodes),]
  #Put margins_by_sector into a matrix in the form of A
  A_margin <- model$A #matrix(nrow=nrow(model$A),ncol=ncol(model$A),rep(0))
  A_margin[,] <- 0 
  for (s in all_margin_sectors) {
    A_margin[,s] <- margins_by_sector[,s]
  }
  #Multiply M,U by margins_by_sector
  #Derive an M_margin as an transformation of M_margin
  result <- list()
  result$M_margin = model$M %*% A_margin
  result$U_margin = model$U %*% A_margin
  return(result)
  
}





