#ValidateModel.R


#'Compares the total flows against the model result calculation with the domestic demand vector and direct perspective
#'@param model, EEIOmodel object completely built
compareEandDomesticLCIResult <- function(model, tolerance=0.05) {
  E <- prepareEfromtbs(model)
  result_domestic <- calculateEEIOModel(model, "DIRECT", demand = "Production", use_domestic = TRUE)
  LCI <- data.frame(t(result_domestic$LCI_d))
  #clean up
  rm(result_domestic)
  
  #temp harmonize E and LCI
  rE <- rownames(E)
  rL <- rownames(LCI)
  #rws <- setdiff(,)
  Ec <- E[(rE %in% rL),]
  LCIc <- LCI[(rL %in% rE),]      
  Ec <- Ec[rownames(LCIc),]
  E <- Ec
  LCI <- LCIc
  
  library(validate)
  rule <- validate::validator(abs(LCI - E)/E <= tolerance)
  confrontation <- validate::confront(LCI, rule, E)
  summary(confrontation)
  confrontation <- validate::as.data.frame(confrontation)
  validation <- merge(confrontation, validate::as.data.frame(rule))
}


#'Concatenate all satellite flows in model
#'@param model, EEIOmodel object completely built
prepareEfromtbs <- function(model) {
  df <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  E <- standardizeandcastSatelliteTable(df,model)
  return(E)
}




