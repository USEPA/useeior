#ValidateModel.R


#'Compares the total flows against the model result calculation with the domestic demand vector and direct perspective
#'@param model, EEIOmodel object completely built
compareEandDomesticLCIResult <- function(model) {
  E <- prepareEfromtbs(model)
  result_domestic <- calculateEEIOModel(model, "DIRECT", demand = "Production", use_domestic = TRUE)
}


#'Concatenate all satellite flows in model
#'@param model, EEIOmodel object completely built
prepareEfromtbs <- function(model) {
  df <- do.call(rbind,model$SatelliteTables$totals_by_sector)
  E <- as.matrix(E)
  return(E)
}



      