#CompareModels.R

#' Compare LCI in the form of totals by sector for two models
#' @param NameModelA Model file to compare
#' @param NameModelB Model file to compare to
compareLCI <- function(NameModelA, NameModelB) {
  modelA <- useeior::loadIOData(NameModelA)
  modelA <- useeior::loadandbuildSatelliteTables(modelA)
  LCI_A <- data.frame(do.call("rbind", modelA$SatelliteTables$totals_by_sector))
  
  modelB <- useeior::loadIOData(NameModelB)
  modelB <- useeior::loadandbuildSatelliteTables(modelB)
  LCI_B <- data.frame(do.call("rbind", modelB$SatelliteTables$totals_by_sector))

  library(validate)
  rule <- validate::validator(abs(LCI_A - LCI_B)/LCI_B <= 0.05)
  confrontation <- validate::confront(LCI_A, rule, LCI_B)
  test <- summary(confrontation)
  print(test)
  df_confrontation <- validate::as.data.frame(confrontation)
  df_rule <- validate::as.data.frame(rule)
  validation <- merge(df_confrontation, df_rule)
  
}

