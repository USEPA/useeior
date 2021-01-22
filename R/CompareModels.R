#CompareModels.R

#' Compare flow totals for two models
#' @param TbSA a totals-by-sector list from a model
#' @param TbSB a totals-by-sector list from a model
#' @return df, the validate::confrontation result
compareFlowTotals <- function(TbSA, TbSB) {
  tbs_A <- groupandsumTbSbyFlowLoc(TbSA)
  tbs_B <- groupandsumTbSbyFlowLoc(TbSB)
  list_A_B <- harmonizeDFsbyrowname(tbs_A,tbs_B)
  A <- list_A_B[[1]]
  B <- list_A_B[[2]]
  
  rule <- validate::validator(abs(A - B)/A <= 0.01)
  confrontation <- validate::confront(A, rule, B)
  summary(confrontation)
  df_confrontation <- validate::as.data.frame(confrontation)
  return(df_confrontation)
}


groupandsumTbSbyFlowLoc <- function(TbS) {
  tbs <- data.frame(do.call("rbind", TbS))
  fields <- c("Flowable","Context","Unit","Location")
  tbs$FlowLoc <- apply(tbs[, fields],1,FUN = joinStringswithSlashes)
  tbs <- dplyr::group_by(tbs,FlowLoc) 
  tbs <- dplyr::summarize(tbs,FlowAmount = sum(FlowAmount))
  tbs <- as.data.frame(tbs)
  rownames(tbs) <- tbs$FlowLoc
  tbs <- subset(tbs,select="FlowAmount")
  return(tbs)
}
