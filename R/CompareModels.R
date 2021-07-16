# Functions for comparing models

#' Compare flow totals for two models
#' @param modelA, a useeior model
#' @param modelB a useeior model 
#' @return comparison, a list with pass/fail comparison results
#' @export
compareFlowTotals <- function(modelA, modelB) {
  # Get flow totals for each model
  A <- groupandsumTbSbyFlowLoc(modelA$TbS)
  B <- groupandsumTbSbyFlowLoc(modelB$TbS)
  # Generate a comparison to see if flow totals from two models are within 1%
  rel_diff <- (A - B)/B
  comparison <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance = 0.01)
  return(comparison)
}

#' Sum totals_by_sector by flow and location
#' @param TbS a totals-by-sector list from a model
#' @return flowtotal, a df with 1 column of flowamount with a total for that flow given in rowname
groupandsumTbSbyFlowLoc <- function(TbS) {
  fields <- c("Flowable","Context","Unit","Location")
  TbS$FlowLoc <- apply(TbS[, fields],1,FUN = joinStringswithSlashes)
  TbS <- dplyr::group_by(TbS,FlowLoc) 
  TbS <- dplyr::summarize(TbS,FlowAmount = sum(FlowAmount))
  TbS <- as.data.frame(TbS)
  rownames(TbS) <- TbS$FlowLoc
  flowtotal <- subset(TbS,select="FlowAmount")
  return(flowtotal)
}
