# Functions for comparing models

#' Compare flow totals for two models
#' @param modelA A complete EEIO model: a list with USEEIO model components and attributes.
#' @param modelB A complete EEIO model: a list with USEEIO model components and attributes.
#' @return a list with pass/fail comparison results
#' @export
compareFlowTotals <- function(modelA, modelB) {
  # Get flow totals for each model
  A <- groupandsumTbSbyFlowLoc(modelA$TbS)
  B <- groupandsumTbSbyFlowLoc(modelB$TbS)
  # Generate a comparison to see if flow totals from two models are within 1%
  A_B <- merge(A, B, by = 0, all = TRUE)
  rel_diff <- (A_B$FlowAmount.x - A_B$FlowAmount.y)/A_B$FlowAmount.y
  comparison <- formatValidationResult(rel_diff, abs_diff = TRUE, tolerance = 0.01)
  # Report flow difference in models
  comparison[["FlowDifference"]] <- list(setdiff(rownames(A), rownames(B)),
                                         setdiff(rownames(B), rownames(A)))
  names(comparison[["FlowDifference"]]) <- c(paste("Flows in", modelA$specs$Model, "not in", modelB$specs$Model),
                                             paste("Flows in", modelB$specs$Model, "not in", modelA$specs$Model))
  # comparison[[paste("Flows in", modelB$specs$Model, "not in", modelA$specs$Model)]] <- setdiff(rownames(B), rownames(A))
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
