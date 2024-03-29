% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculationFunctions.R
\name{calculateFlowContributiontoImpact}
\alias{calculateFlowContributiontoImpact}
\title{Calculate the percent contribution of flows to direct+indirect impacts by an indicator,
using model M matrix for total impacts of flows and C matrix for indicator.}
\usage{
calculateFlowContributiontoImpact(model, sector, indicator, domestic = FALSE)
}
\arguments{
\item{model}{A complete EEIO model: a list with USEEIO model components and attributes.}

\item{sector, }{str, index of a model sector for use in the M matrix, e.g. "221100/US".}

\item{indicator, }{str, index of a model indicator for use in the C matrix, e.g. "Acidification Potential".}

\item{domestic, }{boolean, sets model to use domestic flow matrix. Default is FALSE.}
}
\value{
A dataframe sorted by contribution (high-to-low), also showing "M", "C", "impact".
}
\description{
Calculate the percent contribution of flows to direct+indirect impacts by an indicator,
using model M matrix for total impacts of flows and C matrix for indicator.
}
