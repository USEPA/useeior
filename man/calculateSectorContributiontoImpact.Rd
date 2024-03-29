% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalculationFunctions.R
\name{calculateSectorContributiontoImpact}
\alias{calculateSectorContributiontoImpact}
\title{Calculate the percent contribution of sectors to direct+indirect impacts by an indicator,
using the product of model L matrix (total requirements) and D matrix (direct impacts by indicator).}
\usage{
calculateSectorContributiontoImpact(model, sector, indicator, domestic = FALSE)
}
\arguments{
\item{model}{A complete EEIO model: a list with USEEIO model components and attributes.}

\item{sector, }{str, index of a model sector for use in the L matrix, e.g. "221100/US".}

\item{indicator, }{str, index of a model indicator for use in the D matrix, e.g. "Acidification Potential".}

\item{domestic, }{boolean, sets model to use domestic flow matrix. Default is FALSE.}
}
\value{
A dataframe sorted by contribution (high-to-low), also showing "L", "D", "impact".
}
\description{
Calculate the percent contribution of sectors to direct+indirect impacts by an indicator,
using the product of model L matrix (total requirements) and D matrix (direct impacts by indicator).
}
