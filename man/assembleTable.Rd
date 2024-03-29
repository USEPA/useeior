% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisaggregateFunctions.R
\name{assembleTable}
\alias{assembleTable}
\title{Assemble Table from the various disaggregated components.}
\usage{
assembleTable(
  originalTable,
  disagg,
  disaggCols,
  disaggRows,
  disaggIntersection
)
}
\arguments{
\item{originalTable}{Dataframe. The original table before disaggregation}

\item{disagg}{Specifications for disaggregating the current Table}

\item{disaggCols}{Dataframe. Previously disaggregated columns of the table.}

\item{disaggRows}{Dataframe. Previously disaggregated rows of the table.}

\item{disaggIntersection}{Dataframe. Previously disaggregated intersection of the table.}
}
\value{
The Disaggregated table as a dataframe with the disaggregated rows, columns, and intersection included
}
\description{
Assemble Table from the various disaggregated components.
}
