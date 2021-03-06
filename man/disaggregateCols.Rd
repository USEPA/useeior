% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisaggregateFunctions.R
\name{disaggregateCols}
\alias{disaggregateCols}
\title{Disaggregate multiple columns from a table.}
\usage{
disaggregateCols(
  ColVectors,
  disagg_specs,
  duplicate = FALSE,
  notUniform = FALSE
)
}
\arguments{
\item{ColVectors}{A dataframe containing the columns to disaggregate}

\item{disagg_specs}{Specifications for disaggregating the current Table}

\item{duplicate}{A flag that indicates whether the disaggregated columns are to be duplicated or not (e.g. for CPI values)}

\item{notUniform}{A flag that indicates whether the disaggregated columns are to be disaggregated in uniform manner or not}
}
\value{
A dataframe with disaggregated columns.
}
\description{
Disaggregate multiple columns from a table.
}
