% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisaggregateFunctions.R
\name{disaggregateRows}
\alias{disaggregateRows}
\title{Disaggregate multiple rows from a table.}
\usage{
disaggregateRows(
  RowVectors,
  disagg_specs,
  duplicate = FALSE,
  notUniform = FALSE
)
}
\arguments{
\item{RowVectors}{A dataframe containing the rows to disaggregate}

\item{disagg_specs}{Specifications for disaggregating the current Table}

\item{duplicate}{A flag that indicates whether the disaggregated rows are to be duplicated or not (e.g. for CPI values)}

\item{notUniform}{A flag that indicates whether the disaggregated rows are to be disaggregated in uniform manner or not}
}
\value{
A dataframe with disaggregated rows.
}
\description{
Disaggregate multiple rows from a table.
}
