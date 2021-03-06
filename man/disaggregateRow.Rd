% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisaggregateFunctions.R
\name{disaggregateRow}
\alias{disaggregateRow}
\title{Disaggregate a single row from a table.}
\usage{
disaggregateRow(
  originalRowVector,
  disagg_specs,
  duplicate = FALSE,
  notUniform = FALSE
)
}
\arguments{
\item{originalRowVector}{A dataframe containing the row to disaggregate}

\item{disagg_specs}{Specifications for disaggregating the current Table}

\item{duplicate}{A flag that indicates whether the disaggregated row is to be duplicated or not (e.g. for CPI values)}

\item{notUniform}{A flag that indicates whether the disaggregated row is to be disaggregated in uniform manner or not}
}
\value{
A dataframe with the original row disaggregated.
}
\description{
Disaggregate a single row from a table.
}
