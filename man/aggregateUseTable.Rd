% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AggregationFunctions.R
\name{aggregateUseTable}
\alias{aggregateUseTable}
\title{Aggregate the UseTable based on specified source file}
\usage{
aggregateUseTable(model, aggregationSpecs, domestic = FALSE)
}
\arguments{
\item{model}{An EEIO model object with model specs and IO tables loaded}

\item{aggregationSpecs}{Specifications for aggregation}

\item{domestic}{Boolean to indicate whether to aggregate the UseTransactions or DomesticUseTransactions table}
}
\value{
An aggregated UseTransactions or DomesticUseTransactions Table.
}
\description{
Aggregate the UseTable based on specified source file
}
