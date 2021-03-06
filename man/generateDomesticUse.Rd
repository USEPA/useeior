% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IOFunctions.R
\name{generateDomesticUse}
\alias{generateDomesticUse}
\title{Generate domestic Use table by adjusting Use table based on Import matrix.}
\usage{
generateDomesticUse(Use, specs)
}
\arguments{
\item{Use, }{dataframe of a Use table}

\item{specs, }{list of model specifications}
}
\value{
A Domestic Use table with rows as commodity codes and columns as industry and final demand codes
}
\description{
Generate domestic Use table by adjusting Use table based on Import matrix.
}
