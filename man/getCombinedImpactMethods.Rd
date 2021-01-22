% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LCIAfmtFunctions.R
\name{getCombinedImpactMethods}
\alias{getCombinedImpactMethods}
\title{Get and combine impact methods using the Python LCIAformatter package's get_mapped_method function.}
\usage{
getCombinedImpactMethods(parameters)
}
\arguments{
\item{parameters}{List of parameters, must include 'method_id' and list of two or more 'indicators'
to include from inventory method. Characterization factors for these two indicators are summed by flow.}
}
\value{
An LCIAmethod with the specified indicators
}
\description{
Get and combine impact methods using the Python LCIAformatter package's get_mapped_method function.
}