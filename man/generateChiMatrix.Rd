% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidateModel.R
\name{generateChiMatrix}
\alias{generateChiMatrix}
\title{Generate Chi matrix, i.e. ratios of model IO year commodity output over the output of the flow year in model IO year dollar.}
\usage{
generateChiMatrix(model, output_type = "Commodity")
}
\arguments{
\item{model}{A complete EEIO model: a list with USEEIO model components and attributes}

\item{output_type}{Either Commodity or Industry, default is Commodity}
}
\value{
Chi matrix contains ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
}
\description{
Generate Chi matrix, i.e. ratios of model IO year commodity output over the output of the flow year in model IO year dollar.
}
