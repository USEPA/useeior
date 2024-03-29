% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StateiorFunctions.R
\name{prepare2RDemand}
\alias{prepare2RDemand}
\title{Prepares a production demand vector representing production for two region models
Demand for SoI = SoI2SoI + RoUS2SoI
Demand for RoUS = SoI2RoUS + RoUS2RoUS}
\usage{
prepare2RDemand(model, location, domestic, demand_type = "Production")
}
\arguments{
\item{model}{An EEIO model object with model specs and IO tables loaded}

\item{location, }{str of location code for demand vector}

\item{domestic}{A logical parameter indicating whether to generate domestic demand vector.}

\item{demand_type}{A str indicating whether demand is Production or Consumption}
}
\value{
A named vector with demand
}
\description{
Prepares a production demand vector representing production for two region models
Demand for SoI = SoI2SoI + RoUS2SoI
Demand for RoUS = SoI2RoUS + RoUS2RoUS
}
