% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DemandFunctions.R
\name{sumforConsumption}
\alias{sumforConsumption}
\title{Sums the demand cols representing final consumption, i.e. household, investment, and government
Complete national consumption formula: y_c <-  Y_h + Y_v + Y_g 
Domestic portion of national consumption: y_dc <- Y_dh + Y_dv + Y_dg}
\usage{
sumforConsumption(model, Y, location)
}
\arguments{
\item{model}{An EEIO model object with model specs and IO tables loaded}

\item{Y, }{a model Demand df.}

\item{location, }{str of location code for demand vector}
}
\value{
A named vector with model sectors and demand amounts
}
\description{
Sums the demand cols representing final consumption, i.e. household, investment, and government
Complete national consumption formula: y_c <-  Y_h + Y_v + Y_g 
Domestic portion of national consumption: y_dc <- Y_dh + Y_dv + Y_dg
}
