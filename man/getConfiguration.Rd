% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConfigurationFunctions.R
\name{getConfiguration}
\alias{getConfiguration}
\title{Gets a stored or user specified model or aggregation/disaggregation configuration file}
\usage{
getConfiguration(configname, configtype, configpaths = NULL, pkg = "useeior")
}
\arguments{
\item{configname}{str, name of the configuration file}

\item{configtype}{str, configuration type, can be "model", "disagg", or "agg"}

\item{configpaths}{str vector, paths (including file name) of model configuration file
and optional agg/disagg configuration file(s). If NULL, built-in config files are used.}

\item{pkg}{str, indicate package for access to config, either "useeior" or "stateior"}
}
\value{
A list of model specifications.
}
\description{
Gets a stored or user specified model or aggregation/disaggregation configuration file
}
