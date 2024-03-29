% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_analysis.R
\name{get_concordance}
\alias{get_concordance}
\title{Calculate Harrell's C index.}
\usage{
get_concordance(data, ci = TRUE)
}
\arguments{
\item{data}{a data frame with .outcome and .fitted columns.}

\item{ci}{logical, should 95\% confidence interval be returned?}
}
\value{
a tibble with the statistic values.
}
\description{
Calculates Harrell's C, a wrapper around
\code{\link[survival]{concordance}}.
}
