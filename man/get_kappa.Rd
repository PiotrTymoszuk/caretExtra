% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_analysis.R
\name{get_kappa}
\alias{get_kappa}
\title{Calculate Cohen's Kappa.}
\usage{
get_kappa(data, ci = TRUE)
}
\arguments{
\item{data}{a data frame with .outcome and .fitted columns.}
}
\value{
a tibble with the statistic values.
}
\description{
Calculates Cohen's Kappa, a wrapper around \code{\link[vcd]{Kappa}}.
}
