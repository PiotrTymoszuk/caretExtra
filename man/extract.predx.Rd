% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predx_oop.R
\name{extract.predx}
\alias{extract.predx}
\title{Extract features of predx objects.}
\usage{
\method{extract}{predx}(
  x,
  what = c("data", "prediction", "classes", "fit", "n", "residuals", "confusion"),
  ...
)
}
\arguments{
\item{x}{an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.}

\item{what}{name of the requested feature.}
}
\value{
the requested feature.
}
\description{
a handy extractor function enabling access to the predictions, number of observations and fit summary.
}
\details{
'data' or 'prediction' returns the predicted values, 'classes' returns the outcome classes,
'fit' the summary of fit stats, 'n' the number of complete cases, 'residuals' for working residuals,
'confusion' for the confusion matrix with the raw counts.
}
