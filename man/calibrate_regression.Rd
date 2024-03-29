% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_calibration.R
\name{calibrate_regression}
\alias{calibrate_regression}
\title{Calibrate a regression caretx model.}
\usage{
calibrate_regression(
  caretx_model,
  newdata = NULL,
  bs = "cr",
  k = 20,
  qu = 0.5,
  form = NULL,
  lsig = NULL,
  err = NULL,
  control = list(link = "identity"),
  argGam = NULL,
  ...
)
}
\arguments{
\item{caretx_model}{\code{\link{caretx}} model.}

\item{newdata}{test data set.}

\item{bs}{basis function for the smoother, ignored if a formula provided.}

\item{k}{degrees of freedom for the smoother, ignored if a formula provided..}

\item{qu}{quantile for the calibration, see: \code{\link[qgam]{qgam}}
for details.}

\item{form}{GAM formula as specified by \code{\link[mgcv]{formula.gam}}.
The uncalibrated predictions
are stored internally in the '.raw' variable, which needs to be included
in the user-provided formula.}

\item{lsig}{the value of the log learning rate used to create the
Gibbs posterior, see: \code{\link[qgam]{qgam}} for details.}

\item{err}{an upper bound on the error of the estimated quantile curve,
see: \code{\link[qgam]{qgam}} for details.}

\item{control}{a list of control parameters passed
to \code{\link[qgam]{qgam}}.}

\item{argGam}{a list of parameters to be passed to
\code{\link[mgcv]{gam}},
with the exception of formula, family and data.}

\item{...}{extra arguments passed to \code{\link[qgam]{qgam}}.}
}
\value{
a list of \code{predx} objects (train, cv and, if newdata provided, test)
along with the gamObject named \code{cal_fit},
the chosen quantile value (\code{qu}) and values of explained deviance (\code{qu_tbl}).
}
\description{
Enables post-hoc quantile GAM calibration of the ML predictions using the
\code{\link[qgam]{qgam}} tool.
}
\details{
The calibration is developed for the training data set and applied by
prediction to the CV and, if newdata provided, to the test data.
}
