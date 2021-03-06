% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predx_oop.R
\name{summary.predx}
\alias{summary.predx}
\title{Get the fit statistic summary of a predx object.}
\usage{
\method{summary}{predx}(object, ci = FALSE, cv_ci_method = c("percentile", "bca", "norm"))
}
\arguments{
\item{object}{an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.}

\item{ci}{logical, should 95\% CI be calculated for non-CV predictions? ignored for CV prediction.}

\item{cv_ci_method}{method used for calculation of 95\% CI for the CV folds: normal distribution, percentile
or BCA (\code{\link[coxed]{bca}}). Defaults to 'percentile'.}
}
\value{
a data frame with the fit summary statistic.
}
\description{
Calculates fit statistics of a predx object. For regression
those include the fit errors and outcome:fitted correlations. For binary classification
concordance (C-index), kappa, accuracy and receiver-operator characteristic (ROC) is returned. For multi-class
classification, only the accuracy and kappa are currently available.
}
\details{
Harrell's C-index is calculated as specified for \code{\link[survival]{concordance}}. Unweighted Cohen's kappa
is obtained as described for \code{\link[vcd]{Kappa}}. ROC is computed for prediction probabilities
using \code{\link[caret]{twoClassSummary}}. Fit errors are based on working residuals and include mean absolute error (MAE),
mean squared error (MSE) and root-MSE (RMSE). Pseudo R squared is calculated as 1 - MSE/Var(y). Pearson correlation is
obtained with \code{\link[base]{cor.test}}, Spearman correlation is computed with \code{\link[DescTools]{SpearmanRho}},
Kendall's TauB is obtained with \code{\link[DescTools]{KendallTauB}}.\cr
For cross-validation (CV) prediction, statistic values are calculated as mean across the CV with 95\% confidence intervals (CI).
For non-CV predictions, 95\% CI are obtained as an option by the specific downstream functions.
}
