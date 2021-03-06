# This script provides methods for outcome prediction (training, cross-validation, test), analysis
# and plotting for the caretx class.

# Prediction methods -----

#' Predict the outcome for a caretx model.
#'
#' @description predicts the outcome in train, cross-validation and, optionally, test data set.
#' @details extends the output of \code{\link[caret]{predict.train}} method.
#' @param object caretx model.
#' @param newdata test data set.
#' @param plain logical, should the results be coerced to a single data frame?
#' @return a list of predx objects containing the predictions, model and data set type information
#' or a prediction data frame if plain is set to TRUE.
#' @export predict.caretx
#' @export

  predict.caretx <- function(object, newdata = NULL, plain = FALSE) {

    stopifnot(is_caretx(object))
    stopifnot(is.logical(plain))

    if(is.null(newdata)) {

      preds <- switch(object$modelType,
                      Classification = list(train = caretExtra:::predict_class_train(object),
                                            cv = caretExtra:::predict_class_cv(object),
                                            test = NULL),
                      Regression = list(train = caretExtra:::predict_reg_train(object),
                                        cv = caretExtra:::predict_reg_cv(object),
                                        test = NULL))

    } else {

      preds <- switch(object$modelType,
                      Classification = list(train = caretExtra:::predict_class_train(object),
                                            cv = caretExtra:::predict_class_cv(object),
                                            test = caretExtra:::predict_class_test(object, newdata = newdata)),
                      Regression = list(train = caretExtra:::predict_reg_train(object),
                                        cv = caretExtra:::predict_reg_cv(object),
                                        test = caretExtra:::predict_reg_test(object, newdata = newdata)))

    }

    if(plain) {

      purrr::map2_df(preds,
                     names(preds),
                     ~dplyr::mutate(extract(.x, 'data'), prediction = .y))

    } else {

      preds

    }

  }

# Analysis methods, summary ------

#' Get the fit statistic summary of a caretx object.
#'
#' @description Calculates fit statistics of a caretx object in the training and cross-validation data set.
#' If newdata is provided, fit statistics are computed for the test data as well.
#' For regression those include the fit errors and outcome:fitted correlations. For binary classification
#' concordance (C-index), kappa, accuracy and receiver-operator characteristic (ROC) is returned. For multi-class
#' classification, only the accuracy and kappa are currently available.
#' @details Harrell's C-index is calculated as specified for \code{\link[survival]{concordance}}. Unweighted Cohen's kappa
#' is obtained as described for \code{\link[vcd]{Kappa}}. ROC is computed for prediction probabilities
#' using \code{\link[caret]{twoClassSummary}}. Fit errors are based on working residuals and include mean absolute error (MAE),
#' mean squared error (MSE) and root-MSE (RMSE). Pseudo R squared is calculated as 1 - MSE/Var(y). Caret R squared is calulated
#' as root square of the Pearson correaltion coefficient between the fitted and observed values. Pearson correlation is
#' obtained with \code{\link[base]{cor.test}}, Spearman correlation is computed with \code{\link[DescTools]{SpearmanRho}},
#' Kendall's TauB is obtained with \code{\link[DescTools]{KendallTauB}}.\cr
#' For cross-validation (CV) prediction, statistic values are calculated as mean across the CV with 95\% confidence intervals (CI).
#' For non-CV predictions, 95\% CI are obtained as an option by the specific downstream functions.
#' @param object caretx model.
#' @param newdata test data set.
#' @param ci logical, should 95\% CI be calculated for non-CV predictions? ignored for CV prediction.
#' @param cv_ci_method method used for calculation of 95\% CI for the CV folds: normal distribution, percentile
#' or BCA (\code{\link[coxed]{bca}}). Defaults to 'percentile'.
#' @param plain logical, should the results be coerced to a single data frame?
#' @return a list of data frames with the fit summary statistic or a prediction data frame if plain is set to TRUE.
#' @export summary.caretx
#' @export

  summary.caretx <- function(object,
                             newdata = NULL,
                             ci = FALSE,
                             cv_ci_method = c('percentile', 'bca', 'norm'),
                             plain = FALSE) {

    stopifnot(is_caretx(object))
    stopifnot(is.logical(plain))

    preds <- predict(object,
                     newdata = newdata,
                     plain = FALSE)

    preds <- purrr::compact(preds)

    stats <- purrr::map(preds,
                        ~summary(.x,
                                 ci = ci,
                                 cv_ci_method = cv_ci_method))

    if(plain) {

      purrr::map2_dfr(stats, names(stats), ~dplyr::mutate(.x, prediction = .y))

    } else {

      stats

    }

  }

# Numbers of observations, residuals and an extractor method -----

#' Number of complete observations in a caretx model.
#'
#' @description The number of complete observations.
#' @param x caretx model.
#' @return number of complete cases used for modeling.
#' @export

  nobs.caretx <- function(x) {

    stopifnot(is_caretx(x))

    nrow(x$trainingData)

  }

#' Extract working residuals from a caretx model.
#'
#' @description Extracts extended working residuals of prediction and potential outliers.
#' @details Calculates working residuals for regression and binary classification predictions.
#' In addition, squared and standardized residuals are returned along with
#' expected normal distribution values for the standardized residuals and the true outcome.
#' Potential outliers are identified by the two-SD criterion.
#' @param object caretx model.
#' @param newdata test data set.
#' @return a list of data frames with residuals and potential outliers.
#' @export

  residuals.caretx <- function(object, newdata = NULL) {

    stopifnot(is_caretx(object))

    preds <- predict(object,
                     newdata = newdata,
                     plain = FALSE)

    preds <- purrr::compact(preds)

    purrr::map(preds, residuals)

  }

#' Confusion matrix for binary and multi-class caretx models.
#'
#' @description Creates a confusion matrix, valid only for binary and multi-class predictions.
#' @param x caretx model.
#' @param newdata newdata test data set.
#' @param scale indicates, how the table is to be scales. 'none' returns the counts, 'fraction' returns the fraction
#' of all observations, 'percent' returns the percent of all observations. Defaults to 'none'.
#' @return a list of table object storing the confusion matrices. For regression models NULL and a warning is generated.
#' @export

  confusion.caretx <- function(x, newdata = NULL, scale = c('none', 'fraction', 'percent')) {

    stopifnot(is_caretx(x))

    scale <- match.arg(scale[1], c('none', 'fraction', 'percent'))

    preds <- predict(x,
                     newdata = newdata,
                     plain = FALSE)

    preds <- purrr::compact(preds)

    purrr::map(preds, confusion, scale = scale)

  }

#' Extract features of caretx models.
#'
#' @description a handy extractor function enabling access to the data, predictions, number of observations and fit summary.
#' @param x caretx model.
#' @param what name of the requested feature.
#' @param newdata newdata test data set.
#' @param ... additional arguments passed to the specific methods.
#' @details 'data' returns the training data set, 'prediction' returns the predicted values,
#' 'fit' the summary of fit stats, 'n' the number of complete cases, 'residuals' returns the fit residuals,
#' 'confusion' extracts the confusion matrices with counts.
#' @return the requested feature.
#' @export extract.caretx
#' @export

  extract.caretx <- function(x,
                             newdata = NULL,
                             what = c('data', 'prediction', 'fit', 'n', 'residuals', 'confusion'), ...) {

    stopifnot(is_caretx(x))

    what <- match.arg(what[1], c('data', 'prediction', 'fit', 'n', 'residuals', 'confusion'))

    switch(what,
           data = x$trainingData,
           prediction = predict(x, newdata = newdata, ...),
           fit = summary(x, newdata = newdata, ...),
           n = nobs(x),
           residuals = residuals(x, newdata = newdata),
           confusion = confusion(x, newdata = newdata))

  }

# Plotting methods ----

#' Plot diagnostic plots and predictions versus outcome for caretx models.
#'
#' @description Generates diagnostic plots of residuals or plots of fitted versis outcome
#' as appropriate for the particular prediction type and displays prediction type-specific analysis results.
#' @param x a caretxmodel.
#' @param newdata test data set.
#' @param type type of the plot. 'diagnostic': diagnostic plots of residuals, 'fit': the fitted versus outcome plot
#' specific for the prediction character (regression, ROC for binary classification and heat map of the confusion matrix
#' for multi-class prediction), 'regression': point plot of the fitted vs outcome values, 'roc': ROC plot, 'confusion':
#' heat map representation of the confusion matrix.
#' @param plot_title a vector with plot titles. Length 2 if no newdata provided, otherwise lenght 3.
#' @param signif_digits significant digits used for rounding of statistic values presented in the plot.
#' @param cust_theme customized plot theme provided by the user.
#' @param ... extra arguments passed to plotting functions, as specified by the plot type.
#' For 'diagnostic': \code{\link{get_qc_plots}}, for 'regression': \code{\link{plot_regression}},
#' for 'roc': \code{\link{plot_roc}}, for 'confusion': \code{\link{plot_confusion}}.
#' @return a list of ggplot objects with the requested content.
#' @export plot.caretx
#' @export

  plot.caretx <- function(x,
                          newdata = NULL,
                          type = c('diagnostic', 'fit', 'regression', 'roc', 'confusion'),
                          plot_title = NULL,
                          signif_digits = 2,
                          cust_theme = ggplot2::theme_classic(), ...) {

    stopifnot(is_caretx(x))

    preds <- predict(x,
                     newdata = newdata,
                     plain = FALSE)

    preds <- purrr::compact(preds)

    if(!is.null(plot_title)) {

      plot_list <- list(x = preds,
                        plot_title = plot_title)

    } else {

      plot_list <- list(x = preds)

    }

    purrr::pmap(plot_list,
                plot,
                type = type,
                signif_digits = signif_digits,
                cust_theme = cust_theme, ...)

  }

# Calibration -----

#' Calibrate a caretx model.
#'
#' @description Enables post-hoc quantile GAM calibration of the ML predictions using the
#' \code{\link[qgam]{qgam}} tool.
#' @details Currently, only quantile calibration for regresison models is implemented. If a vector of
#' quantiles is provided, the optimal one is choosen based on the maximum explained variance (more formal criteria
#' are in development).
#' @param x caretx model.
#' @param newdata test data set.
#' @param bs basis function for the smoother, ignored if a formula provided.
#' @param k degrees of freedom for the smoother, ignored if a formula provided.
#' @param qu quantile for the calibration, see: \code{\link[qgam]{qgam}} for details.
#' @param form GAM formula as specified by \code{\link[mgcv]{formula.gam}}. The uncalibrated predictions
#' are stored internally in the '.raw' variable, which needs to be included in the user-provided formula.
#' @param lsig the value of the log learning rate used to create the Gibbs posterior,
#' see: \code{\link[qgam]{qgam}} for details.
#' @param err an upper bound on the error of the estimated quantile curve,
#' see: \code{\link[qgam]{qgam}} for details.
#' @param control a list of control parameters passed to \code{\link[qgam]{qgam}}.
#' @param argGam  a list of parameters to be passed to \code{\link[mgcv]{gam}},
#' with the exception of formula, family and data.
#' @param ... extra arguments passed to \code{\link[qgam]{qgam}}.
#' @return a list with the predx object (.raw stores the uncalibrated predctions, .fitted stores the calibrated predictions)
#' along with the gamObject named cal_fit, the chosen quantile value (qu) and values of explained deviance (qu_tbl).
#' @export calibration.caretx
#' @export

  calibration.caretx <- function(x,
                                 newdata = NULL,
                                 bs = 'cr',
                                 k = 20,
                                 qu = 0.5,
                                 form = NULL,
                                 lsig = NULL,
                                 err = NULL,
                                 control = list(link = 'identity'),
                                 argGam = NULL, ...) {

    ## entry control

    stopifnot(is_caretx(x))

    if(x$modelType != 'Regression') stop('Calibration is currently implemented only for regression models.', call. = FALSE)

    ## regression models

    caretExtra:::calibrate_regression(x,
                                      newdata = newdata,
                                      bs = bs,
                                      k = k,
                                      qu = qu,
                                      form = form,
                                      lsig = lsig,
                                      err = err,
                                      control = control,
                                      argGam = argGam, ...)

  }
