# This script provides print, summary and plotting methods for the predx class.

# extractor methods -----

#' Number of complete observations in a predx object
#'
#' @description The number of complete observations.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @return number of complete cases used for modeling.
#' @export

  nobs.predx <- function(predx_object) {

    stopifnot(class(predx_object) == 'predx')

    length(unique(predx_object$data$.observation))

  }

#' Extract features of predx objects.
#'
#' @description a handy extractor function enabling access to the predictions, number of observations and fit summary.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @param what name of the requested feature.
#' @details 'data' or 'prediction' returns the predicted values, 'classes' returns the outcome classes,
#' 'fit' the summary of fit stats, 'n' the number of complete cases, 'residuals' for working residuals,
#' 'confusion' for the confusion matrix with the raw counts.
#' @return the requested feature.
#' @export extract.predx
#' @export

  extract.predx <- function(predx_object,
                            what = c('data', 'prediction', 'classes', 'fit', 'n', 'residuals', 'confusion')) {

    stopifnot(class(predx_object) == 'predx')

    what <- match.arg(what[1], c('data', 'prediction', 'classes', 'fit', 'n', 'residuals'))

    switch(what,
           data = predx_object$data,
           prediction = predx_object$data,
           classes = predx_object$classes,
           fit = summary(predx_object),
           n = nobs(predx_object),
           residuals = residuals(predx_object),
           confusion = confusion(predx_object))

  }

#' Extract working residuals from a predx object.
#'
#' @description Extracts extended working residuals of prediction and potential outliers.
#' @details Calculates working residuals for regression and binary classification predictions.
#' In addition, squared and standardized residuals are returned along with
#' expected normal distribution values for the standardized residuals and the true outcome.
#' Potential outliers are identified by the two-SD criterion.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @return a data frame with residuals.
#' @export

  residuals.predx <- function(predx_object) {

    stopifnot(class(predx_object) == 'predx')

    if(predx_object$type == 'multi_class') {

      warning('Residuals for the multi-class predictions are not available.', call. = FALSE)

      return(NULL)

    }

    caretExtra:::get_qc_tbl(predx_object)

  }

# printing ------

#' Print a predx object.
#'
#' @description prints the heading of the prediction table.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @return nothing, called for its side effects.
#' @export

  print.predx <- function(predx_object) {

    stopifnot(class(predx_object) == 'predx')

    print(predx_object$data)

  }

# summary ------

#' Get the fit statistic summary of a predx object.
#'
#' @description Calculates fit statistics of a predx object. For regression
#' those include the fit errors and outcome:fitted correlations. For binary classification
#' concordance (C-index), kappa, accuracy and receiver-operator characteristic (ROC) is returned. For multi-class
#' classification, only the accuracy and kappa are currently available.
#' @details Harrell's C-index is calculated as specified for \code{\link[survival]{concordance}}. Unweighted Cohen's kappa
#' is obtained as described for \code{\link[vcd]{Kappa}}. ROC is computed for prediction probabilities
#' using \code{\link[caret]{twoClassSummary}}. Fit errors are based on working residuals and include mean absolute error (MAE),
#' mean squared error (MSE) and root-MSE (RMSE). Pseudo R squared is calculated as 1 - MSE/Var(y). Pearson correlation is
#' obtained with \code{\link[base]{cor.test}}, Spearman correlation is computed with \code{\link[DescTools]{SpearmanRho}},
#' Kendall's TauB is obtained with \code{\link[DescTools]{KendallTauB}}.\cr
#' For cross-validation (CV) prediction, statistic values are calculated as mean across the CV with 95\% confidence intervals (CI).
#' For non-CV predictions, 95\% CI are obtained as an option by the specific downstream functions.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @param ci logical, should 95\% CI be calculated for non-CV predictions? ignored for CV prediction.
#' @param cv_ci_method method used for calculation of 95\% CI for the CV folds: normal distribution, percentile
#' or BCA (\code{\link[coxed]{bca}}). Defaults to 'percentile'.
#' @return a data frame with the fit summary statistic.
#' @export summary.predx
#' @export

  summary.predx <- function(predx_object, ci = FALSE, cv_ci_method = c('percentile', 'bca', 'norm')) {

    stopifnot(class(predx_object) == 'predx')

    cv_ci_method <- match.arg(cv_ci_method[1], c('percentile', 'bca', 'norm'))

    if(predx_object$prediction != 'cv') {

      switch(predx_object$type,
             multi_class = caretExtra:::analyze_multi(predx_object$data, ci = ci),
             regression = caretExtra:::analyze_reg(predx_object$data, ci = ci),
             binary = caretExtra:::analyze_binary(predx_object$data, classes = predx_object$classes, ci = ci))

    } else {

      switch(predx_object$type,
             multi_class = caretExtra:::analyze_cv(predx_object$data,
                                                   fun = caretExtra:::analyze_multi,
                                                   ci_method = cv_ci_method,
                                                   ci = FALSE),
             regression = caretExtra:::analyze_cv(predx_object$data,
                                                  fun = caretExtra:::analyze_reg,
                                                  ci_method = cv_ci_method,
                                                  ci = FALSE),
             binary = caretExtra:::analyze_cv(predx_object$data,
                                              fun = caretExtra:::analyze_binary,
                                              classes = predx_object$classes,
                                              ci_method = cv_ci_method,
                                              ci = FALSE))

    }

  }

# confusion matrix ------

#' Confusion matrix for binary and multi-class predx objects.
#'
#' @description Creates a confusion matrix, valid only for binary and multi-class predictions.
#' @details a wrapper around the base \code{\link[base]{table}} function.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @param scale indicates, how the table is to be scales. 'none' returns the counts, 'fraction' returns the fraction
#' of all observations, 'percent' returns the percent of all observations. Defaults to 'none'.
#' @return a table object storing the confusion matrix. For regression models NULL and a warning is generated.
#' @export

  confusion.predx <- function(predx_object, scale = c('none', 'fraction', 'percent')) {

    stopifnot(class(predx_object) == 'predx')

    scale <- match.arg(scale[1], c('none', 'fraction', 'percent'))

    if(predx_object$type == 'regression') {

      warning('Confusion matrix for regression predictions is not available.', call. = FALSE)

      return(NULL)

    }

    conf <- table(predx_object$data[c('.outcome', '.fitted')])

    switch(scale,
           none = conf,
           fraction = conf/sum(conf),
           percent = conf/sum(conf) * 100)

  }


# plotting -----

#' Plot diagnostic plots and predictions versus outcome for predx objects.
#'
#' @description Generates diagnostic plots of residuals or plots of fitted versis outcome
#' as appropriate for the particular prediction type and displays prediction type-specific analysis results.
#' @param predx_object an object of class 'predx_object' created e.g. by \code{\link{predict.caretx}}.
#' @param type type of the plot. 'diagnostic': diagnostic plots of residuals, 'fit': the fitted versus outcome plot
#' specific for the prediction character (regression, ROC for binary classification and heat map of the confusion matrix
#' for multi-class prediction), 'regression': point plot of the fitted vs outcome values, 'roc': ROC plot, 'confusion':
#' heat map representation of the confusion matrix.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle, statistics specific for the prediction type, if not specified otherwise by the user.
#' @param plot_tag plot tag, number of complete observations and events if not specified otherwise by the user.
#' @param x_lab X axis title.
#' @param y_lab Y axis title.
#' @param signif_digits significant digits used for rounding of statistic values presented in the plot.
#' @param cust_theme customized plot theme provided by the user.
#' @param ... extra arguments passed to plotting functions, as specified by the plot type.
#' For 'diagnostic': \code{\link{get_qc_plots}}, for 'regression': \code{\link{plot_regression}},
#' for 'roc': \code{\link{plot_roc}}, for 'confusion': \code{\link{plot_confusion}}.
#' @return a ggplot with the requested content.
#' @export plot.predx
#' @export

  plot.predx <- function(predx_object,
                         type = c('diagnostic', 'fit', 'regression', 'roc', 'confusion'),
                         plot_title = NULL,
                         plot_subtitle = NULL,
                         plot_tag = NULL,
                         signif_digits = 2,
                         cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control

    stopifnot(class(predx_object) == 'predx')
    stopifnot(any(class(cust_theme) == 'theme'))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    type <- match.arg(type[1], c('diagnostic', 'fit', 'regression', 'roc', 'confusion'))

    if(predx_object$type == 'multi_class' & !type %in% c('fit', 'confusion')) {

      warning('Only heat map of the confusion matrix is available for multi-class predictions.', call. = FALSE)

      return(NULL)

    }

    if(predx_object$type == 'regression' & type %in% c('roc', 'confusion')) {

      warning('ROC and confusion matrix plots are not available for regression predictions.', call. = FALSE)

      return(NULL)

    }

    if(predx_object$type == 'binary' & type == 'regression') {

      warning('Regression plots are not available for binary classification.', call. = FALSE)

      return(NULL)

    }

    ## analyses

    if(predx_object$type == 'binary') {

      roc_annotation <- dplyr::filter(summary(predx_object),
                                      statistic %in% c('AUC', 'Se', 'Sp'))

      if(predx_object$prediction == 'cv') {

        roc_annotation <- paste0('AUC = ', signif(roc_annotation[1, 2], signif_digits),
                                 ' [', signif(roc_annotation[1, 3], signif_digits),
                                 ' - ', signif(roc_annotation[1, 4], signif_digits), ']',
                                 '\nSe = ', signif(roc_annotation[2, 2], signif_digits),
                                 ' [', signif(roc_annotation[2, 3], signif_digits),
                                 ' - ', signif(roc_annotation[2, 4], signif_digits), ']',
                                 '\nSp = ', signif(roc_annotation[3, 2], signif_digits),
                                 ' [', signif(roc_annotation[3, 3], signif_digits),
                                 ' - ', signif(roc_annotation[3, 4], signif_digits), ']')

      } else {

        roc_annotation <- paste0('AUC = ', signif(unlist(roc_annotation[1, 2]), signif_digits),
                                 ', Se = ', signif(unlist(roc_annotation[2, 2]), signif_digits),
                                 ', Sp = ', signif(unlist(roc_annotation[3, 2]), signif_digits))

      }

    }

    if(is.null(plot_subtitle)) {

      stats <- summary(predx_object)

      plot_subtitle <- switch(predx_object$type,
                              regression = paste0('RMSE = ', signif(unlist(stats[3, 2]), signif_digits),
                                                  ', Rsq = ', signif(unlist(stats[4, 2]), signif_digits)),
                              binary = paste0('Rsq = ', signif(unlist(stats[4, 2]), signif_digits),
                                              ', Kappa = ', signif(unlist(stats[8, 2]), signif_digits)),
                              multi_class = paste0('Class. Err. = ', signif(unlist(stats[1, 2]), signif_digits),
                                                   ', Kappa = ', signif(unlist(stats[3, 2]), signif_digits)))
    }

    ## plotting

    if(type == 'diagnostic') {

      caretExtra:::get_qc_plots(predx_object = predx_object,
                                cust_theme = cust_theme, ...)

    } else if(type == 'fit') {

      switch(predx_object$type,
             regression = caretExtra:::plot_regression(predx_object = predx_object,
                                                       x_var = '.outcome',
                                                       y_var = '.fitted',
                                                       plot_title = plot_title,
                                                       plot_subtitle = plot_subtitle,
                                                       plot_tag = plot_tag,
                                                       cust_theme = cust_theme, ...),
             binary = caretExtra:::plot_roc(predx_object = predx_object,
                                            plot_title = plot_title,
                                            plot_subtitle = plot_subtitle,
                                            plot_tag = plot_tag,
                                            cust_theme = cust_theme,
                                            annotation_txt = roc_annotation, ...),
             multi_class = caretExtra:::plot_confusion(predx_object = predx_object,
                                                       plot_title = plot_title,
                                                       plot_subtitle = plot_subtitle,
                                                       plot_tag = plot_tag,
                                                       cust_theme = cust_theme, ...))

    } else {

      switch(type,
             regression = caretExtra:::plot_regression(predx_object = predx_object,
                                                       x_var = '.outcome',
                                                       y_var = '.fitted',
                                                       plot_title = plot_title,
                                                       plot_subtitle = plot_subtitle,
                                                       plot_tag = plot_tag,
                                                       cust_theme = cust_theme, ...),
             roc = caretExtra:::plot_roc(predx_object = predx_object,
                                         plot_title = plot_title,
                                         plot_subtitle = plot_subtitle,
                                         plot_tag = plot_tag,
                                         cust_theme = cust_theme,
                                         annotation_txt = roc_annotation, ...),
             confusion = caretExtra:::plot_confusion(predx_object = predx_object,
                                                     plot_title = plot_title,
                                                     plot_subtitle = plot_subtitle,
                                                     plot_tag = plot_tag,
                                                     cust_theme = cust_theme, ...))

    }


  }
