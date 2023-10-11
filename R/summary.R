# Summary method for the `caretx` and `predx` classes.
# The method computes fit stats.

#' Fit statistic summary .
#'
#' @description
#' Calculates fit statistics for models (\code{\link{caretx}}) and
#' predictions (\code{\link{predx}}).
#' For regression, those include the fit errors and outcome - fitted value
#' correlations.
#' For binary classification, concordance (C-index), kappa, accuracy,
#' receiver-operator characteristic (ROC), weighted average class probabilities
#' and Brier scores are returned.
#' For multiple-class predictions and models, accuracy, kappa, weighted average
#' class probabilities and Brier scores are returned.
#'
#' @references
#' Kuhn M. Building predictive models in R using the caret package.
#' J Stat Softw (2008) 28:1–26. doi:10.18637/jss.v028.i05
#' @references
#' Brier GW. VERIFICATION OF FORECASTS EXPRESSED IN TERMS OF PROBABILITY.
#' Mon Weather Rev (1950) 78:1–3.
#' doi:10.1175/1520-0493(1950)078<0001:vofeit>2.0.co;2
#' @references
#' Goldstein-Greenwood J. A Brief on Brier Scores | UVA Library. (2021) Available
#' at: https://library.virginia.edu/data/articles/a-brief-on-brier-scores
#' @references
#' Cohen J. A Coefficient of Agreement for Nominal Scales. Educ Psychol Meas
#' (1960) 20:37–46. doi:10.1177/001316446002000104
#'
#'
#' @details
#' Harrell's C-index is calculated as specified for
#' \code{\link[survival]{concordance}}.
#' Receiver-operating characteristic (ROC), accuracy and unweighted Cohen's
#' kappa is computed for prediction
#' probabilities using caret's \code{\link[caret]{multiClassSummary}}.
#' Fit errors are based on working residuals and include
#' mean absolute error (MAE),
#' mean squared error (MSE) and root-MSE (RMSE).
#' Pseudo R squared is calculated as 1 - MSE/Var(y).
#' Pearson correlation is obtained with \code{\link[stats]{cor.test}},
#' Spearman correlation is computed with \code{\link[DescTools]{SpearmanRho}},
#' Kendall's TauB is obtained with \code{\link[stats]{cor}}.
#' For cross-validation (CV) prediction, statistic values are calculated as
#' mean across the CV with 95\% confidence intervals (CI).
#' For multi-class predictions and models, statistics referring to
#' discrimination of single classes like sensitivity or recall are averaged
#' over all classes.
#'
#' @param object a \code{\link{caretx}} model or \code{\link{predx}} prediction
#' object.
#' @param newdata optional, a data frame with the test data.
#' @param ci_method method used for calculation of 95\% CI for the CV folds:
#' normal distribution, percentile or BCA (\code{\link[coxed]{bca}}).
#' Defaults to 'percentile'.
#' @param plain logical, should the output be coerced to a single data frame?
#' @param ... extra arguments, currently none.
#'
#' @return a data frame with the fit summary statistic (`predx`) or
#' a list of such data frames for the training, resample (CV)
#' and test data (`caretx`).
#'
#' @export summary.predx
#' @export

  summary.predx <- function(object,
                            ci_method = c('percentile', 'bca', 'norm'),
                            ...) {

    stopifnot(is_predx(object))

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca', 'norm'))

    data <- filter(object$data, complete.cases(object$data))

    if(object$prediction != 'cv') {

      summ_object <-
        switch(object$type,
               multi_class = analyze_class(data,
                                           classes = object$classes,
                                           c_index = FALSE),
               regression = analyze_reg(data),
               binary = analyze_class(data,
                                      classes = object$classes))

    } else {

      summ_object <-
        switch(object$type,
               multi_class = analyze_cv(data,
                                        fun = analyze_class,
                                        classes = object$classes,
                                        ci_method = ci_method,
                                        c_index = FALSE,
                                        ci = FALSE),
               regression = analyze_cv(data,
                                       fun = analyze_reg,
                                       ci_method = ci_method,
                                       ci = FALSE),
               binary = analyze_cv(data,
                                   fun = analyze_class,
                                   classes = object$classes,
                                   ci_method = ci_method,
                                   ci = FALSE))

    }

    if(!object$type %in% c('binary', 'multi_class')) {

      return(summ_object)

    }

    rbind(summ_object,
          get_brier(squared(object), ci_method = ci_method),
          get_classp(classp(object), ci_method = ci_method))

  }

#' @rdname summary.predx
#' @export summary.caretx
#' @export

  summary.caretx <- function(object,
                             newdata = NULL,
                             ci_method = c('percentile', 'bca', 'norm'),
                             plain = FALSE, ...) {

    stopifnot(is_caretx(object))
    stopifnot(is.logical(plain))

    preds <- predict(object,
                     newdata = newdata,
                     plain = FALSE)

    stats <- map(compact(preds),
                 ~summary(.x,
                          ci_method = ci_method))

    if(plain) {

      prediction <- NULL

      return(map2_dfr(stats, names(stats),
                      ~mutate(.x, prediction = .y)))

    }

    return(stats)

  }

# END ------
