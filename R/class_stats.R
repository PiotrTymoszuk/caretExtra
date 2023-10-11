# computation of numeric metrics of the class identification quality.

# Numeric stats -------

#' Class detection quality.
#'
#' @description
#' Computes a bunch of numeric statistics for quality of identification of
#' particular classes in an one versus rest manner.
#'
#' @details
#' For regression, NULL is returned with a warning.
#' The function uses internally \code{\link[caret]{multiClassSummary}}.
#' `clstats` is a S3 generic function.
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
#' @param x a \code{\link{predx}} prediction object or a \code{\link{caretx}}
#' model.
#' @param newdata optional, a data frame with the test data.
#' @param ... arguments passed to methods, currently none.
#'
#' @return For `predx` objects: a data frame with class-specific
#' receiver-operator characteristic as well as class-specific Brier scores
#' and average class assignment probabilities.
#' For `caretx` a list of such data frames, each one for the train, resample
#' and, it `newdata` is specified, the training data set.
#'
#' @export

  clstats <- function(x, ...) UseMethod('clstats')

#' @rdname clstats
#' @export clstats.predx
#' @export

  clstats.predx <- function(x, ...) {

    ## entry control -------

    stopifnot(is_predx(x))

    if(x$type == 'regression') {

      warning("Class-specific statistics are not available for regression.",
              call. = FALSE)

      return(NULL)

    }

    ## stats -----

    left_join(comp_class_roc(x), comp_class_stats(x), by = '.outcome')

  }

#' @rdname clstats
#' @export clstats.caretx
#' @export

  clstats.caretx <- function(x, newdata = NULL, ...) {

    stopifnot(is_caretx(x))

    preds <- predict(x, newdata = newdata, ...)

    map(compact(preds), clstats)

  }

# Class ROC plots --------

#' Receiver operating characteristic plots for outcome classes.
#'
#' @description
#' Draws receiver operating characteristic (ROC) plots for particular outcome
#' classes. The ROC statistics for particular classes are obtained by comparing
#' the given class the remaining ones (one versus rest comparison).
#'
#' @details
#' The function employs internally \code{\link[caret]{multiClassSummary}} and
#' plotting tools from the `plotROC` package. `clplots` is a S3 generic
#' function.
#'
#' @param one_plot logical: should all ROC curves be displayed in one plot?
#' @param ... extra arguments passed to \code{\link{plot_class_roc}}.
#' @inheritParams clstats
#'
#' @return a single `ggplot` object or a list of `ggplot` objects.
#' For `clplots.caretex` a list of `ggplots` with plots for the training data,
#' resamples and, if `newdata` is specified, also for the test data set.
#'
#' @export clplots

  clplots <- function(x, ...) UseMethod('clplots')

#' @rdname clplots
#' @export clplots.predx
#' @export

  clplots.predx <- function(x, one_plot = TRUE, ...) {

    plot_class_roc(x, one_plot = one_plot, ...)

  }

#' @rdname clplots
#' @export clplots.caretx
#' @export

  clplots.caretx <- function(x,
                             newdata = NULL,
                             one_plot = TRUE, ...) {

    stopifnot(is_caretx(x))

    preds <- compact(predict(x, newdata = newdata))

    map(preds, clplots, one_plot = one_plot, ...)

  }

# END ------
