# computation of numeric metrics of the class identification quality.

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
#' and train data set.
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

# END ------
