# Numbers of complete observations and observations in the classes.

# Numbers of complete cases -------

#' Number of complete observations in a model or prediction.
#'
#' @description The number of complete observations.

#' @param object a \code{\link{caretx}} model or \code{\link{predx}}
#' prediction object.
#' @param ... extra arguments, currently none.
#'
#' @return number of complete cases used for modeling.
#'
#' @export

  nobs.caretx <- function(object, ...) {

    stopifnot(is_caretx(object))

    nrow(object$trainingData)

  }

#' @rdname nobs.caretx
#' @export

  nobs.predx <- function(object, ...) {

    stopifnot(is_predx(object))

    length(unique(object$data$.observation))

  }

# Numbers of observations in the classes ------

#' Numbers of observation in the actual and predicted classes.
#'
#' @description
#' Computes the number of observation in the outcome an predicted classes.
#'
#' @details
#' For regression, NULL is returned with a warning.
#'
#' @param x a \code{\link{caretx}} model or \code{\link{predx}}
#' prediction object.
#' @param newdata optional test data set.
#' @param ... extra arguments, currently none.
#'
#' @return a data frame with the class names and numbers of observations
#' in the outcome and fitted classes (`predx`) or a list of such data frames
#' for the training, resample (CV) and test datasets.
#'
#' @export ngroups.predx
#' @export

  ngroups.predx <- function(x, ...) {

    ## entry control ------

    stopifnot(is_predx(x))

    if(x$type == 'regression') {

      warning('Numbers of observations in the classes is not available for regression.',
              call = FALSE)

      return(NULL)

    }

    ## Number of observations -----

    pred_data <- components(x, 'data')

    classes <- c('.fitted' = '.fitted',
                 '.outcome' = '.outcome')

    counts <- map(classes, ~count(pred_data, .data[[.x]]))

    counts <-
      map2(counts,
           c('.fitted' = 'n_fitted',
             '.outcome' = 'n_outcome'),
           ~set_names(.x, c('class', .y)))

    reduce(counts, left_join, by = 'class')

  }

#' @rdname ngroups.predx
#' @export ngroups.caretx
#' @export

  ngroups.caretx <- function(x, newdata = NULL, ...) {

    stopifnot(is_caretx(x))

    preds <- predict(x, newdata = newdata)

    map(compact(preds), ngroups)

  }

# END -----
