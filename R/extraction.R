# Accessing components and stats of the `caretx` models or `predx` predictions.

# General extractor: components method -------

#' Access features and statistics of models and predictions
#'
#' @description
#' The `components()` and `model.frame()` methods provide handy extractor
#' functions enabling access to the data, predictions, number of observations
#' and fit summary.
#'
#' @references
#' Brier GW. VERIFICATION OF FORECASTS EXPRESSED IN TERMS OF PROBABILITY.
#' Mon Weather Rev (1950) 78:1–3.
#' doi:10.1175/1520-0493(1950)078<0001:vofeit>2.0.co;2
#' @references
#' Goldstein-Greenwood J. A Brief on Brier Scores | UVA Library. (2021) Available
#' at: https://library.virginia.edu/data/articles/a-brief-on-brier-scores
#'
#' @param object a \code{\link{caretx}} model or
#' \code{\link{predx}} prediction object.
#' @param formula \code{\link{caretx}} model or
#' \code{\link{predx}} prediction object.
#' @param what name of the requested feature:
#'
#' * `data`: the training data (`caretx`) or predictions (`predx`)
#'
#' * `formula`: the model's formula
#'
#' * `resample`: a data frame with tuning fit stats in the model's
#' resamples (e.g. CV folds)
#'
#' * `tuning`: a data frame with tuning results.
#'
#' * `best_tune`: the combination of tuning parameters for
#' the model with the optimal performance in the resamples.
#'
#' * `prediction`: the predictions, optionally for the test data provides as
#' the `newdata` argument
#'
#' * `fit`: returns statistics of the model fit
#'
#' * `classes`: returns the outcome class vector
#'
#' * `probability`: a data frame or a list of data frames with class assignment
#' probability
#'
#' * `square_dist`: squared distance from the outcome as defined by Brier at al.
#'
#' * `n`: the complete case number
#'
#' * `n_classes`: number of observations in the true outcome and fitted classes.
#' NULL for regression models.
#'
#' * `residuals`: model fit residuals
#'
#' * `confusion`: computes the confusion matrix with observation counts
#'
#' @param newdata test data set required for making predictions.
#' NULL by default, used only if `what` is set to `prediction`.
#' @param ... additional arguments passed to the specific methods.
#'
#' @return the requested feature.
#'
#' @export components.caretx
#' @export

  components.caretx <- function(object,
                             newdata = NULL,
                             what = c('data', 'formula',
                                      'resample', 'tuning', 'best_tune',
                                      'prediction', 'probability',
                                      'square_dist',
                                      'fit', 'n', 'n_classes', 'residuals',
                                      'confusion'), ...) {

    stopifnot(is_caretx(object))

    what <-
      match.arg(what[1],
                c('data', 'formula',
                  'resample', 'tuning', 'best_tune',
                  'prediction', 'probability', 'square_dist',
                  'fit', 'n', 'n_classes', 'residuals',
                  'confusion'))

    switch(what,
           data = object$trainingData,
           formula = formula(object),
           resamples = object$resamples,
           tuning = object$results,
           best_tune = object$bestTune,
           prediction = predict(object, newdata = newdata, ...),
           probability = classp(object, newdata = newdata, ...),
           square_dist = squared(object, newdata = newdata, ...),
           fit = summary(object, newdata = newdata, ...),
           n = nobs(object),
           n_classes = ngroups(object, newdata = newdata, ...),
           residuals = residuals(object, newdata = newdata),
           confusion = confusion(object, newdata = newdata))

  }

#' @rdname components.caretx
#' @export components.predx
#' @export

  components.predx <- function(object,
                            what = c('data', 'prediction',
                                     'classes', 'probability', 'square_dist',
                                     'fit', 'n', 'n_classes',
                                     'residuals', 'confusion'), ...) {

    stopifnot(is_predx(object))

    what <-
      match.arg(what[1],
                c('data', 'prediction',
                  'classes', 'probability', 'square_dist',
                  'fit', 'n', 'n_classes', 'residuals', 'confusion'))

    switch(what,
           data = object$data,
           prediction = object$data,
           classes = object$classes,
           probability = classp(object, ...),
           square_dist = squared(object, ...),
           fit = summary(object),
           n = nobs(object),
           n_classes = ngroups(object, ...),
           residuals = residuals(object),
           confusion = confusion(object))

  }

#' @rdname components.caretx
#' @export

  model.frame.predx <- function(formula, ...) formula$data

#' @rdname components.caretx
#' @export

  model.frame.caretx <- function(formula, ...) NextMethod()

# Residuals -------

#' Calculate model or prediction residuals.
#'
#' @description
#' Computes extended working residuals of prediction and potential outliers.
#'
#' @details Calculates working residuals for regression and binary
#' classification predictions.
#' In addition, squared and standardized residuals are returned along with
#' expected normal distribution values for the standardized residuals
#' and the true outcome.
#' Potential outliers are identified by the two-SD criterion.
#' Returns NULL for multi-class models.
#'
#' @param object \code{\link{caretx}} model or
#' \code{\link{predx}} prediction object.
#' @param newdata test data set.
#' @param ... extra arguments, currently none.
#'
#' @return a list of data frames for the test, resample (CV) and
#' training data set prediction with residuals and potential outliers.
#'
#' @export residuals.caretx
#' @export

  residuals.caretx <- function(object, newdata = NULL, ...) {

    stopifnot(is_caretx(object))

    preds <- predict(object,
                     newdata = newdata,
                     plain = FALSE)

    map(compact(preds), residuals)

  }

#' @rdname residuals.caretx
#' @export residuals.predx
#' @export

  residuals.predx <- function(object, ...) {

    stopifnot(is_predx(object))

    if(object$type == 'multi_class') {

      warning('Residuals for the multi-class predictions are not available.',
              call. = FALSE)

      return(NULL)

    }

    get_qc_tbl(object)

  }

# Confusion matrix -------

#' Confusion matrix for binary and multi-class models and predictions.
#'
#' @description Creates a confusion matrix, valid only for binary and
#' multi-class models and predictions.
#'
#' @details
#' `confusion()` is a S3 generic function.
#'
#' @param x \code{\link{caretx}} model or \code{\link{predx}} prediction object.
#' @param newdata newdata test data set.
#' @param scale indicates, how the table is to be scaled:
#'
#' * `none`: returns the counts (default)
#'
#' * `fraction` returns the fraction of all observations
#'
#' * `percent` returns the percent of all observations
#'
#' @param ... extra arguments passed to methods.
#'
#' @return a list of table object storing the confusion matrices (`caretx`)
#' or a single table bject representing the confusion matrix  (`predx`).
#' For regression models NULL and a warning is generated.#
#'
#' @export

  confusion <- function(x, ...) UseMethod('confusion')

#' @rdname confusion
#' @export confusion.caretx
#' @export

  confusion.caretx <- function(x,
                               newdata = NULL,
                               scale = c('none', 'fraction', 'percent'),
                               ...) {

    stopifnot(is_caretx(x))

    scale <- match.arg(scale[1], c('none', 'fraction', 'percent'))

    preds <- predict(x,
                     newdata = newdata,
                     plain = FALSE)

    map(compact(preds), confusion, scale = scale)

  }

#' @rdname confusion
#' @export confusion.predx
#' @export

  confusion.predx <- function(x,
                              scale = c('none', 'fraction', 'percent'),
                              ...) {

    stopifnot(is_predx(x))

    scale <- match.arg(scale[1], c('none', 'fraction', 'percent'))

    if(x$type == 'regression') {

      warning('Confusion matrix for regression predictions is not available.', call. = FALSE)

      return(NULL)

    }

    conf <- table(x$data[c('.outcome', '.fitted')])

    switch(scale,
           none = conf,
           fraction = conf/sum(conf),
           percent = conf/sum(conf) * 100)

  }

# Class assignment probability -------

#' Extract class assignment probability from models and predictions.
#'
#' @description
#' Computes the class assignment probability for classification models.
#'
#' @details
#' Returns NULL for regression models and predictions and throws a warning.
#' The `classp` is a S3 generic function.
#'
#' @return For `predx` objects, a data frame with
#' the columns `.outcome` and `.fitted` representing the true outcome
#' and predicted class assignment, respectively, columns storing class
#' assignment probabilities as welll as the `winner_p` variable storing
#' the probability for the predicted class.
#' For `caretx` models, the function returns a list of tibbles described above,
#' each one for the training, resample (CV) and test data sets.
#'
#' @param x \code{\link{caretx}} model or \code{\link{predx}} prediction object.
#' @param newdata test data set (optional).
#' @param ... extra arguments passed to methods, currently none.
#'
#' @export

  classp <- function(x, ...) UseMethod('classp')

#' @rdname classp
#' @export classp.predx
#' @export

  classp.predx <- function(x, ...) {

    ## entry control

    stopifnot(is_predx(x))

    if(x$type == 'regression') {

      warning('Class assignment probabilities are not available for regression.',
              call. = FALSE)

      return(NULL)

    }

    ## voting results

    vote_res <- components(x, 'data')

    winner_res <- split(vote_res, vote_res[['.fitted']])

    winner_p <- NULL

    winner_res <-
      map2_dfr(winner_res, names(winner_res),
               ~mutate(.x, winner_p = .data[[.y]]))

    winner_res <-
      select(winner_res, any_of(c('.observation', '.resample', 'winner_p')))

    if('.resample' %in% names(vote_res)) {

      by_vars <- c('.observation', '.resample')

    } else {

      by_vars <- '.observation'

    }

    left_join(vote_res, winner_res, by = by_vars)

  }

#' @rdname classp
#' @export classp.caretx
#' @export

  classp.caretx <- function(x, newdata, ...) {

    ## entry control

    stopifnot(is_caretx(x))

    if(x$modelType != 'Classification') {

      warning('Class assignment probabilities are not available for regression.',
              call. = FALSE)

      return(NULL)

    }

    preds <- predict(x, newdata = newdata)

    map(compact(preds), classp)

  }

# Square distances from outcome -------

#' Square distances from the outcome.
#'
#' @description
#' Calculates square distances from the true outcome, i.e. squared differences
#' between the outcome and fitted class probability as defined in the formula
#' of the Brier score.
#'
#' @details The sum of squared distances between fitted and outcome over
#' all observations is the Brier score, which ranges from 0 to 1 for binary
#' classification models and from 0 to 2 for multi-class classifiers.
#' In each case, 0 indicates a perfect concordance, while 1 or 2
#' (binary or multi level) hallmarks a completely false prediction.
#' For regression, squared working residuals are returned.
#' `squared()` is a S3 generic function.
#'
#' @return For `predx` objects, a data frame with
#' the columns `.outcome` and `.fitted` representing the true outcome
#' and predicted class assignment, respectively, and `square_dist` with the
#' squared distance from the outcome.
#'
#' @references
#' Brier GW. VERIFICATION OF FORECASTS EXPRESSED IN TERMS OF PROBABILITY.
#' Mon Weather Rev (1950) 78:1–3.
#' doi:10.1175/1520-0493(1950)078<0001:vofeit>2.0.co;2
#' @references
#' Goldstein-Greenwood J. A Brief on Brier Scores | UVA Library. (2021) Available
#' at: https://library.virginia.edu/data/articles/a-brief-on-brier-scores
#'
#' @param x \code{\link{caretx}} model or \code{\link{predx}} prediction object.
#' @param newdata test data set (optional).
#' @param ... extra arguments passed to methods, currently none.
#'
#' @export

  squared <- function(x, ...) UseMethod('squared')

#' @rdname squared
#' @export squared.predx
#' @export

  squared.predx <- function(x, ...) {

    ## entry control ------

    stopifnot(is_predx(x))

    square_dist <- NULL
    .num_outcome <- NULL
    .outcome <- NULL
    .resid <- NULL

    ## regression -------

    if(x$type == 'regression') {

      pred_data <-
        mutate(residuals(x), square_dist = (.resid)^2)

    }

    ## binary classification models -------

    pred_data <- components(x, 'data')

    if(x$type == 'binary') {

      pos_class <- levels(pred_data[['.outcome']])[2]

      pred_data <- mutate(pred_data,
                          .num_outcome = as.numeric(.outcome) - 1,
                          square_dist = (.num_outcome - .data[[pos_class]])^2)
    }

    ## multi-class predictions ------

    if(x$type == 'multi_class') {

      outcomes_num <-
        as.data.frame(DescTools::Dummy(pred_data[['.outcome']],
                                       method = 'full',
                                       levels = x$classes))

      fitted_num <- pred_data[x$classes]

      sqd <- map2(outcomes_num, fitted_num,
                  function(x, y) (x - y)^2)

      sqd <- reduce(sqd, function(x, y) x + y)

      pred_data <-
        mutate(pred_data, square_dist = sqd)


    }

    return(select(pred_data,
                  any_of(c('.observation',
                           '.resample',
                           '.outcome',
                           '.fitted',
                           'square_dist'))))
  }

#' @rdname squared
#' @export squared.caretx
#' @export

  squared.caretx <- function(x, newdata = NULL, ...) {

    stopifnot(is_caretx(x))

    preds <- predict(x, newdata = newdata, ...)

    map(compact(preds), squared)

  }

# END -------
