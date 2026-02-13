# Aggregation of predictions in repeated cross-validation

#' Case-wise collapsing of out-of-fold predictions.
#'
#' @description
#' Function `collapse_cv()` collapses out-of-fold predictions in cross-validation
#' by an user-provided function like `mean()` or `median()` in an observation-wise
#' manner.
#'
#' @details
#' The function collapses only predictions made in repeated cross-validation,
#' objects storing predictions of canonical cross-validation, or objects with
#' predictions in the training or test data are returned without any modifications.
#' For predictions made by regression, the predicted values are simply aggregated
#' by function `fun`.
#' For predictions made by classification models, the class-assignment probabilities
#' are aggregated by function `fun`, and the class assignment is determined by
#' nodal voting (see \code{\link{nodal_voting}}).
#' Because there is no possibility to elucidate the cross-validation fold identifier
#' for the aggregated observations, the `.resample` column in the output
#' \code{\link{predx}} object is replaced with the fold-assignment in the
#' first repeat of the cross-validation.
#' The function can't handle `predx` objects with calibration function attached:
#' in such cases, no modifications are made, and a warning is raised.
#'
#' @return
#' The `collapse_cv` method for \code{\link{predx}} objects returns an
#' \code{\link{predx}} with the collapsed observations.
#' The `collapse_cv` method for \code{\link{caretx}} models generates predictions
#' for the training, CV, and optionally, test data, and returns a list of
#' \code{\link{predx}} objects.
#'
#' @param x a \code{\link{predx}} object with model's predictions or
#' a \code{\link{caretx}} model.
#' @param fun a function used for aggregation of predicted values (regression)
#' or class probabilities. It should take a numeric vector and return
#' a single numeric value. `mean()` by default.
#' @param newdata test data set (optional).
#' @param ... additional arguments passed to `fun`.
#'
#' @export

  collapse_cv <- function(x, ...) UseMethod("collapse_cv")

#' @rdname collapse_cv
#' @export
#' @export collapse_cv.predx

  collapse_cv.predx <- function(x, fun = mean, ...) {

    ## input control -------

    stopifnot(is_predx(x))

    if(!is_function(fun)) stop("'fun' has to be a function.", call. = FALSE)

    if(x$prediction %in% c("train", "test")) return(x)

    if(!is.null(x$calibrator_fun)) {

      warning("The function can't handle predictions with a calibration function attached.",
              call. = FALSE)

      return(x)

    }

    ## data frames storing the numeric predictions and labels --------

    .observation <- NULL
    .outcome <- NULL
    .fitted <- NULL
    .resample <- NULL

    pred_data <- model.frame(x)

    meta_data <-
      filter(pred_data[, c(".observation", ".outcome", ".resample")],
             stri_detect(.resample, regex = "Rep1$"))

    ## checking if there was repeated cross-validation or any cases to collapse -------

    obs_freq <- as.numeric(table(pred_data[[".observation"]]))

    if(all(obs_freq == 1)) return(x)

    ## testing and wrapping the fun function --------

    new_fun <- function(x) fun(x, ...)

    tst_res <- try(new_fun(1:10), silent = TRUE)

    if(inherits(tst_res, "try-error")) {

      stop("'fun' can't handle numeric vectors without errors.", call. = FALSE)

    }

    if(length(tst_res) > 1 | !is.numeric(tst_res)) {

      stop("'fun' has to take a numeric vector and return a single numeric value.",
           call. = FALSE)

    }

    ## handling model types-----------

    if(x$type == "regression") {

      pred_data <- pred_data[, c(".observation", ".fitted")]

      pred_data <- group_by(pred_data, .observation)

      pred_data <- summarise(pred_data,
                             .fitted = new_fun(.fitted))

    } else {

      class_levs <- x$classes

      pred_data <- split(pred_data[, class_levs],
                         f = pred_data[[".observation"]])

      case_ids <- names(pred_data)

      pred_data <- map(pred_data, map_dbl, new_fun)

      pred_data <- reduce(pred_data, rbind)

      votes <- nodal_voting(pred_data)

      pred_data <-
        mutate(as.data.frame(pred_data),
               .observation = as.numeric(case_ids),
               .fitted = factor(class_levs[votes], class_levs))

      pred_data <- relocate(as_tibble(pred_data),
                            .observation, .fitted)

    }

    ## the output --------

    predx(data = left_join(meta_data,
                           pred_data,
                           by = ".observation"),
          classes = x$classes,
          type = x$type,
          prediction = x$prediction,
          calibrator_fun = NULL)

  }

#' @rdname collapse_cv
#' @export
#' @export collapse_cv.caretx

  collapse_cv.caretx <- function(x, fun = mean, newdata = NULL, ...) {

    stopifnot(is_caretx(x))

    preds <- predict(x, newdata = newdata, plain = FALSE)

    preds$cv <- collapse_cv(preds$cv, fun = fun, ...)

    return(preds)

  }

# END --------
