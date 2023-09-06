# Specific OOP for the `caretx` class: prediction and plotting

# Prediction methods -----

#' Predict the outcome for a caretx model.
#'
#' @description
#' Predicts the outcome in train, cross-validation and, optionally,
#' test data set.
#'
#' @details Extends the output of \code{\link[caret]{predict.train}} method.
#'
#' @param object \code{\link{caretx}} model.
#' @param newdata test data set.
#' @param plain logical, should the results be coerced to a single data frame?
#' @param ... extra arguments, currently none.
#'
#' @return a list of \code{\link{predx}} objects containing the predictions
#' for the training, resample (CV) and test data sets along with
#' model and data set type information.
#' See: \code{\link{components.predx}}, \code{\link{summary.predx}},
#' \code{\link{residuals.predx}}, \code{\link{confusion.predx}}
#' and \code{\link{plot.predx}} for useful methods for `predx` objects.
#' If `plain` is set to TRUE a prediction data frame if plain is set to TRUE.
#'
#' @export predict.caretx
#' @export

  predict.caretx <- function(object,
                             newdata = NULL,
                             plain = FALSE, ...) {

    stopifnot(is_caretx(object))
    stopifnot(is.logical(plain))

    if(is.null(newdata)) {

      preds <-
        switch(object$modelType,
               Classification = list(train = predict_class_train(object),
                                     cv = predict_class_cv(object),
                                     test = NULL),
               Regression = list(train = predict_reg_train(object),
                                 cv = predict_reg_cv(object),
                                 test = NULL))

    } else {

      preds <-
        switch(object$modelType,
               Classification = list(train = predict_class_train(object),
                                     cv = predict_class_cv(object),
                                     test = predict_class_test(object,
                                                               newdata = newdata)),
               Regression = list(train = predict_reg_train(object),
                                 cv = predict_reg_cv(object),
                                 test = predict_reg_test(object,
                                                         newdata = newdata)))

    }

    if(plain) {

      return(map2_df(preds,
                     names(preds),
                     ~mutate(components(.x, 'data'),
                             prediction = .y)))

    }

    return(preds)

  }
