# Specific OOP for the `caretx` class: prediction, augmentation and subsetting

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

# Augmentation: extended data frames ------

#' Predictions with explanatory variables.
#'
#' @description
#' The `augment()` method for \code{\link{caretx}} objects derives the
#' predictions for the training, resample (CV), and, optionally, test
#' data set appended with the explanatory variables.
#'
#' @param x a \code{\link{caretx}} object.
#' @param newdata an optional data frame with the test data.
#' @param ... extra arguments, currently none.
#'
#' @return a list of data frames. Each of them contains the
#' observation number (`.observation`),
#' resample ID (only for resample/CV, `.resample`),
#' outcome (`.outcome`), fitted values/classes (`.fitted`).
#' For classification models, class assignment probabilities are returned
#' as well (columns named after levels of the `.outcome` variable).
#'
#' @export augment.caretx
#' @export

  augment.caretx <- function(x, newdata = NULL, ...) {

    ## entry control ------

    stopifnot(is_caretx(x))

    .outcome <- NULL

    ## training data and predictions ------

    outcome_var <- as.character(formula(x))[[2]]

    train_data <- select(model.frame(x), -all_of(outcome_var))

    train_data <- mutate(train_data,
                         .observation = 1:nrow(train_data))

    preds <- map(compact(predict(x, newdata = newdata)),
                 model.frame)

    map(preds,
        left_join,
        train_data,
        by = '.observation')

  }

# Splitting ------

#' Split predictions by an explanatory factor.
#'
#' @description
#' The `split()` method for the \code{\link{caretx}} class generates predictions
#' for the training, resample (CV) and, optionally, test data set and splits
#' them by the levels of an explanatory factor present in the training data.
#'
#' @details
#' This method may be used to investigate quality of prediction in a particular
#' subset or subsets of the data set. The method returns a plain list of
#' \code{\link{predx}} objects, whose properties can be further explored
#' with the specific \code{\link{summary.predx}} and \code{\link{plot.predx}}
#' methods.
#'
#' @return a plain list of \code{\link{predx}} objects.
#'
#' @param x a \code{\link{caretx}} object.
#' @param f a splitting factor, in quoted or unquoted form.
#' @param drop logical, should unused levels of the splitting factor
#' f be dropped?
#' @param newdata an optional data frame with the test data.
#' @param ... extra arguments, currently none.
#'
#' @export split.caretx
#' @export

  split.caretx <- function(x,
                           f,
                           drop = FALSE,
                           newdata = NULL, ...) {

    ## entry control -------

    stopifnot(is_caretx(x))
    stopifnot(is.logical(drop))

    f <- rlang::as_string(rlang::ensym(f))

    ## augmented data --------

    aug_data <- augment(x, newdata = newdata)

    if(!f %in% names(aug_data[[1]])) {

      stop("'f' not found in the training data.", call. = FALSE)

    }

    split_vecs <- map(aug_data, ~.x[[f]])

    split_data <-
      map2(aug_data, split_vecs, split, drop = drop)

    split_data <- unlist(split_data, recursive = FALSE)

    ## predx objects --------

    classes <- levels(split_data[[1]][['.outcome']])

    split_data <- map(split_data,
                      select,
                      any_of(c('.observation',
                               '.resample',
                               '.outcome',
                               '.fitted',
                               classes)))

    pred_types <-
      stringi::stri_replace(names(split_data),
                            regex = '\\..*$',
                            replacement = '')

    if(is.null(levels(split_data[[1]][['.outcome']]))) {

      mod_type <- 'regression'

    } else {

      outcome_lens <- length(levels(split_data[[1]][['.outcome']]))

      if(outcome_lens == 2) mod_type <- 'binary' else mod_type <- 'multi_class'

    }

    pmap(list(data = split_data,
              prediction = pred_types),
         predx,
         classes = classes,
         type = mod_type)

  }

# END ------
