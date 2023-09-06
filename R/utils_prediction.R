# Helper functions

#' Predict train data outcome.
#'
#' @description
#' a helper function extracting the predictions for the train, cross-validation
#' and test data set from a classification or regression `caretx` model.
#'
#' @param class_model a `caretx` classification model.
#' @param reg_model a `caretx` regression model.
#' @param newdata a data frame with test data.
#'
#' @return a `predx` object.

  predict_class_train <- function(class_model) {

    stopifnot(is_caretx(class_model))

    preds <- list()

    preds$.outcome <- class_model$trainingData$.outcome
    preds$.observation <- 1:length(class_model$trainingData$.outcome)

    preds$.fitted <- predict.train(class_model, type = 'raw')
    preds$probs <- predict.train(class_model, type = 'prob')

    data <-
      cbind(as.data.frame(preds[c('.observation',
                                  '.outcome',
                                  '.fitted')]),
            preds$probs)


    predx(data = as_tibble(data),
          classes = names(preds$probs),
          type = if(ncol(preds$probs) == 2) 'binary' else 'multi_class',
          prediction = 'train')

  }

#' @rdname predict_class_train

  predict_reg_train <- function(reg_model) {

    stopifnot(is_caretx(reg_model))

    preds <- list()

    preds$.outcome <- reg_model$trainingData$.outcome
    preds$.observation <- 1:length(reg_model$trainingData$.outcome)

    preds$.fitted <- predict.train(reg_model)

    predx(data = as_tibble(preds[c('.observation',
                                   '.outcome',
                                   '.fitted')]),
          classes = NULL,
          type = 'regression',
          prediction = 'train')

  }

#' @rdname predict_class_train

  predict_class_cv <- function(class_model) {

    stopifnot(any(class(class_model) == 'caretx'))

    classes <- levels(class_model$pred$obs)

    .observation <- NULL
    .outcome <- NULL
    .fitted <- NULL
    .resample <- NULL

    rowIndex <- NULL
    obs <- NULL
    pred <- NULL
    Resample <- NULL

    data <- mutate(class_model$pred,
                   .observation = rowIndex,
                   .outcome = obs,
                   .fitted = pred,
                   .resample = Resample)

    data <- dplyr::arrange(data, .observation)
    data <- as_tibble(data)

    predx(data = data[c('.observation',
                        '.outcome',
                        '.fitted',
                        '.resample',
                        classes)],
          classes = classes,
          type = if(length(classes) == 2) 'binary' else 'multi_class',
          prediction = 'cv')

  }

#' @rdname predict_class_train

  predict_reg_cv <- function(reg_model) {

    stopifnot(any(class(reg_model) == 'caretx'))

    .observation <- NULL
    .outcome <- NULL
    .fitted <- NULL
    .resample <- NULL

    rowIndex <- NULL
    obs <- NULL
    pred <- NULL
    Resample <- NULL

    data <- mutate(reg_model$pred,
                   .observation = rowIndex,
                   .outcome = obs,
                   .fitted = pred,
                   .resample = Resample)

    data <- dplyr::arrange(data, .observation)
    data <- as_tibble(data)

    predx(data = data[c('.observation',
                        '.outcome',
                        '.fitted',
                        '.resample')],
          classes = NULL,
          type = 'regression',
          prediction = 'cv')

  }

#' @rdname predict_class_train

  predict_class_test <- function(class_model, newdata) {

    ## finding the outcome variable

    outcome_var <- as.character(formula(class_model)[[2]])

    preds <- list()

    preds$.outcome <- newdata[[outcome_var]]
    preds$.observation <- 1:length(newdata[[outcome_var]])

    preds$.fitted <- predict.train(class_model,
                                   newdata = newdata,
                                   type = 'raw')

    preds$probs <- predict.train(class_model,
                                 newdata = newdata,
                                 type = 'prob')

    data <- cbind(as.data.frame(preds[c('.observation',
                                        '.outcome',
                                        '.fitted')]),
                  preds$probs)

    predx(data = as_tibble(data),
          classes = names(preds$probs),
          type = if(ncol(preds$probs) == 2) 'binary' else 'multi_class',
          prediction = 'test')


  }

#' @rdname predict_class_train

  predict_reg_test <- function(reg_model, newdata) {

    ## finding the outcome variable

    outcome_var <- as.character(formula(reg_model)[[2]])

    preds <- list()

    preds$.outcome <- newdata[[outcome_var]]
    preds$.observation <- 1:length(newdata[[outcome_var]])

    preds$.fitted <- predict.train(reg_model, newdata = newdata)

    predx(data = as_tibble(preds[c('.observation',
                                   '.outcome',
                                   '.fitted')]),
          classes = NULL,
          type = 'regression',
          prediction = 'test')

  }

# END -----
