# Helper functions

# Prediction -------

#' Predict train data outcome for classification models.
#'
#' @description a helper function extracting the predictions for the train data set from
#' a classification caretx model.
#' @return a predx object.

  predict_class_train <- function(class_model) {

    stopifnot(any(class(class_model) == 'caretx'))

    preds <- list()

    preds$.outcome <- class_model$trainingData$.outcome
    preds$.observation <- 1:length(class_model$trainingData$.outcome)

    preds$.fitted <- caret::predict.train(class_model, type = 'raw')
    preds$probs <- caret::predict.train(class_model, type = 'prob')

    data <- tibble::as_tibble(cbind(as.data.frame(preds[c('.observation', '.outcome', '.fitted')]),
                                    preds$probs))


    caretExtra::predx(data = data,
                      classes = names(preds$probs),
                      type = if(ncol(preds$probs) == 2) 'binary' else 'multi_class',
                      prediction = 'train')

  }

#' Predict train data outcome for regression models.
#'
#' @description a helper function extracting the predictions for the train data set from
#' a regression caretx model.
#' @return a predx object.

  predict_reg_train <- function(reg_model) {

    stopifnot(any(class(reg_model) == 'caretx'))

    preds <- list()

    preds$.outcome <- reg_model$trainingData$.outcome
    preds$.observation <- 1:length(reg_model$trainingData$.outcome)

    preds$.fitted <- caret::predict.train(reg_model)

    caretExtra::predx(data = tibble::as_tibble(preds[c('.observation', '.outcome', '.fitted')]),
                      classes = NULL,
                      type = 'regression',
                      prediction = 'train')

  }

#' Predict cross-validation data outcome for classification models.
#'
#' @description a helper function extracting the predictions for the cross-validation data set from
#' a classification caretx model.
#' @return a predx object.
#' @export

  predict_class_cv <- function(class_model) {

    stopifnot(any(class(class_model) == 'caretx'))

    classes <- levels(class_model$pred$obs)

    data <- dplyr::mutate(class_model$pred,
                          .observation = rowIndex,
                          .outcome = obs,
                          .fitted = pred,
                          .resample = Resample)

    data <- dplyr::arrange(data, .observation)
    data <- tibble::as_tibble(data)

    caretExtra::predx(data = data[c('.observation',
                                    '.outcome',
                                    '.fitted',
                                    '.resample',
                                    classes)],
                      classes = classes,
                      type = if(length(classes) == 2) 'binary' else 'multi_class',
                      prediction = 'cv')

  }

#' Predict cross-validation data outcome for regression models.
#'
#' @description a helper function extracting the predictions for the cross-validation data set from
#' a regression caretx model.
#' @return a predx object.

  predict_reg_cv <- function(reg_model) {

    stopifnot(any(class(reg_model) == 'caretx'))

    data <- dplyr::mutate(reg_model$pred,
                          .observation = rowIndex,
                          .outcome = obs,
                          .fitted = pred,
                          .resample = Resample)

    data <- dplyr::arrange(data, .observation)
    data <- tibble::as_tibble(data)

    caretExtra::predx(data = data[c('.observation',
                                    '.outcome',
                                    '.fitted',
                                    '.resample')],
                      classes = NULL,
                      type = 'regression',
                      prediction = 'cv')

  }

#' Predict test data outcome for classification models.
#'
#' @description a helper function extracting the predictions for the test data set from
#' a classification caretx model.
#' @return a predx object.

  predict_class_test <- function(class_model, newdata) {

    ## finding the outcome variable

    outcome_var <- as.character(eval(class_model$call$form))[2]

    preds <- list()

    preds$.outcome <- newdata[[outcome_var]]
    preds$.observation <- 1:length(newdata[[outcome_var]])

    preds$.fitted <- caret::predict.train(class_model,
                                          newdata = newdata,
                                          type = 'raw')

    preds$probs <- caret::predict.train(class_model,
                                        newdata = newdata,
                                        type = 'prob')

    data <- tibble::as_tibble(cbind(as.data.frame(preds[c('.observation', '.outcome', '.fitted')]),
                                    preds$probs))

    caretExtra::predx(data = data,
                      classes = names(preds$probs),
                      type = if(ncol(preds$probs) == 2) 'binary' else 'multi_class',
                      prediction = 'test')


  }

#' Predict test data outcome for regression models.
#'
#' @description a helper function extracting the predictions for the test data set from
#' a regression caretx model.
#' @return a predx object.

  predict_reg_test <- function(reg_model, newdata) {

    ## finding the outcome variable

    outcome_var <- as.character(eval(reg_model$call$form))[2]

    preds <- list()

    preds$.outcome <- newdata[[outcome_var]]
    preds$.observation <- 1:length(newdata[[outcome_var]])

    preds$.fitted <- caret::predict.train(reg_model,
                                          newdata = newdata)

    caretExtra::predx(data = tibble::as_tibble(preds[c('.observation', '.outcome', '.fitted')]),
                      classes = NULL,
                      type = 'regression',
                      prediction = 'test')


  }
