# This script contains the constructor functions for the S3 caretx class.

# caretx class ------

#' Create a caretx object.
#'
#' @description converts a caret model (class 'train', see: \code{\link[caret]{train}}) to a caretx object.
#' @details The input caret model has to contain a table with the final model predictions
#' (\code{\link[caret]{trainControl}}, savePredictions = 'final'),
#' training data (\code{\link[caret]{trainControl}}, returnData  = TRUE)
#' and final model resamples raining data (\code{\link[caret]{trainControl}}, returnResamp = 'final').
#' For classification models, the final model probabilities need to be provided
#' (\code{\link[caret]{trainControl}}, classProbs = TRUE).
#' Currently designed to work only with cross-validated models (\code{\link[caret]{trainControl}},
#' method = 'cv' or 'repeatedcv'), generated with a formula.
#' @param train_model a caret model (\code{\link[caret]{train}}).
#' @return a caretx object.
#' @export

  caretx <- function(caret_model) {

    ## user entry control

    if(is.null(caret_model$trainingData)) {

      stop("Please provide a caret model with trainingData. Set caret::trainControl returnData to TRUE", call. = FALSE)

    }

    if(is.null(caret_model$pred)) {

      stop("Please provide a caret model with final model predictions. Set caret::trainControl savePredictions to 'final'", call. = FALSE)

    }

    if(is.null(caret_model$resample)) {

      stop("Please provide a caret model with final model resamples. Set caret::trainControl returnResamp to 'final'", call. = FALSE)

    }

    if(!caret_model$control$method %in% c('cv', 'repeatedcv')) {

      stop("Cross-validated model needed. Set caret::trainControl method to 'cv' or 'repeatedcv'", call. = FALSE)

    }

    if(caret_model$modelType == 'Classification') {

      check_probs <- levels(factor(caret_model$pred$pred))

      if(!all(check_probs %in% names(caret_model$pred))) {

        stop("Please provide a caret model with final model classification probs. Set caret::trainControl classProbs to TRUE", call. = FALSE)

      }

    }

    if(is.null(caret_model$call$form)) {

      stop('Provide a caret model genarated with a formula.', call. = FALSE)

    }

    structure(caret_model,
              class = c('caretx', class(caret_model)))

  }

#' Create a caretx object.
#'
#' @description converts a caret model (class 'train', see: \code{\link[caret]{train}}) to a caretx object.
#' @details The input caret model has to contain a table with the final model predictions
#' (\code{\link[caret]{trainControl}}, savePredictions = 'final'),
#' training data (\code{\link[caret]{trainControl}}, returnData  = TRUE)
#' and final model resamples raining data (\code{\link[caret]{trainControl}}, returnResamp = 'final').
#' For classification models, the final model probabilities need to be provided
#' (\code{\link[caret]{trainControl}}, classProbs = TRUE).
#' Currently designed to work only with cross-validated models (\code{\link[caret]{trainControl}},
#' method = 'cv' or 'repeatedcv'), generated with a formula.
#' @param train_model a caret model (\code{\link[caret]{train}}).
#' @return a caretx object.
#' @export

  as_caretx <- function(caret_model) {

    caretx(caret_model)

  }

# predx class -----

#' Create a predx object.
#'
#' @description creates a predx object storing extended predictions of caretx models (\code{\link{caretx}}) with information
#' concerning model type, class number and the type of prediction (training, cross-validation or test).
#' @details The type argument describes the prediction type: regression, binary or multi_class.
#' @param data data frame with predictions, the columns .outcome and .fitted are obligatory.
#' @param type model type (regression, binary or multi_class).
#' @param prediction type of prediction (training, cross-validation or test).
#' @return  a predx object.
#' @export

  predx <- function(data,
                    classes = NULL,
                    type = c('regression', 'binary', 'multi_class'),
                    prediction = c('train', 'cv', 'test')) {

    type <- match.arg(type[1], c('regression', 'binary', 'multi_class'))
    prediction <- match.arg(prediction[1], c('train', 'cv', 'test'))

    if(any(!c('.outcome', '.fitted') %in% names(data))) stop('The data has to contain .outcome and .fitted columns.')

    structure(list(data = data,
                   classes = classes,
                   type = type,
                   prediction = prediction),
              class = 'predx')

  }
