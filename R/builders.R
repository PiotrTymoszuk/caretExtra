# This script contains the constructor functions for the S3 caretx class.

# caretx class ------

#' Create a `caretx` object.
#'
#' @description
#' Converts a caret model (class \code{\link[caret]{train}}) to
#' a `caretx` object.
#'
#' @details
#' Several parameters need to be set to enable conversion of a caret model
#' to a `caretx` object; they are done by providing a \code{\link[caret]{trainControl}}
#' object to \code{\link[caret]{train}} function.
#' The input caret model has to contain a table with the final model
#' predictions (`trainControl(savePredictions = 'final')`),
#' training data (`trainControl(returnData  = TRUE)`)
#' and final model re-sample data (`trainControl(returnResamp = 'final')`).
#' For classification models, the final model probabilities need to be provided
#' (`trainControl(classProbs = TRUE)`).
#' Currently designed to work only with cross-validated models
#' (`trainControl(method = 'cv')` or `trainControl(method = 'cv')`) generated
#' with a formula.
#' Of note, in case of binary classifiers, the 'positive' outcome is
#' defined by the second level of the outcome variable.
#' I.e. if `outcome <- factor(x, c('a', 'b'))`, the level `b` is considered as
#' the event of interest.
#'
#' @references Kuhn M. Building predictive models in R using the caret
#' package. J Stat Softw (2008) 28:1–26. doi:10.18637/jss.v028.i05
#'
#' @param caret_model a caret model (class \code{\link[caret]{train}}).
#' @param x an object.
#'
#' @return a `caretx` object; `is_caretx()` returns a logical value.
#'
#' @export

  caretx <- function(caret_model) {

    if(is_caretx(caret_model)) return(caret_model)

    ## user entry control -------

    if(is.null(caret_model$trainingData)) {

      stop(paste("Please provide a caret model with trainingData.",
                 "Set caret::trainControl returnData to TRUE"),
           call. = FALSE)

    }

    if(is.null(caret_model$pred)) {

      stop(paste("Please provide a caret model with final model",
                 "predictions. Set caret::trainControl savePredictions",
                 "to 'final'"),
           call. = FALSE)

    }

    if(is.null(caret_model$resample)) {

      stop(paste("Please provide a caret model with final model resamples.",
                 "Set caret::trainControl returnResamp to 'final'"),
           call. = FALSE)

    }

    if(!caret_model$control$method %in% c('cv', 'repeatedcv')) {

      stop(paste("Cross-validated model needed. Set caret::trainControl",
                 "method to 'cv' or 'repeatedcv'"),
           call. = FALSE)

    }

    if(caret_model$modelType == 'Classification') {

      check_probs <- levels(factor(caret_model$pred$pred))

      if(!all(check_probs %in% names(caret_model$pred))) {

        stop(paste("Please provide a caret model with final model",
                   "classification probs. Set caret::trainControl",
                   "classProbs to TRUE"),
             call. = FALSE)

      }

    }

    if(is.null(caret_model$call$form)) {

      stop('Provide a caret model genarated with a formula.',
           call. = FALSE)

    }

    ## output --------

    structure(caret_model,
              class = c('caretx', class(caret_model)))

  }

#' @rdname caretx
#' @export

  as_caretx <- function(caret_model) caretx(caret_model)

#' @rdname caretx
#' @export

  is_caretx <- function(x) inherits(x, 'caretx')


# predx class -----

#' Create a `predx` object.
#'
#' @description
#' Creates a `predx` object storing extended predictions of
#' \code{\link{caretx}} models with information
#' concerning model type, class number and the type of prediction
#' (training, cross-validation, or test).
#'
#' @details The `type` argument describes the prediction type:
#' `regression`, `binary` or `multi_class`.
#'
#' @references Kuhn M. Building predictive models in R using the caret
#' package. J Stat Softw (2008) 28:1–26. doi:10.18637/jss.v028.i05
#'
#' @param data a data frame with predictions, the
#' columns `.outcome` and `.fitted` are obligatory.
#' @param classes an character vector with class names, required only
#' for binary and multi-class classification models.
#' @param type model type (`regression`, `binary` or `multi_class`).
#' @param prediction type of prediction (`training`, `cv` or `test`).
#' @param calibrator_fun optional and experimental, a function that takes the
#' data frame provided as `data` argument and returns calibrated predictions in
#' a data frame with the same format as `data`.
#'
#' @return  a `predx` object; `is_predx()` returns a logical value.
#'
#' @export

  predx <- function(data,
                    classes = NULL,
                    type = c('regression', 'binary', 'multi_class'),
                    prediction = c('train', 'cv', 'test'),
                    calibrator_fun = NULL) {

    type <- match.arg(type[1],
                      c('regression', 'binary', 'multi_class'))

    prediction <- match.arg(prediction[1],
                            c('train', 'cv', 'test'))

    if(any(!c('.outcome', '.fitted') %in% names(data))) {

      stop('The data has to contain .outcome and .fitted columns.',
           call. = FALSE)

    }

    if(type %in% c('binary', 'multi_class')) {

      if(is.null(classes)) {

        stop(paste("'classes' - a vector with class",
                   "labels - is required for classification models."),
             call. = FALSE)

      }

    }

    if(!is.null(calibrator_fun)) {

      if(!is.function(calibrator_fun)) {

        stop("'calibrator_fun' need s to be NULL or a function.",
             call. = FALSE)

      }

    }

    structure(list(data = data,
                   classes = classes,
                   type = type,
                   prediction = prediction,
                   calibrator_fun = calibrator_fun),
              class = 'predx')

  }

#' @rdname predx
#' @export

  is_predx <- function(x) inherits(x, 'predx')

# END ------
