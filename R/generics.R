# This script declares generic functions

#' Extract features of an object
#'
#' @description S3 generic functions used to extract object-specific properties or other features
#' @return The requested feature.
#' @export

  extract <- function(x, ...) {

    UseMethod('extract', x)

  }

#' Extract numbers of complete observations
#'
#' @description S3 generic functions used to extract the numbers of complete observations
#' @return The number of complete observations.
#' @export

  nobs <- function(x, ...) {

    UseMethod('nobs', x)

  }

#' Confusion matrix.
#'
#' @description S3 generic function used to create confusion matrices.
#' @return a table object representing a confusion matrix.
#' @export

  confusion <- function(x, ...) {

    UseMethod('confusion', x)

  }
