# Utilities for case-wise aggregation of out-of-fold predictions

# nodal voting ----------

#' Nodal voting: outcome with the highest probability.
#'
#' @description
#' Nodal voting: the function returns the index of a numeric vector, which
#' corresponds to its maximum.
#' Ties are resolved at random with a warning.
#' For matrices, the voting is performed in a row-wise manner.
#'
#' @param x a numeric vector or a matrix.
#' @param ... arguments passed to the methods, none at the moment.
#'
#' @return an integer value: the index number.
#'
#' @export

  nodal_voting <- function(x, ...) UseMethod("nodal_voting")

#' @rdname nodal_voting
#' @export

  nodal_voting.default <- function(x, ...) {

    stopifnot(is.numeric(x))

    if(length(x) == 1) return(1)

    if(all(is.na(x))) return(NA)

    ## handling of NAs they are replaced by the minimum

    if(any(is.na(x))) {

      pad <- min(x, na.rm = TRUE)

      x <- ifelse(is.na(x), pad, x)

    }

    idx <- which(x == max(x))

    if(length(idx) == 1) return(idx)

    warning("Ties present and resolved at random")

    return(sample(idx, size = 1, replace = FALSE))

  }

#' @rdname nodal_voting
#' @export

  nodal_voting.matrix <- function(x, ...) {

    stopifnot(is.matrix(x))

    map_dbl(1:nrow(x), ~nodal_voting(x[.x, , drop = TRUE]))

  }

# END ---------
