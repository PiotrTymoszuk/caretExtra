# This script provides print and plotting methods for the `predx` class.

# printing ------

#' Print a predx object.
#'
#' @description prints the heading of the prediction table.
#' @param x a `predx` object.
#' @param ... extra arguments, currently none.
#' @return nothing, called for its side effects.
#' @export

  print.predx <- function(x, ...) {

    stopifnot(is_predx(x))

    print(x$data)

  }

# END -------
