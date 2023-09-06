# Calibration models for predictions and models

#' Calibrate predictions and models.
#'
#' @description
#' Enables post-hoc quantile GAM calibration of the regression predictions
#' and models using the \code{\link[qgam]{qgam}} tool.
#'
#' @details
#' Currently, only quantile calibration for regression `predx` and `caretx`
#' objects is implemented. If a vector of quantiles is provided, the optimal
#' one is choosen based on the minimum RMSE
#' (more formal criteria are in development).
#'
#' @param x a \code{\link{caretx}} model or \code{\link{predx}} prediction
#' object.
#' @param newdata a data frame with the test data.#'
#' @param bs basis function for the smoother, ignored if a formula provided.
#' @param k degrees of freedom for the smoother, ignored if a formula provided.
#' @param qu quantile or quantiles for the calibration,
#' see: \code{\link[qgam]{qgam}} for details.
#' @param form optional GAM formula as specified by
#' \code{\link[mgcv]{formula.gam}}. The uncalibrated predictions are stored
#' internally in the '.raw' variable, which needs to be included in the
#' user-provided formula.
#' @param lsig the value of the log learning rate used to create the
#' Gibbs posterior, see: \code{\link[qgam]{qgam}} for details.
#' @param err an upper bound on the error of the estimated quantile curve,
#' see: \code{\link[qgam]{qgam}} for details.
#' @param control a list of control parameters passed to
#' \code{\link[qgam]{qgam}}.
#' @param argGam  a list of parameters to be passed to
#' \code{\link[mgcv]{gam}}, with the exception of formula, family and data.
#' @param ... extra arguments passed to \code{\link[qgam]{qgam}}.
#'
#' @return A list with the `predx` object
#' (.raw stores the uncalibrated predctions, .fitted stores the calibrated
#' predictions) along with the gamObject named `cal_fit`,
#' the chosen quantile value (`qu`) and values of explained deviance (`qu_tbl`).
#' @export calibration.predx
#' @export

  calibration.predx <- function(x,
                                bs = 'cr',
                                k = 20,
                                qu = 0.5,
                                form = NULL,
                                lsig = NULL,
                                err = NULL,
                                control = list(link = 'identity'),
                                argGam = NULL, ...) {

    ## entry control --------

    stopifnot(is_predx(x))

    stopifnot(is.numeric(k))
    stopifnot(is.numeric(qu))

    if(!is.null(form) & !rlang::is_formula(form)) {

      stop('form needs to be a formula.', call = FALSE)

    }

    if(any(qu < 0) | any(qu > 1)) {

      stop('The qu argument in range [0,1] required.', call. = FALSE)

    }

    if(x$type != 'regression') {

      warning('Currently, calibration is implemented only for regression objects.',
              call. = FALSE)

      return(NULL)

    }

    .raw <- NULL
    .fitted <- NULL
    order <- NULL
    rmse <- NULL

    ## extracting the predictions and formula --------

    pred <- mutate(components(x, 'data'),
                   .raw = .fitted)

    if(is.null(form)) {

      form <- paste0(".outcome ~ s(.raw, bs = '",
                     bs, "', k = ", k, ")")

      form <- stats::as.formula(form)

    }

    ## calibration -------

    cal_fit <- map(qu,
                   ~qgam::qgam(form = form,
                               data = pred,
                               qu = .x,
                               lsig = lsig,
                               err = err,
                               control = control,
                               argGam = argGam, ...))

    rmse_fit <-
      map_dbl(cal_fit, ~sqrt(mean((.x$fitted.values - .x$y)^2, na.rm = TRUE)))

    qu_tbl <- tibble(order = 1:length(qu),
                     qu = qu,
                     rmse = rmse_fit)

    opt_qu <- filter(qu_tbl, rmse == min(rmse, na.rm = TRUE))

    cal_fit <- cal_fit[[opt_qu$order[1]]]

    pred <- mutate(pred, .fitted = predict(cal_fit))

    opt_qu <- opt_qu$qu[1]

    ## output

    list(cal_fit = cal_fit,
         qu = opt_qu,
         qu_tbl = qu_tbl,
         predx_object = predx(data = pred,
                              classes = x$classes,
                              type = x$type,
                              prediction = x$prediction))

  }

#' @rdname calibration.predx
#' @export calibration.caretx
#' @export

  calibration.caretx <- function(x,
                                 newdata = NULL,
                                 bs = 'cr',
                                 k = 20,
                                 qu = 0.5,
                                 form = NULL,
                                 lsig = NULL,
                                 err = NULL,
                                 control = list(link = 'identity'),
                                 argGam = NULL, ...) {

    ## entry control

    stopifnot(is_caretx(x))

    if(x$modelType != 'Regression') {

      warning('Calibration is currently implemented only for regression models.',
              call. = FALSE)

      return(NULL)

    }

    ## regression models

    calibrate_regression(x,
                         newdata = newdata,
                         bs = bs,
                         k = k,
                         qu = qu,
                         form = form,
                         lsig = lsig,
                         err = err,
                         control = control,
                         argGam = argGam, ...)

  }

# END -----
