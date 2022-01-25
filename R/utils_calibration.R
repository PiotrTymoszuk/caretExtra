# Provides calibration tools.

#' Calibrate a regression caretx model.
#'
#' @description Enables post-hoc quantile GAM calibration of the ML predictions using the
#' \code{\link[qgam]{qgam}} tool.
#' @details The calibration is developed for the training data set and applied by prediction
#' to the CV and, if newdata provided, to the test data.
#' @param caretx_model caretx model.
#' @param newdata test data set.
#' @param bs basis function for the smoother, ignored if a formula provided.
#' @param k degrees of freedom for the smoother, ignored if a formula provided..
#' @param qu quantile for the calibration, see: \code{\link[qgam]{qgam}} for details.
#' @param form GAM formula as specified by \code{\link[mgcv]{formula.gam}}. The uncalibrated predictions
#' are stored internally in the '.raw' variable, which needs to be included in the user-provided formula.
#' @param lsig the value of the log learning rate used to create the Gibbs posterior,
#' see: \code{\link[qgam]{qgam}} for details.
#' @param err an upper bound on the error of the estimated quantile curve,
#' see: \code{\link[qgam]{qgam}} for details.
#' @param control a list of control parameters passed to \code{\link[qgam]{qgam}}.
#' @param argGam  a list of parameters to be passed to \code{\link[mgcv]{gam}},
#' with the exception of formula, family and data.
#' @param ... extra arguments passed to \code{\link[qgam]{qgam}}.
#' @return a list of predx objects (train, cv and, if newdata provided, test) along with the gamObject named cal_fit,
#' the chosen quantile value (qu) and values of explained deviance (qu_tbl).

  calibrate_regression <- function(caretx_model,
                                   newdata = NULL,
                                   bs = 'cr',
                                   k = 20,
                                   qu = 0.5,
                                   form = NULL,
                                   lsig = NULL,
                                   err = NULL,
                                   control = list(link = 'identity'),
                                   argGam = NULL, ...) {

    # entry control

    stopifnot(any(class(caretx_model) == 'caretx'))

    if(!is.null(newdata)) {

      if(!any(class(newdata) == 'data.frame')) stop('newdata has to be a data frame or tibble.', call. = FALSE)

    }

    stopifnot(is.numeric(k))
    stopifnot(is.numeric(qu))

    if(!is.null(form) & !rlang::is_formula(form)) stop('form needs to be a formula.', call = FALSE)

    if(any(qu < 0) | any(qu > 1)) stop('The qu argument in range [0,1] required.', call. = FALSE)


    ## obtaining the predictions
    ## the .raw variable stores the 'raw' RF predictions

    preds <- predict(caretx_model,
                     newdata = newdata,
                     plain = FALSE)

    preds <- purrr::compact(preds)

    pred_data <- purrr::map(preds,
                            caretExtra::extract,
                            what = 'data')

    pred_data <- purrr::map(pred_data,
                            dplyr::mutate,
                            .raw = .fitted)

    ## calibration for the training data set

    train_fit <- caretExtra::calibration.predx(preds$train,
                                               bs = bs,
                                               k = k,
                                               qu = qu,
                                               form = form,
                                               lsig = lsig,
                                               err = err,
                                               control = control,
                                               argGam = argGam, ...)

    ## calibrated predictions

    new_preds <- purrr::map(pred_data,
                            ~predict(train_fit$cal_fit,
                                     newdata = .x))

    new_preds <- purrr::map2(pred_data,
                             new_preds,
                             ~dplyr::mutate(.x, .fitted = .y))

    ## output as a list of predx objects and the fit

    new_preds <- purrr::pmap(list(data = new_preds,
                                  prediction = names(new_preds)),
                             caretExtra::predx,
                             classes = NULL,
                             type = 'regression')

    c(train_fit[c('cal_fit', 'qu', 'qu_tbl')],
      new_preds)

  }
