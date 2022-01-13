# This script contains functions for calculation of residuals and error statistic

# residuals, correlations, fit errors and Rsquared for regression data -----

#' Calculate residuals.
#'
#' @description calculates working residuals.
#' @param data a data frame with .outcome and .fitted columns.
#' @return a numeric vector of residuals.

  get_resids <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    ## working residuals

    data[['.fitted']] - data[['.outcome']]

  }

#' Calculate outcome variance.
#'
#' @description calculates outcome variance.
#' @param data a data frame with .outcome and .fitted columns.
#' @return numeric, outcome variance.

  get_var <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    stats::var(data[['.outcome']])

  }

#' Calculate fit errors.
#'
#' @description calculates MAE, MSE, RMSE and pseudo R squared based on MSE and outcome variance.
#' @param data a data frame with .outcome and .fitted columns.
#' @return a tibble with the statistic values.

  get_errors <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    tibble::tibble(statistic = c('MAE', 'MSE', 'RMSE', 'rsq'),
                   estimate = c(mean(abs(caretExtra:::get_resids(data))),
                                mean(caretExtra:::get_resids(data)^2),
                                sqrt(mean(caretExtra:::get_resids(data)^2)),
                                1 - (mean(caretExtra:::get_resids(data)^2)/caretExtra:::get_var(data))),
                   lower_ci = NA,
                   upper_ci = NA)

  }

#' Calculate correlations between the fitted and outcome values.
#'
#' @description calculates Pearson, Spearman and Kendal TauB correlations between the fitted and outcome values.
#' @param data a data frame with .outcome and .fitted columns.
#' @param ci logical, should 95\% confidence intervals be calculated?
#' @return a tibble with the statistic values.

  get_correlations <- function(data, ci = TRUE) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    ## correlations of the fitted and outcome values

    ## Correlations need to be done in the safely() mode, since model convergence errors such
    ## as constant values predicted will result in correlation errors as well.

    correlations <- list(pearson = function(x, y) stats::cor.test(x, y, method = 'pearson', conf.level = 0.95),
                         spearman = function(x, y) DescTools::SpearmanRho(x, y, conf.level = if(ci) 0.95 else NA),
                         kendall = function(x, y) DescTools::KendallTauB(x, y, conf.level = if(ci) 0.95 else NA))

    correlations <- purrr::map(correlations,
                               ~purrr::safely(.x)(data[['.outcome']], data[['.fitted']]))

    correlations <- purrr::transpose(correlations)$result

    correlations$pearson <- c(rho = correlations$pearson$estimate,
                              lwr.ci = correlations$pearson$conf.int[1],
                              lwr.ci = correlations$pearson$conf.int[2])

    correlations <- purrr::map(correlations,
                               function(x) if(is.null(x)) c(NA, NA, NA) else x)

    purrr::map2_dfr(correlations,
                    names(correlations),
                    ~tibble::tibble(statistic = .y,
                                    estimate = unlist(.x[1]),
                                    lower_ci = if(ci) unlist(.x[2]) else NA,
                                    upper_ci = if(ci) unlist(.x[3]) else NA))

  }

# Classification errors for binary and multi_class classifiers -------

#' Calculates classification error.
#'
#' @description Calculates simple percentage of correct prediction and failures.
#' @param data a data frame with .outcome and .fitted columns.
#' @return a tibble with the statistic values.

  get_class_error <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    data <- dplyr::mutate(data,
                          success = .data[['.fitted']] == .data[['.outcome']])

    tibble::tibble(statistic = c('class_error', 'correct_rate'),
                   estimate = c(1 - mean(data[['success']]),
                                mean(data[['success']])),
                   lower_ci = NA,
                   upper_ci = NA)

  }

# ROC statistic, optimized ROC statistic, kappa and C index form binary classifiers ------

#' Calculate Harrell's C index.
#'
#' @description  Calculates Harrell's C, a wrapper around \code{\link[survival]{concordance}}.
#' @param data a data frame with .outcome and .fitted columns.
#' @return a tibble with the statistic values.

  get_concordance <- function(data, ci = TRUE) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    c_object <- survival::concordance(.outcome ~ .fitted, data = data)

    tibble::tibble(statistic = 'c_index',
                   estimate = c_object$concordance,
                   lower_ci = if(!ci) NA else c_object$concordance + sqrt(c_object$var) * qnorm(0.025),
                   upper_ci = if(!ci) NA else c_object$concordance + sqrt(c_object$var) * qnorm(0.975))

  }

#' Calculate Cohen's Kappa.
#'
#' @description  Calculates Cohen's Kappa, a wrapper around \code{\link[vcd]{Kappa}}.
#' @param data a data frame with .outcome and .fitted columns.
#' @return a tibble with the statistic values.

  get_kappa <- function(data, ci = TRUE) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    kappa_object <- vcd::Kappa(table(data[c('.outcome', '.fitted')]))

    tibble::tibble(statistic = 'kappa',
                   estimate = kappa_object[[1]][1],
                   lower_ci = if(!ci) NA else kappa_object[[1]][1] + kappa_object[[1]][2] * qnorm(0.025),
                   upper_ci = if(!ci) NA else kappa_object[[1]][1] + kappa_object[[1]][2] * qnorm(0.975))

  }

#' Calculates simple receiver-operating characteristic.
#'
#' @description A wrapper around \code{\link[caret]{twoClassSummary}}.
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with the class order.
#' @return a tibble with the statistic values.

  get_roc <- function(data, classes) {

    stopifnot(all(c('.outcome', '.fitted', classes) %in% names(data)))

    roc_res <- caret::twoClassSummary(as.data.frame(dplyr::mutate(data,
                                                                  pred = .fitted,
                                                                  obs = .outcome)),
                                      lev = classes)

    tibble::tibble(statistic = c('AUC', 'Se', 'Sp'),
                   estimate = unname(roc_res),
                   lower_ci = NA,
                   upper_ci = NA)

  }

#' Binary classifier residuals.
#'
#' @description Retrieves working binary residuals.
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with the class order.
#' @return a tibble with the statistic values.

  get_resids_binary <- function(data, classes) {

    stopifnot(all(c('.outcome', '.fitted', classes) %in% names(data)))

    if(is.factor(data[['.outcome']])) {

      outcome <- as.numeric(data[['.outcome']]) - 1

    } else {

      outcome <- data[['.outcome']]

    }

    data[[classes[2]]] - outcome

  }

#' Binary classifier outcome variance.
#'
#' @description calculates outcome variance.
#' @param data a data frame with .outcome and .fitted columns.
#' @return numeric, outcome variance.

  get_var_binary <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    if(is.factor(data[['.outcome']])) {

      outcome <- as.numeric(data[['.outcome']]) - 1

    } else {

      outcome <- data[['.outcome']]

    }

    stats::var(outcome)

  }

#' Calculate binary fit errors.
#'
#' @description calculates MAE, MSE, RMSE and pseudo R squared based on MSE and outcome variance.
#' @param data a data frame with .outcome and .fitted columns.
#' @return a tibble with the statistic values.

  get_errors_binary <- function(data, classes) {

    stopifnot(all(c('.outcome', '.fitted', classes) %in% names(data)))

    tibble::tibble(statistic = c('MAE', 'MSE', 'RMSE', 'rsq'),
                   estimate = c(mean(abs(caretExtra:::get_resids_binary(data, classes))),
                                mean(caretExtra:::get_resids_binary(data, classes)^2),
                                sqrt(mean(caretExtra:::get_resids_binary(data, classes)^2)),
                                1 - (mean(caretExtra:::get_resids_binary(data, classes)^2)/caretExtra:::get_var_binary(data))),
                   lower_ci = NA,
                   upper_ci = NA)

  }

# Ready to use analysis functions -----

#' Analyze regression fit.
#'
#' @description computes fit errors and fit:outcome correlations.
#' @param data a data frame with .outcome and .fitted columns.
#' @param ci should 95\% CI be computed?
#' @return a tibble with the statistic values.

  analyze_reg <- function(data, ci = FALSE) {

    rbind(caretExtra:::get_errors(data),
          caretExtra:::get_correlations(data, ci = ci))

  }

#' Analyze binary fit.
#'
#' @description computes fit errors, C-index, kappa and ROC for binary classification.
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with classes of the outcome.
#' @return a tibble with the statistic values.

  analyze_binary <- function(data, classes, ci = FALSE) {

    rbind(caretExtra:::get_errors_binary(data, classes = classes),
          caretExtra:::get_class_error(data),
          caretExtra:::get_concordance(data, ci = ci),
          caretExtra:::get_kappa(data, ci = ci),
          caretExtra:::get_roc(data, classes = classes))

  }

#' Analyze multi-class fit.
#'
#' @description computes accuracy and kappa.
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with classes of the outcome.
#' @return a tibble with the statistic values.

  analyze_multi <- function(data, ci = FALSE) {

    rbind(caretExtra:::get_class_error(data),
          caretExtra:::get_kappa(data, ci = ci))

  }

# Solutions for cross validation -----

#' Calculate mean with 95\% confidence interval
#'
#' @param vector a numeric vector
#' @return a data frame with mean, lower CI and upper CI values

  get_ci <- function(vector, method = c('norm', 'percentile', 'bca')) {

    method <- match.arg(method[1], c('norm', 'percentile', 'bca'))

    ci_vec <- switch(method,
                     norm = c(mean(vector, na.rm = TRUE) + stats::sd(vector, na.rm = TRUE) * qnorm(0.025),
                              mean(vector, na.rm = TRUE) + stats::sd(vector, na.rm = TRUE) * qnorm(0.975)),
                     percentile = stats::quantile(vector, c(0.025, 0.975), na.rm = TRUE),
                     bca = coxed::bca(vector, conf.level = 0.95))

    stat_tbl <- as.list(rlang::set_names(ci_vec, c('lower_ci', 'upper_ci')))
    stat_tbl$estimate <- mean(vector, na.rm = TRUE)

    tibble::as_tibble(stat_tbl[c('estimate', 'lower_ci', 'upper_ci')])

  }

#' Get CV prediction statistics.
#'
#' @description computes fit statistics for each CV fold and returns the statistics' means with 95\% confidence intervals (CI).
#' @param data a data frame with .outcome, .fitted and .resample columns.
#' @param fun analysis function.
#' @param ci_method method used for 95\% CI calculation.
#' @param ... additional arguments passed to the analysis function.
#' @return a tibble with the statistic values.

  analyze_cv <- function(data, fun, ci_method = c('norm', 'percentile', 'bca'), ...) {

    ci_method <- match.arg(ci_method[1], c('norm', 'percentile', 'bca'))

    splits <- plyr::dlply(data, '.resample')

    split_stats <- purrr::map(splits, fun, ...)

    est_tbl <- purrr::map2_dfc(split_stats,
                               names(split_stats),
                               ~rlang::set_names(.x['estimate'], .y))

    summ_stats <- purrr::map_dfr(1:nrow(est_tbl),
                                 ~caretExtra:::get_ci(unlist(est_tbl[.x, ]), method = ci_method))

    summ_stats <- dplyr::mutate(summ_stats, statistic = split_stats[[1]][['statistic']])

    summ_stats[c('statistic', 'estimate', 'lower_ci', 'upper_ci')]

  }

