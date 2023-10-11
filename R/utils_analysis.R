# This script contains functions for calculation of residuals and error statistic

# residuals, correlations, fit errors and Rsquared for regression data -----

#' Calculate residuals, variance, errors and correlation of the outcome and fit.
#'
#' @description
#' Calculates working residuals (`get_resids()`),
#' variance of the outcome (`get_var()`),
#' fir errors based on MSE and outcome variance
#' (`get_errors()`: MAE, MSE, RMSE and pseudo-R-squared),
#' and correlation between the outcome and fitted values
#' (`get_correlations()`: Pearson, Spearman and Kendal TauB).
#'
#' @param data a data frame with .outcome and .fitted columns.
#' @param ci logical, should 95% confidence intervals be calculated?
#'
#' @return a numeric vector of residuals or a tibble with statistic values.

  get_resids <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    ## working residuals

    data[['.fitted']] - data[['.outcome']]

  }

#' @rdname get_resids

  get_var <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    stats::var(data[['.outcome']])

  }

#' @rdname get_resids

  get_errors <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    resamplCor <-
      try(stats::cor(data[['.outcome']], data[['.fitted']],
                     use = 'pairwise.complete.obs'),
          silent = TRUE)

    resids <- get_resids(data)

    tibble(statistic = c('MAE', 'MSE', 'RMSE', 'rsq', 'caret_rsq'),
           estimate = c(mean(abs(resids)),
                        mean(resids^2),
                        sqrt(mean(resids^2)),
                        1 - (mean(resids^2)/get_var(data)),
                        resamplCor^2),
           lower_ci = NA,
           upper_ci = NA)

  }

#' @rdname get_resids

  get_correlations <- function(data, ci = TRUE) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    ## correlations of the fitted and outcome values

    ## Correlations need to be done in the safely() mode,
    ## since model convergence errors such
    ## as constant values predicted will result in
    ## correlation errors as well.

    correlations <-
      list(pearson = function(x, y) stats::cor.test(x, y, method = 'pearson', conf.level = 0.95),
           spearman = function(x, y) DescTools::SpearmanRho(x, y, conf.level = if(ci) 0.95 else NA),
           kendall = function(x, y) c(stats::cor(x, y, method = 'kendall'), NA, NA))

    correlations <- map(correlations,
                        ~safely(.x)(data[['.outcome']], data[['.fitted']]))

    correlations <- purrr::transpose(correlations)$result

    if(!is.null(correlations$pearson)) {

      correlations$pearson <- c(rho = correlations$pearson$estimate,
                                lwr.ci = correlations$pearson$conf.int[1],
                                lwr.ci = correlations$pearson$conf.int[2])

    } else {

      correlations$pearson <- c(rho = NA,
                                lwr.ci = NA,
                                lwr.ci = NA)

    }

    map2_dfr(correlations,
             names(correlations),
             ~tibble(statistic = .y,
                     estimate = unlist(.x[1]),
                     lower_ci = if(ci) unlist(.x[2]) else NA,
                     upper_ci = if(ci) unlist(.x[3]) else NA))

  }

# ROC for binary and multi-class classifers ---------

#' Receiver-operating characteristic.
#'
#' @description A wrapper around \code{\link[caret]{twoClassSummary}} and
#' \code{\link[caret]{multiClassSummary}}.
#'
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with the class order.
#'
#' @details
#' For multi-class predictions, stats referring to class detection
#' (such as sensitivity or specificity) are averaged over all categories.
#'
#'
#' @return a tibble with the statistic values.

  get_roc <- function(data, classes) {

    stopifnot(all(c('.outcome', '.fitted', classes) %in% names(data)))

    pred <- NULL
    obs <- NULL
    .fitted <- NULL
    .outcome <- NULL

    roc_res <-
      as.data.frame(mutate(data,
                           pred = factor(.fitted, rev(classes)),
                           obs = factor(.outcome, rev(classes))))

    roc_res <-
      caret::multiClassSummary(roc_res, lev = rev(classes))

    tibble(statistic = c('log_loss',
                         'AUC',
                         'prAUC',
                         'correct_rate',
                         'kappa', 'F1',
                         'Se', 'Sp',
                         'PPV', 'NPV',
                         'precision', 'recall',
                         'detection_rate',
                         'balanced_accuracy'),
           estimate = unname(roc_res),
           lower_ci = NA,
           upper_ci = NA)

  }

# C index, man class probability and Brier scores for classification ------

#' Calculate Harrell's C index.
#'
#' @description
#' Calculates Harrell's C, a wrapper around
#' \code{\link[survival]{concordance}}.
#'
#' @param data a data frame with .outcome and .fitted columns.
#' @param ci logical, should 95% confidence interval be returned?
#'
#' @return a tibble with the statistic values.

  get_concordance <- function(data, ci = TRUE) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    c_object <- survival::concordance(.outcome ~ .fitted, data = data)

    tibble(statistic = 'c_index',
           estimate = c_object$concordance,
           lower_ci = if(!ci) NA else c_object$concordance +
             sqrt(c_object$var) * qnorm(0.025),
           upper_ci = if(!ci) NA else c_object$concordance +
             sqrt(c_object$var) * qnorm(0.975))

  }

#' Calculate Brier score and weighted class probability average.
#'
#' @description
#' `get_brier()` computes the Brier score (statistic name `brier_score`).
#' `get_classp()` computes a weighted arithmetic mean of the winning class
#' assignment probabilities. The weights are frequencies of the true outcome
#' classes(statistic name `class_p_outcome`) and predicted classes
#' (`class_p_fitted`).
#' For cross-validation, mean metrics with 95% confidence intervals
#' are returned.
#'
#' @param data a data frame with the columns `.outcome`, `.fitted`,
#' and, optionally `.resample`. The column `square_dist` is required for the
#' Brier scores. The column `winner_p` is required for the average probabilites.
#' @param ci_method method used for 95% CI calculation.
#'
#' @return a tibble with the columns `statistic`, `estimate`, `lower_ci` and
#' `upper_ci` storing the statistic name, its estimated value
#' as well as lower and upper limit of the 95% confidence interval.

  get_brier <- function(data, ci_method = c('percentile', 'bca', 'norm')) {

    ## entry control -------

    stopifnot(all(c('.outcome', '.fitted', 'square_dist') %in% names(data)))

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca', 'norm'))

    statistic <- NULL
    estimate <- NULL
    lower_ci <- NULL
    upper_ci <- NULL

    ## stats ------

    if(!'.resample' %in% names(data)) {

      bs <- tibble(statistic = 'brier_score',
                   estimate = mean(data[['square_dist']], na.rm = TRUE),
                   lower_ci = NA,
                   upper_ci = NA)

    } else {

      splits <- split(data, factor(data[['.resample']]))

      bs_res <- map_dbl(splits, ~mean(.x[['square_dist']]))

      bs <- get_ci(bs_res, method = ci_method)

      bs <- mutate(bs, statistic = 'brier_score')

    }

    return(bs)

  }

#' @rdname get_brier

  get_classp <- function(data, ci_method = c('percentile', 'bca', 'norm')) {

    ## entry control -------

    stopifnot(all(c('.outcome', '.fitted', 'winner_p') %in% names(data)))

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca', 'norm'))

    statistic <- NULL
    estimate <- NULL
    lower_ci <- NULL
    upper_ci <- NULL

    outcome_wt <- NULL
    fitted_wt <- NULL

    .outcome <- NULL
    .fitted <- NULL

    ## weights and analysis table -------

    wts <- c('.outcome' = '.outcome',
             '.fitted' = '.fitted')

    wts <- map(wts, ~count(data, .data[[.x]]))

    wts <- map(wts, ~set_names(.x[[2]], as.character(.x[[1]])))

    data <-
      mutate(data,
             outcome_wt = wts[['.outcome']][as.character(.outcome)],
             fitted_wt = wts[['.fitted']][as.character(.fitted)])

    ## weighted means --------

    if(!'.resample' %in% names(data)) {

      wt_means <-
        tibble(statistic = c('class_p_outcome', 'class_p_fitted'),
               estimate = c(weighted.mean(data[['winner_p']],
                                          data[['outcome_wt']]),
                            weighted.mean(data[['winner_p']],
                                          data[['fitted_wt']])),
               lower_ci = NA,
               upper_ci = NA)

    } else {

      splits <- split(data, factor(data[['.resample']]))

      wt_res <-
        map(splits,
            ~list(class_p_outcome = weighted.mean(.x[['winner_p']],
                                                  .x[['outcome_wt']]),
                  class_p_fitted = weighted.mean(.x[['winner_p']],
                                                 .x[['fitted_wt']])))

      wt_res <- purrr::transpose(wt_res)

      wt_res <- map(wt_res, unlist)

      wt_means <- map(wt_res, get_ci, method = ci_method)

      wt_means <- map2_dfr(wt_means, names(wt_means),
                           ~mutate(.x, statistic = .y))

      wt_means <- dplyr::relocate(wt_means, statistic)

    }

    return(wt_means)

  }

# Binary classifier residuals, variance and errors -------

#' Binary classifier residuals, variance and errors.
#'
#' @description
#' Retrieves working binary residuals (`get_resids_binary()`),
#' outcome variance (`get_var_binary()`) and fit errors
#' (`get_errors_binary()`: MAE, MSE, RMSE and pseudo-R-squared).
#'
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with the class order.
#'
#' @return a numeric or a  tibble with the statistic values.

  get_resids_binary <- function(data, classes) {

    stopifnot(all(c('.outcome', '.fitted', classes) %in% names(data)))

    if(is.factor(data[['.outcome']])) {

      outcome <- as.numeric(data[['.outcome']]) - 1

    } else {

      outcome <- data[['.outcome']]

    }

    data[[classes[2]]] - outcome

  }

#' @rdname get_resids_binary

  get_var_binary <- function(data) {

    stopifnot(all(c('.outcome', '.fitted') %in% names(data)))

    if(is.factor(data[['.outcome']])) {

      outcome <- as.numeric(data[['.outcome']]) - 1

    } else {

      outcome <- data[['.outcome']]

    }

    stats::var(outcome)

  }

#' @rdname get_resids_binary

  get_errors_binary <- function(data, classes) {

    stopifnot(all(c('.outcome', '.fitted', classes) %in% names(data)))

    resids <- get_resids_binary(data, classes)

    tibble(statistic = c('MAE', 'MSE', 'RMSE', 'rsq'),
                   estimate = c(mean(abs(resids)),
                                mean(resids^2),
                                sqrt(mean(resids^2)),
                                1 - (mean(resids^2)/get_var_binary(data))),
                   lower_ci = NA,
                   upper_ci = NA)

  }

# Ready to use analysis functions -----

#' Analyze regression, binary or multi-class fit.
#'
#' @description
#' For regression: computes fit errors and fit - outcome correlations.
#' For binary and multiple-class models, C-index,
#' receiver-operator characteristic, Cohen's kappa and accuracy is retrieved.
#' Single class-relevant statistics are averaged over all classes in case of
#' multi-class predictions.
#'
#' @param data a data frame with .outcome and .fitted columns.
#' @param classes a vector with classes of the outcome.
#' @param ci should 95% CI be computed?
#' @param c_index logical, should concordance index be computed?
#'
#' @return a tibble with the statistic values.

  analyze_reg <- function(data, ci = FALSE) {

    rbind(get_errors(data),
          get_correlations(data, ci = ci))

  }

#' @rdname analyze_reg

  analyze_class <- function(data,
                            classes,
                            c_index = TRUE,
                            ci = FALSE) {

    if(!c_index) return(get_roc(data, classes = classes))

    rbind(get_concordance(data, ci = ci),
          get_roc(data, classes = classes))

  }

# Solutions for cross validation -----

#' Calculate mean with 95% confidence interval
#'
#' @description
#' Computes mean with 95% confidence interval.#'
#'
#' @param vector a numeric vector.
#' @param method the method used for calculation of 95% confidence interval.
#'
#' @return a data frame with mean, lower CI and upper CI values

  get_ci <- function(vector,
                     method = c('percentile', 'bca', 'norm')) {

    method <- match.arg(method[1],
                        c('percentile', 'bca', 'norm'))

    ci_vec <-
      switch(method,
             norm = c(mean(vector, na.rm = TRUE) + stats::sd(vector, na.rm = TRUE) * stats::qnorm(0.025),
                      mean(vector, na.rm = TRUE) + stats::sd(vector, na.rm = TRUE) * stats::qnorm(0.975)),
             percentile = stats::quantile(vector, c(0.025, 0.975), na.rm = TRUE),
             bca = coxed::bca(vector, conf.level = 0.95))

    stat_tbl <- as.list(set_names(ci_vec, c('lower_ci', 'upper_ci')))

    stat_tbl$estimate <- mean(vector, na.rm = TRUE)

    as_tibble(stat_tbl[c('estimate', 'lower_ci', 'upper_ci')])

  }

#' Get CV prediction statistics.
#'
#' @description
#' Computes fit statistics for each CV fold and returns the statistics' means
#' with 95% confidence intervals (CI).
#'
#' @param data a data frame with .outcome, .fitted and .resample columns.
#' @param fun analysis function.
#' @param ci_method method used for 95% CI calculation.
#' @param ... additional arguments passed to the analysis function.
#'
#' @return a tibble with the statistic values.

  analyze_cv <- function(data,
                         fun,
                         ci_method = c('percentile', 'bca', 'norm'),
                         ...) {

    ci_method <- match.arg(ci_method[1], c('percentile', 'bca', 'norm'))

    splits <- split(data, factor(data[['.resample']]))

    ## eliminating splits with single observations
    ## no stats can be computed for them

    splits <- map(splits,
                  function(x) if(nrow(x) > 1) x else NULL)

    splits <- compact(splits)

    split_stats <- map(splits, fun, ...)

    est_tbl <- map2_dfc(split_stats, names(split_stats),
                        ~set_names(.x['estimate'], .y))

    summ_stats <- map_dfr(1:nrow(est_tbl),
                          ~get_ci(unlist(est_tbl[.x, ]),
                                  method = ci_method))

    statistic <- NULL

    summ_stats <-
      mutate(summ_stats,
             statistic = split_stats[[1]][['statistic']])

    summ_stats[c('statistic', 'estimate', 'lower_ci', 'upper_ci')]

  }

# END ------
