# Plotting methods

#' Draw diagnostic plots and plots for predictions.
#'
#' @description
#' Generates diagnostic plots of residuals or plots of fitted versus outcome
#' as appropriate for the particular prediction type and
#' displays prediction type-specific analysis results.
#'
#' @references
#' Kuhn M. Building predictive models in R using the caret package.
#' J Stat Softw (2008) 28:1–26. doi:10.18637/jss.v028.i05
#' @references
#' Brier GW. VERIFICATION OF FORECASTS EXPRESSED IN TERMS OF PROBABILITY.
#' Mon Weather Rev (1950) 78:1–3.
#' doi:10.1175/1520-0493(1950)078<0001:vofeit>2.0.co;2
#' @references
#' Goldstein-Greenwood J. A Brief on Brier Scores | UVA Library. (2021) Available
#' at: https://library.virginia.edu/data/articles/a-brief-on-brier-scores
#' @references
#' Cohen J. A Coefficient of Agreement for Nominal Scales. Educ Psychol Meas
#' (1960) 20:37–46. doi:10.1177/001316446002000104
#'
#' @param x a \code{\link{caretx}} model or \code{\link{predx}} prediction
#' object.
#' @param newdata an optional test data set.
#' @param type type of the plot:
#'
#' * `diagnostic`: diagnostic plots of residuals
#'
#' * `fit`: the fitted versus outcome plot specific for the prediction character
#' (scatter plot for regression, ROC for binary classification and
#' heat map of the confusion matrix for multi-class prediction)
#'
#' * `regression`: scatter plot of the fitted vs outcome values
#'
#' * `roc`: receiver-operating characteristic (ROC) plot
#'
#' * `confusion`: heat map representation of the confusion matrix.
#'
#' * `performance`: a scatter plot with model performance stats in the
#' test, resample (CV) and, optionally, training data set.
#' See: \code{\link{plot_performance}} for details.
#'
#' * `class_p`: a list of scatter plots with square distances to the outcome
#' (Brier et al.) and class-assignment probabilities. By default, misclassified
#' observations are labeled. See: \code{\link{plot_class_p}} for details.
#'
#' * `class_stats`: a list of box plots with square distances to the outcome
#' (Brier et al.) and class-assignment probabilities.
#' See: \code{\link{plot_class_stats}} for details.
#'
#' @param plot_title `predx`: plot title;
#' `caretx`: a vector with plot titles, length 2 if no `newdata` provided,
#' otherwise length 3.
#' @param plot_subtitle plot subtitle. If not specified by the user,
#' the sub-title displays statistics specific for the prediction type.
#' @param plot_tag plot tag, number of complete observations and events
#' if not specified otherwise by the user.
#' @param signif_digits significant digits used for rounding of statistic
#' values presented in the plot.
#' @param cust_theme customized plot theme provided by the user.
#' @param ... extra arguments passed to plotting functions, as specified by
#' the plot type:
#'
#' * `diagnostic`: \code{\link{get_qc_plots}}
#'
#' * `regression`: \code{\link{plot_regression}}
#'
#' * `roc`: \code{\link{plot_roc}}
#'
#' * `confusion`: \code{\link{plot_confusion}}
#'
#' * `performance`: \code{\link{plot_performance}}
#'
#' * `class_p`: \code{\link{plot_class_p}}
#'
#' * `class_stats`: \code{\link{plot_class_stats}}
#'
#' @return a ggplot graphic with the requested content (`predx`) or a list of
#' ggplot graphics (`caretx`, except for `performance` plot type).
#'
#' @export plot.predx
#' @export

  plot.predx <- function(x,
                         type = c('diagnostic', 'fit',
                                  'regression', 'roc',
                                  'confusion', 'class_p',
                                  'class_stats'),
                         plot_title = NULL,
                         plot_subtitle = NULL,
                         plot_tag = NULL,
                         signif_digits = 2,
                         cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control --------

    stopifnot(is_predx(x))
    stopifnot(inherits(cust_theme, 'theme'))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    type <-
      match.arg(type[1],
                c('diagnostic', 'fit',
                  'regression', 'roc',
                  'confusion', 'class_p',
                  'class_stats'))

    if(x$type == 'multi_class' & !type %in% c('fit', 'confusion', 'class_p', 'class_stats')) {

      warning(paste('Scatter plots and ROC plots are not available',
                    'for multi-class models.'),
              call. = FALSE)

      return(NULL)

    }

    if(x$type == 'regression' & type %in% c('roc', 'confusion', 'class_p', 'class_stats')) {

      warning(paste('ROC, confusion matrix and class-specific plots are not',
                    'available for regression.'),
              call. = FALSE)

      return(NULL)

    }

    if(x$type %in% c('binary', 'multi_class') & type == 'regression') {

      warning('Regression plots are not available for classification.',
              call. = FALSE)

      return(NULL)

    }

    ## annotation -----------

    if(x$type == 'binary') {

      roc_annotation <- summary(x)

      if(x$prediction == 'cv') {

        roc_annotation <-
          paste0('AUC = ', signif(roc_annotation[3, 2], signif_digits),
                 ' [', signif(roc_annotation[3, 3], signif_digits),
                 ' - ', signif(roc_annotation[3, 4], signif_digits), ']',
                 '\nSe = ', signif(roc_annotation[8, 2], signif_digits),
                 ' [', signif(roc_annotation[8, 3], signif_digits),
                 ' - ', signif(roc_annotation[8, 4], signif_digits), ']',
                 '\nSp = ', signif(roc_annotation[9, 2], signif_digits),
                 ' [', signif(roc_annotation[9, 3], signif_digits),
                 ' - ', signif(roc_annotation[9, 4], signif_digits), ']')

      } else {

        roc_annotation <-
          paste0('AUC = ', signif(unlist(roc_annotation[3, 2]), signif_digits),
                 '\nSe = ', signif(unlist(roc_annotation[8, 2]), signif_digits),
                 '\nSp = ', signif(unlist(roc_annotation[9, 2]), signif_digits))

      }

    }

    if(is.null(plot_subtitle)) {

      stats <- summary(x)

      if(x$type == 'regression') {

        plot_subtitle <-
          paste0('RMSE = ', signif(unlist(stats[3, 2]),
                                   signif_digits),
                 ', R\u00B2 = ', signif(unlist(stats[4, 2]),
                                        signif_digits))

      } else if(x$type == 'binary') {

        plot_subtitle <-
          paste0('Acc = ', signif(unlist(stats[5, 2]),
                                  signif_digits),
                 ', \u03BA = ', signif(unlist(stats[6, 2]),
                                       signif_digits),
                 ', BS = ', signif(unlist(stats[16, 2]),
                                   signif_digits))

      } else {

        plot_subtitle <-
          paste0('Acc = ', signif(unlist(stats[5, 2]),
                                  signif_digits),
                 ', \u03BA = ', signif(unlist(stats[6, 2]),
                                       signif_digits),
                 ', BS = ', signif(unlist(stats[15, 2]),
                                   signif_digits))

      }

    }

    ## plotting ------

    if(type == 'diagnostic') {

      get_qc_plots(predx_object = x, cust_theme = cust_theme, ...)

    } else if(type == 'fit') {

      switch(x$type,
             regression = plot_regression(predx_object = x,
                                          x_var = '.outcome',
                                          y_var = '.fitted',
                                          plot_title = plot_title,
                                          plot_subtitle = plot_subtitle,
                                          plot_tag = plot_tag,
                                          cust_theme = cust_theme, ...),
             binary = plot_roc(predx_object = x,
                               plot_title = plot_title,
                               plot_subtitle = plot_subtitle,
                               plot_tag = plot_tag,
                               cust_theme = cust_theme,
                               annotation_txt = roc_annotation, ...),
             multi_class = plot_confusion(predx_object = x,
                                          plot_title = plot_title,
                                          plot_subtitle = plot_subtitle,
                                          plot_tag = plot_tag,
                                          cust_theme = cust_theme, ...))

    } else {

      switch(type,
             regression = plot_regression(predx_object = x,
                                          x_var = '.outcome',
                                          y_var = '.fitted',
                                          plot_title = plot_title,
                                          plot_subtitle = plot_subtitle,
                                          plot_tag = plot_tag,
                                          cust_theme = cust_theme, ...),
             roc = plot_roc(predx_object = x,
                            plot_title = plot_title,
                            plot_subtitle = plot_subtitle,
                            plot_tag = plot_tag,
                            cust_theme = cust_theme,
                            annotation_txt = roc_annotation, ...),
             confusion = plot_confusion(predx_object = x,
                                        plot_title = plot_title,
                                        plot_subtitle = plot_subtitle,
                                        plot_tag = plot_tag,
                                        cust_theme = cust_theme, ...),
             class_p = plot_class_p(predx_object = x,
                                    plot_subtitle = plot_subtitle,
                                    plot_tag = plot_tag,
                                    cust_theme = cust_theme, ...),
             class_stats = plot_class_stats(predx_object = x,
                                            plot_subtitle = plot_subtitle,
                                            plot_tag = plot_tag,
                                            cust_theme = cust_theme, ...))

    }

  }

#' @rdname plot.predx
#' @export plot.caretx
#' @export

  plot.caretx <- function(x,
                          newdata = NULL,
                          type = c('diagnostic', 'fit',
                                   'regression', 'roc',
                                   'confusion', 'performance',
                                   'class_p', 'class_stats'),
                          plot_title = NULL,
                          plot_subtitle = NULL,
                          plot_tag = NULL,
                          signif_digits = 2,
                          cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control --------

    stopifnot(is_caretx(x))

    type <- match.arg(type[1],
                      c('diagnostic', 'fit',
                        'regression', 'roc',
                        'confusion', 'performance',
                        'class_p', 'class_stats'))

    ## performance plots ------

    if(type == 'performance') {

      return(plot_performance(caretx_object = x,
                              newdata = newdata,
                              plot_subtitle = plot_subtitle,
                              plot_tag = plot_tag,
                              cust_theme = cust_theme, ...))

    }

    ## other plot types -------

    preds <- predict(x,
                     newdata = newdata,
                     plain = FALSE)

    preds <- compact(preds)

    if(!is.null(plot_title)) {

      plot_list <- list(x = preds,
                        plot_title = plot_title)

    } else {

      plot_list <- list(x = preds)

    }

    pmap(compact(plot_list),
         plot,
         type = type,
         signif_digits = signif_digits,
         cust_theme = cust_theme, ...)

  }

# END ------
