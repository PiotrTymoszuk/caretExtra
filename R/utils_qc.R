# Contains code for the functions used for stripping the model residuals
# and visual quality control by generating residual plots.

# Helper functions ------

#' Calculate expected normal values for the given variable.
#'
#' @description
#' Calclate expected values from the normal distribution.
#'
#'
#' @param data data frame.
#' @param observed name of the variable of interest
#'
#' @details
#' credits to:
#' https://stackoverflow.com/questions/43217104/coloring-points-in-a-geom-qq-plot
#'
#' @return A data frame with the extra variable .expect.norm with
#' the expected normal distribution values

  calc_expected_ <- function(data, observed) {

    .expect.norm <- NULL

    mutate(data[order(data[[observed]]), ],
           .expect.norm = stats::qnorm(stats::ppoints(nrow(data))))

  }

#' Generate a customized scatter plot.
#'
#' @description
#' Draws a simple scatter plot for model diagnostic purposes.
#'
#' @details As per design, takes the output
#' of get_qc_tbl() as data argument,
#' color-codes model potential outliers.
#'
#' @param data data frame.
#' @param x_var name of the variable to be shown in the x axis.
#' @param y_var name of the variable to be shown in the y axis.
#' @param x_lab x axis title.
#' @param y_lab y axis title.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle.
#' @param plot_tag plot tag.
#' @param point_wjitter horizontal jittering of the points.
#' @param point_hjitter vertical jittering of the points.
#' @param point_alpha plot point alpha.
#' @param smooth logical, should a trend line be displayed.
#' @param silent logical, display warnings?
#' @param cust_theme a custom ggplot theme.
#' @param ... extra arguments passed to geom_smooth().
#'
#' @return a ggplot graphic

  point_plot_ <- function(data,
                          x_var,
                          y_var,
                          x_lab = x_var,
                          y_lab = y_var,
                          plot_title = NULL,
                          plot_subtitle = NULL,
                          plot_tag = NULL,
                          point_wjitter = 0.01,
                          point_hjitter = 0.01,
                          point_alpha = 0.75,
                          smooth = TRUE,
                          silent = TRUE,
                          cust_theme = ggplot2::theme_classic(), ...) {

    ## table for plotting

    misslab <- NULL
    .candidate_missfit <- NULL
    .observation <- NULL

    data <- mutate(data,
                   misslab = ifelse(.candidate_missfit == 'yes',
                                    .observation,
                                    NA))
    ## fill colors

    fill_colors <- c(no = 'cornflowerblue',
                     yes = 'firebrick4')

    ## point plot

    point_plot <- ggplot(data,
                         aes(x = .data[[x_var]],
                             y = .data[[y_var]],
                             fill = .candidate_missfit)) +
      ggplot2::geom_point(size = 2,
                          shape = 21,
                          alpha = point_alpha,
                          position = ggplot2::position_jitter(width = point_wjitter,
                                                              height = point_hjitter)) +
      ggrepel::geom_text_repel(aes(label = misslab),
                               show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = fill_colors,
                                 name = 'Candidate outlier') +
      ggplot2::labs(x = x_lab,
                    y = y_lab,
                    title = plot_title) +
      cust_theme

    if(smooth) {

      if(silent) {

        suppressWarnings(point_plot <- point_plot +
                           ggplot2::geom_smooth(show.legend = FALSE,
                                                color = 'black',
                                                fill = 'dodgerblue2', ...))

      } else {

        point_plot <- point_plot +
          ggplot2::geom_smooth(show.legend = FALSE,
                               color = 'black',
                               fill = 'dodgerblue2', ...)

      }

    }

    return(point_plot)

  }

# Table of expanded residuals -----

#' Extended residuals of a predx object.
#'
#' @description
#' Extracts extended working residuals of prediction and potential outliers.
#'
#' @details
#' Calculates working residuals for regression and binary
#' classification predictions.
#' In addition, squared and standardized residuals are returned along with
#' expected normal distribution values for the standardized residuals
#' and the true outcome.
#' Potential outliers are identified by the two-SD criterion.
#'
#' @param predx_object an object of class `predx`.
#'
#' @return a data frame with the fitted values, true outcome,
#' residuals and candidate outliers.

  get_qc_tbl <- function(predx_object) {

    .resid = NULL
    .std.resid = NULL
    .sq.std.resid = NULL
    .candidate_missfit <- NULL

    resids <-
      switch(predx_object$type,
             binary = unname(get_resids_binary(predx_object$data,
                                               classes = predx_object$classes)),
             regression = unname(get_resids(predx_object$data)))

    qc_tbl <-
      mutate(predx_object$data,
             .resid = resids,
             .std.resid = scale(.resid)[, 1],
             .sq.std.resid = .std.resid^2,
             .candidate_missfit = ifelse(abs(.std.resid) > qnorm(0.975),
                                         'yes', 'no'))

    calc_expected_(qc_tbl, '.std.resid')

  }

# Plots of residuals ------

#' Diagnostic plots of model residuals for predx objects.
#'
#' @description
#' Plots a series of diagnostic plots of model residuals with
#' potential outliers indicated.
#'
#' @param predx_object an object of class `predx`.
#' @param cust_theme customized plot theme provided by the user.
#' @param point_wjitter horizontal jittering of the points.
#' @param point_hjitter vertical jittering of the points.
#' @param point_alpha plot point alpha.
#'
#' @return returns a series of ggplot objects with the
#' diagnostic residuals plots.

  get_qc_plots <- function(predx_object,
                           cust_theme = ggplot2::theme_classic(),
                           point_wjitter = 0.01,
                           point_hjitter = 0.01,
                           point_alpha = 0.75) {

    stopifnot(is_predx(predx_object))

    if(predx_object$type == 'multi_class') {

      warning('Residuals for the multi-class predictions are not available.',
              call. = FALSE)

      return(NULL)

    }

    if(!inherits(cust_theme, 'theme')) {

      stop('Please provide a valid ggplot2 theme class object.', call. = FALSE)

    }

    ## QC table

    qc_tbl <- get_qc_tbl(predx_object)

    ## QC plots

    qc_plotting_lst <-
      list(x_var = c('.fitted', '.fitted', '.fitted', '.expect.norm'),
           y_var = c('.resid', '.std.resid', '.sq.std.resid', '.std.resid'),
           plot_title = c('Residuals vs. fitted',
                          'Standardized residuals vs. fitted',
                          'Sqared residuals vs. fitted',
                          'QQ standardized residuals vs expected normal'),
           method = c('loess', 'loess', 'loess', 'lm'),
           smooth = c(TRUE, TRUE, TRUE, TRUE))

    qc_plots <-
      pmap(qc_plotting_lst,
           point_plot_,
           data = qc_tbl,
           point_wjitter = point_wjitter,
           point_hjitter = point_hjitter,
           point_alpha = point_alpha,
           cust_theme = cust_theme)

    set_names(qc_plots,
              c('resid_fitted',
                'std.resid_fitted',
                'sq.resid_fitted',
                'qq.std.resid'))

  }

# END -------
