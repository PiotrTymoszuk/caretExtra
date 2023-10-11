# Utils for classification models tackling quality of detection of particular
# classes in a one versus rest manner.

# Numeric stats -------

#' Accuracy, assignment probability, Brier scores and ROC for outcome classes.
#'
#' @description
#' Those internally utilized functions compute:
#'
#' * `comp_class_stats()`:  average class assignment probability and
#' Brier scores for the outcome classes.
#'
#' * `comp_class_roc()`: accuracy, sensitivity, specificity, recall etc.
#' for each of the class compared with the rest.
#'
#' @details
#' For regression, NULL is returned with a warning.
#' `comp_class_roc()` uses \code{\link[caret]{multiClassSummary}}.
#'
#' @return a data frame with columns representing the requested statistics for
#' particular classes.
#'
#' @param predx_object a `predx` class object.

  comp_class_stats <- function(predx_object) {

    ## entry control -------

    stopifnot(is_predx(predx_object))

    if(predx_object$type == 'regression') {

      warning("Class-spacific statistics are not available for regression.",
              call. = FALSE)

      return(NULL)

    }

    .outcome <- NULL
    square_dist <- NULL
    winner_p <- NULL

    brier_score <- NULL
    class_p <- NULL

    ## analysis table ------

    sq_tbl <- squared(predx_object)
    class_p_tbl <- classp(predx_object)

    sq_tbl <- select(sq_tbl,
                     any_of(c('.observation', '.resample',
                              '.outcome', '.fitted',
                              'square_dist')))

    class_p_tbl <- select(class_p_tbl,
                          any_of(c('.observation', '.resample',
                                   'winner_p')))

    if(predx_object$prediction == 'cv') {

      by_vars <- c('.observation', '.resample')

    } else {

      by_vars <- '.observation'

    }

    class_data <- left_join(sq_tbl, class_p_tbl, by = by_vars)

    ## outcome class-specific stats ------

    class_data <- dplyr::group_by(class_data, .outcome)

    dplyr::summarise(class_data,
                     brier_score = mean(square_dist, na.rm = TRUE),
                     class_p = mean(winner_p, na.rm = TRUE))

  }

#' @rdname comp_class_stats

  comp_class_roc <- function(predx_object) {

    ## entry control -------

    stopifnot(is_predx(predx_object))

    if(predx_object$type == 'regression') {

      warning("Class-spacific statistics are not available for regression.",
              call. = FALSE)

      return(NULL)

    }

    ## analysis data frame and stats -------

    .outcome <- NULL
    .fitted <- NULL
    pred <- NULL
    obs <- NULL

    pred_data <- components(predx_object, 'data')[c('.outcome', '.fitted')]

    pred_data <- mutate(pred_data,
                        .outcome = as.character(.outcome),
                        .fitted = as.character(.fitted))

    class_lst <- list()
    stat_lst <- list()

    for(i in predx_object$classes) {

      class_lst[[i]] <- mutate(pred_data,
                               obs = ifelse(.outcome == i,
                                            i, 'rest'),
                               obs = factor(obs, c(i, 'rest')),
                               pred = ifelse(.fitted == i,
                                             i, 'rest'),
                               pred = factor(pred, c(i, 'rest')))

      class_lst[[i]] <- as.data.frame(class_lst[[i]][c('obs', 'pred')])

      stat_lst[[i]] <-
        caret::multiClassSummary(class_lst[[i]], lev = c(i, 'rest'))

    }

    ## formatting of the ROC results ------

    stat_lst <- map(stat_lst, as.list)
    stat_lst <- map(stat_lst, as_tibble)

    stat_lst <- map2_dfr(stat_lst, names(stat_lst),
                         ~mutate(.x, .outcome = .y))

    stat_lst <- dplyr::relocate(stat_lst, .outcome)

    stat_lst <- set_names(stat_lst,
                          c('.outcome',
                            'correct_rate',
                            'kappa', 'F1',
                            'Se', 'Sp',
                            'PPV', 'NPV',
                            'precision', 'recall',
                            'detection_rate',
                            'balanced_accuracy'))

    mutate(stat_lst,
           .outcome = factor(.outcome, predx_object$classes))

  }

# ROC plots for classes ---------

#' Receiver operating characteristic plots for outcome classes.
#'
#' @description
#' Plots receiver-operating characteristic (ROC) plots for outcome classes.
#' ROC metrics for particular classes are obtained in an one versus rest
#' comparison.
#'
#' @details
#' For regression, NULL is returned with a warning.
#'
#' @param predx_object a `predx` class object.
#' @param one_plot logical: should all ROC curves be presented in one plot?
#' @param palette a character vector with color names of hex codes. If `NULL`,
#' the default `ggplot2` palette is used.
#' @param line_size width of the ROC curve lines.
#' @param point_size size of the plot point.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle.
#' @param plot_tag plot tag.
#' @param show_annotation logical: should sensitivity and specificity values
#' be displayed in the plot?
#' @param annotation_size size of the annotation text and of the cutoff label.
#' @param annotation_x annotation x position.
#' @param annotation_y annotation y position.
#' @param annotation_hjust horizontal justification of the annotation text.
#' @param annotation_vjust horizontal justification of the annotation text.
#' @param annotation_offset line spacing in the annotation text.
#' Applies only if `one_plot = TRUE`.
#' @param cust_theme custom `ggplot` theme. If `NULL`, the default ROC theme
#' of the `plotROC` is used.
#' @param ... extra arguments passed to \code{\link[plotROC]{geom_roc}}.
#'
#' @return if `one_plot = TRUE` a single `ggplot` object. Otherwise,
#' a list of `ggplot` objects.

  plot_class_roc <- function(predx_object,
                             one_plot = FALSE,
                             palette = NULL,
                             line_size = 0.5,
                             point_size = 0.3,
                             plot_title = NULL,
                             plot_subtitle = NULL,
                             plot_tag = NULL,
                             show_annotation = TRUE,
                             annotation_size = 2.75,
                             annotation_x = 0.6,
                             annotation_y = 0.3,
                             annotation_offset = 0.1,
                             annotation_hjust = 0,
                             annotation_vjust = 1,
                             cust_theme = NULL, ...) {

    ## input control -------

    stopifnot(is_predx(predx_object))

    if(predx_object$type != 'multi_class') {

      stop(paste('ROC plots for outcome categories can be generated',
                 'only for predictions of multi-category',
                 'classification models.'),
           call. = FALSE)

    }

    classes <- predx_object$classes

    stopifnot(is.logical(one_plot))

    if(is.null(palette)) {

      palette <- scales::hue_pal()(length(classes))

    }

    stopifnot(is.character(palette))

    if(length(palette) != length(classes)) {

      stop("The lenght of 'palette' must match the number of classes.",
           call. = FALSE)

    }

    stopifnot(is.numeric(line_size))
    stopifnot(is.numeric(point_size))

    stopifnot(is.logical(show_annotation))
    stopifnot(is.numeric(annotation_size))
    stopifnot(is.numeric(annotation_x))
    stopifnot(is.numeric(annotation_y))
    stopifnot(is.numeric(annotation_hjust))
    stopifnot(is.numeric(annotation_vjust))

    if(!is.null(cust_theme)) {

      if(!inherits(cust_theme, 'theme')) {

        stop("'cust_theme' has to be a valid ggplot theme object.",
             call. = FALSE)

      }

    }

    ## annotation text--------

    if(show_annotation) {

      roc_stats <- clstats(predx_object)

      .outcome <- NULL
      .fitted <- NULL
      Se <- NULL
      Sp <- NULL
      plot_txt <- NULL
      x_pos <- NULL
      y_pos <- NULL
      plot_subtitle <- NULL
      correct_rate <- NULL
      kappa <- NULL
      brier_score <- NULL

      roc_stats <- mutate(roc_stats,
                          class = factor(.outcome,
                                         levels = predx_object$classes),
                          .fitted = .outcome,
                          plot_txt = paste0('Se = ', signif(Se, 2),
                                            ', Sp = ', signif(Sp, 2)),
                          x_pos = annotation_x,
                          y_pos = 0:(nrow(roc_stats) - 1),
                          y_pos = annotation_y + y_pos * annotation_offset,
                          plot_subtitle = paste0('Acc = ',
                                                 signif(correct_rate, 2),
                                                 ', \u03BA = ',
                                                 signif(kappa, 2),
                                                 ', BS = ',
                                                 signif(brier_score, 2)))

      roc_stats <- dplyr::arrange(roc_stats, class)

    }

    ## plotting data -------

    pred_data <- components(predx_object, c('data'))[c('.outcome', '.fitted')]

    plot_data <- list()

    for(i in classes) {

      plot_data[[i]] <-
        mutate(pred_data,
               .outcome = ifelse(.outcome == i,
                                 i, 'rest'),
               .outcome = factor(.outcome, c('rest', i)),
               .fitted = ifelse(.fitted == i,
                                i, 'rest'),
               .fitted = factor(.fitted, c('rest', i)))

      plot_data[[i]] <- mutate(plot_data[[i]],
                               .outcome = as.numeric(.outcome) - 1,
                               .fitted = as.numeric(.fitted) - 1)

    }

    if(one_plot) {

      plot_data <- map2_dfr(plot_data, names(plot_data),
                            ~mutate(.x, class = .y))

    }

    ## n numbers -------

    n_numbers <- components(predx_object, 'n_classes')

    n_labs <-
      map2_chr(n_numbers[[1]], n_numbers[[3]],
               paste, sep = ': n = ')

    ## plots --------

    if(one_plot) {

      roc_stats <-
        dplyr::arrange(roc_stats, dplyr::desc(class))

      if(is.null(plot_subtitle)) {

        plot_subtitle <- paste(n_labs, collapse = ', ')

      }

      roc_plot <- ggplot(plot_data,
                         aes(d = .data[['.outcome']],
                             m = .data[['.fitted']],
                             color = .data[['class']])) +
        plotROC::geom_roc(pointsize = point_size,
                          size = line_size,
                          cutoffs.at = 1,
                          labels = FALSE, ...) +
        ggplot2::geom_text(data = roc_stats,
                           aes(x = x_pos,
                               y = y_pos,
                               color = class,
                               label = plot_txt),
                           size = annotation_size,
                           hjust = annotation_hjust,
                           vjust = annotation_vjust,
                           show.legend = FALSE) +
        ggplot2::scale_color_manual(values = palette,
                                    name = '') +
        plotROC::style_roc() +
        ggplot2::geom_abline(slope = 1,
                             intercept = 0,
                             linetype = 'dashed') +
        ggplot2::labs(title = plot_title,
                      subtitle = plot_subtitle,
                      tag = plot_tag)

      return(roc_plot)

    } else {

      roc_plots <-
        pmap(list(x = plot_data,
                  y = palette,
                  z = classes,
                  v = roc_stats$plot_subtitle,
                  w = n_labs),
             function(x, y, z, v, w) ggplot(x,
                                            aes(d = .data[['.outcome']],
                                                m = .data[['.fitted']])) +
               plotROC::geom_roc(pointsize = point_size,
                                 size = line_size,
                                 cutoffs.at = 1,
                                 labels = FALSE,
                                 color = y, ...) +
               plotROC::style_roc() +
               ggplot2::geom_abline(slope = 1,
                                    intercept = 0,
                                    linetype = 'dashed') +
               ggplot2::labs(title = z,
                             subtitle = v,
                             tag = paste0('total: n = ', nobs(predx_object),
                                          ', ', w)))

      roc_plots <-
        pmap(list(x = roc_plots,
                  y = roc_stats$plot_txt,
                  z = palette),
             function(x, y, z) x +
               ggplot2::annotate('text',
                                 label = y,
                                 x = annotation_x,
                                 y = annotation_y,
                                 hjust = annotation_hjust,
                                 vjust = annotation_vjust,
                                 size = annotation_size,
                                 color = z))

      return(roc_plots)

    }

  }

# END -------
