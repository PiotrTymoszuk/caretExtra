# Utils for classification models tackling quality of detection of particular
# classes in a one versus rest manner.

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

# END -------
