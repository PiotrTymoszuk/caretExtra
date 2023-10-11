# Examples for GitHub's readme

# packages -------

  ## the 'biopsy' and `Boston` data sets provided by MASS

  library(MASS)

  ## the 'wines' data set provided by kohonen

  library(kohonen)

  ## packages used for analysis

  library(tidyverse)
  library(caret)
  library(caretExtra)
  library(gbm)

  ## doParallel for a parallel backend
  ## used for tuning and cross-validation

  library(doParallel)

  ## patchwork for plot panels

  library(patchwork)

# Boston data set -------

  ## Boston: used for the regression model of the house price
  ## normalization of numeric explanatory variables
  ## adding the observation ID in the rownames

  my_boston <- Boston %>%
    filter(complete.cases(.)) %>%
    mutate(chas = car::recode(chas,
                              "0 = 'no';
                              1 = 'yes'"),
           chas = factor(chas, c('no', 'yes')))

  my_boston[, c("crim", "zn", "indus",
                "nox", "rm", "age",
                "dis", "tax", "rad",
                "ptratio", "black", "lstat")] <-
    my_boston[, c("crim", "zn", "indus",
                  "nox", "rm", "age",
                  "dis", "tax", "rad",
                  "ptratio", "black", "lstat")] %>%
    map_dfc(~scale(.x)[, 1])

  rownames(my_boston) <- paste0('obs_', 1:nrow(my_boston))

  ## training and test portion at a 2 to 1 ratio

  boston_ids <- createDataPartition(my_boston$medv, p = 2/3)[[1]]

  my_boston <- list(train = my_boston[boston_ids, ],
                    test = my_boston[-boston_ids, ])

# biopsy data set -------

  ## biopsy: binary classification of benign and malignant samples
  ## explanatory variables are not normalized, since they
  ## are anyway in the  same 1 - 10 scale
  ## IDs in the rownames

  my_biopsy <- biopsy %>%
    filter(complete.cases(.))

  rownames(my_biopsy) <-
    paste(my_biopsy$ID, 1:nrow(my_biopsy), sep = '_sample_')

  my_biopsy <- my_biopsy %>%
    select(-ID)

  ## training and test portion at a 2 to 1 ratio

  biopsy_ids <- createDataPartition(my_biopsy$class, p = 2/3)[[1]]

  my_biopsy <- list(train = my_biopsy[biopsy_ids, ],
                    test = my_biopsy[-biopsy_ids, ])

# wines data set ------

  ## wines: multi-level classification of vintages
  ## normalization of explanatory variables
  ## IDs in the rownames

  data(wines)

  my_wines <- wines %>%
    as.data.frame %>%
    filter(complete.cases(.)) %>%
    map_dfc(~scale(.x)[, 1])

  names(my_wines) <- make.names(names(my_wines))

  my_wines$vintage <- vintages

  my_wines <- as.data.frame(my_wines)

  rownames(my_wines) <- paste0('wine_', 1:nrow(my_wines))

  ## training - test split at a 2 to 1 ratio

  wines_ids <- createDataPartition(my_wines$vintage, p = 2/3)[[1]]

  my_wines <- list(train = my_wines[wines_ids, ],
                   test = my_wines[-wines_ids, ])

# Train control object --------

  train_control <- trainControl(method = 'repeatedcv',
                                number = 5,
                                repeats = 5,
                                savePredictions = 'final',
                                returnData = TRUE,
                                returnResamp = 'final',
                                classProbs = TRUE)

# Building of the caretx models ------

  my_models <- list()

  registerDoParallel(cores = 7)

  set.seed(12345)

  my_models$regression <- caret::train(form = medv ~ .,
                                       data = my_boston$train,
                                       metric = 'MAE',
                                       method = 'gbm',
                                       trControl = train_control)

  my_models$binary <- caret::train(form = class ~ .,
                                   data = my_biopsy$train,
                                   metric = 'Kappa',
                                   method = 'gbm',
                                   trControl = train_control)

  my_models$multi_class <- caret::train(form = vintage ~ .,
                                        data = my_wines$train,
                                        metric = 'Kappa',
                                        method = 'gbm',
                                        trControl = train_control)

  stopImplicitCluster()

  my_models <- my_models %>%
    map(as_caretx)

# Accessing the model components --------

  head(model.frame(my_models$regression))

  formula(my_models$binary)

  components(my_models$multi_class, what = 'tuning')

  components(my_models$regression, what = 'residuals')

  residuals(my_models$regression, newdata = my_boston$test)

  components(my_models$binary, what = 'square_dist')

  components(my_models$binary,
             what = 'confusion',
             newdata = my_biopsy$test)

  components(my_models$multi_class,
             what = 'confusion')

  augment(my_models$regression)
  augment(my_models$multi_class)

# Plots for regression --------

  ## plots of residuals

  regression_resid_plots <- plot(my_models$regression,
                                 newdata = my_boston$test,
                                 type = 'diagnostic')

  regression_resid_fitted_plots <- regression_resid_plots %>%
    map(~.x$resid_fitted) %>%
    map2(., c('Boston: training', 'Boston: CV', 'Boston: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'bottom'))

  regression_qqplots <- regression_resid_plots %>%
    map(~.x$qq.std.resid) %>%
    map2(., c('Boston: training', 'Boston: CV', 'Boston: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'bottom'))

  regression_resid_fitted_plots$train +
    regression_resid_fitted_plots$cv +
    regression_resid_fitted_plots$test +
    plot_layout(ncol = 2)

  regression_qqplots$train +
    regression_qqplots$cv +
    regression_qqplots$test +
    plot_layout(ncol = 2)

  ## plots of fitted versus predicted

  regression_fit_predict <- plot(my_models$regression,
                                 newdata = my_boston$test,
                                 type = 'fit')

  regression_fit_predict <- regression_fit_predict %>%
    map2(., c('Boston: training', 'Boston: CV', 'Boston: test'),
         ~.x +
           labs(title = .y) +
           theme(plot.tag.position = 'bottom'))

  regression_fit_predict$train +
    regression_fit_predict$cv +
    regression_fit_predict$test +
    plot_layout(ncol = 2)

  ## performance stats

  regression_performance_plots <- plot(my_models$regression,
                                       newdata = my_boston$test,
                                       type = 'performance')

  regression_performance_plots +
    scale_size_area(limits = c(0, 1))

# Plots for binary classification models -------

  ## class assignment probability and squared distance to the outcome

  binary_p_plots <- plot(my_models$binary,
                         newdata = my_biopsy$test,
                         type = 'class_p')

  binary_sq_dist_plots <- binary_p_plots %>%
    map(~.x$square_dist) %>%
    map2(c('Biopsy: training', 'Biopsy: CV', 'Biopsy: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'bottom'))

  binary_p_plots <- binary_p_plots %>%
    map(~.x$winner_p) %>%
    map2(c('Biopsy: training', 'Biopsy: CV', 'Biopsy: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'bottom'))

  binary_sq_dist_plots$train  +
    binary_sq_dist_plots$cv +
    binary_sq_dist_plots$test +
    plot_layout(ncol = 2)

  binary_p_plots$train +
    binary_p_plots$cv +
    binary_p_plots$test +
    plot_layout(ncol = 2)

  ## confusion matrices

  binary_confusion_plots <- plot(my_models$binary,
                                 newdata = my_biopsy$test,
                                 type = 'confusion',
                                 scale = 'percent')

  binary_confusion_plots <- binary_confusion_plots %>%
    map2(c('Biopsy: training', 'Biopsy: CV', 'Biopsy: test'),
         ~.x +
           labs(title = .y) +
           theme(plot.tag.position = 'bottom',
                 legend.position = 'none'))

  binary_confusion_plots$train +
    binary_confusion_plots$cv +
    binary_confusion_plots$test +
    plot_layout(ncol = 2)

  ## receiver operating characteristic (ROC) curves

  binary_roc_plots <- plot(my_models$binary,
                           newdata = my_biopsy$test,
                           type = 'roc')

  binary_roc_plots <- binary_roc_plots %>%
    map2(c('Biopsy: training', 'Biopsy: CV', 'Biopsy: test'),
         ~.x +
           labs(title = .y) +
           theme(plot.tag.position = 'bottom'))

  binary_roc_plots$train +
    binary_roc_plots$cv +
    binary_roc_plots$test +
    plot_layout(ncol = 2)

  ## plots of performance statistics

  binary_performance_plots <- plot(my_models$binary,
                                   newdata = my_biopsy$test,
                                   type = 'performance')

  ## indicating kappa and Brier score values expected
  ## for a dummy classifier

  binary_performance_plots +
    geom_hline(yintercept = 0.75, linetype = 'dashed') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    scale_size_area(limits = c(0.5, 1))

# Plots for multi-level classifiers -------

  ## plots of squared distances
  ## and class assignment probabilities

  multi_p_plots <- plot(my_models$multi_class,
                        newdata = my_wines$test,
                        type = 'class_p')

  multi_p_sq_dist_plots <- multi_p_plots %>%
    map(~.x$square_dist) %>%
    map2(., c('Wines: training', 'Wines: CV', 'Wines: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'bottom'))

  multi_p_sq_dist_plots$train +
    multi_p_sq_dist_plots$cv +
    multi_p_sq_dist_plots$test +
    plot_layout(ncol = 2)

  ## confusion matrix plots

  multi_confusion_plots <- plot(my_models$multi_class,
                                newdata = my_wines$test,
                                type = 'confusion')

  multi_confusion_plots <- multi_confusion_plots %>%
    map2(., c('Wines: training', 'Wines: CV', 'Wines: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'none',
                 plot.tag.position = 'bottom'))

  multi_confusion_plots$train +
    multi_confusion_plots$cv +
    multi_confusion_plots$test +
    plot_layout(ncol = 2)

# Numeric stats -------

  regression_stats <- summary(my_models$regression,
                              newdata = my_boston$test)

  binary_stats <- summary(my_models$binary,
                          newdata = my_biopsy$test)

  multi_class_stats <- summary(my_models$multi_class,
                               newdata = my_wines$test)

# Model performance in strata of explanatory variables ------

  regression_chas <- split(my_models$regression,
                           f = chas,
                           newdata = my_boston$test)

  regression_chas_stats <- regression_chas %>%
    map(summary)

  regression_chas_stats[c("cv.no", "cv.yes")] %>%
    map(filter, statistic == 'MAE')

  regression_chas[c("cv.no", "cv.yes")] %>%
    map(plot, type = 'fit')

# Class stats ---------

  clstats(my_models$multi_class)

  class_roc_plots <- clplots(my_models$multi_class,
                             newdata = my_wines$test) %>%
    map2(., c('Wines: training', 'Wines: CV', 'Wines: test'),
         ~.x +
           labs(title = .y) +
           theme(legend.position = 'bottom'))

  class_roc_plots$train +
    class_roc_plots$cv +
    class_roc_plots$test +
    plot_layout(ncol = 2)

# Variable importance -------

  my_importance <- my_models %>%
    map(varImp)

# END ------
