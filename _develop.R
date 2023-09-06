
  library(caret)
  library(caretExtra)

# Development and testing stuff -----

  testControl <- caret::trainControl(method = 'repeatedcv',
                                     number = 10,
                                     repeats = 5,
                                     returnData = TRUE,
                                     returnResamp = 'final',
                                     savePredictions = 'final',
                                     classProbs = TRUE)

  devData_class <- tibble::as_tibble(MASS::biopsy)
  devData_class <- dplyr::filter(devData_class, complete.cases(devData_class))


  devData_corr <- tibble::as_tibble(MASS::birthwt)
  devData_corr <- dplyr::filter(devData_corr, complete.cases(devData_corr))

  devData_multi <- dplyr::mutate(mtcars,
                                 .cyl = paste0('cyl_', cyl),
                                 .cyl = factor(.cyl))

  devData_multi <- dplyr::filter(devData_multi,
                                 complete.cases(devData_multi))

  trainClassIDs <- sample(1:nrow(devData_class), 500, replace = FALSE)
  trainCorrIDs <- sample(1:nrow(devData_corr), 120, replace = FALSE)
  trainMultiIDs <- sample(1:nrow(devData_multi), 20, replace = FALSE)

  trainClass <- devData_class[trainClassIDs, ]
  testClass <- devData_class[-trainClassIDs, ]

  trainCorr <- devData_corr[trainCorrIDs, ]
  testCorr <- devData_corr[-trainCorrIDs, ]

  trainMulti <- devData_multi[trainMultiIDs, ]
  testMulti <- devData_multi[-trainMultiIDs, ]

  class_form <- class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9

  corr_form <- bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv

  multi_form <- .cyl ~ mpg + disp + hp + drat + wt + qsec + gear + carb

# Development models -----

  doParallel::registerDoParallel(cores = 7)

  class_model <- caret::train(form = class_form,
                              data = trainClass,
                              method = 'nnet',
                              metric = 'Kappa',
                              trControl = testControl)

  corr_models <- caret::train(form = corr_form,
                              data = trainCorr,
                              method = 'rf',
                              metric = 'MAE',
                              trControl = testControl)

  multi_models <- caret::train(form = multi_form,
                               data = trainMulti,
                               method = 'nnet',
                               metric = 'Kappa',
                               trControl = testControl)

  doParallel::stopImplicitCluster()

# testing the toolbox -----

  ## builder

  caretx_class <- caretx(class_model)

  caretx_corr <- caretx(corr_models)

  caretx_multi <- caretx(multi_models)

  ## predictions, predx objects

  test_class_pred <- predict(caretx_class, newdata = testClass)

  test_corr_pred <- predict(caretx_corr, newdata = testCorr)

  test_multi_pred <- predict(caretx_multi, newdata = testMulti)

  ## prediction, caretx models

  predict(caretx_class, newdata = testClass, plain = TRUE)

  predict(caretx_corr, newdata = testCorr, plain = TRUE)

  predict(caretx_multi, newdata = testMulti, plain = TRUE)

  ## summary, predx objects

  summary(test_class_pred$cv, ci_method = 'bca')

  summary(test_corr_pred$cv, ci_method = 'percentile')

  summary(test_multi_pred$cv, ci_method = 'norm')

  ## summary, caretx models

  summary(caretx_class, newdata = testClass)

  summary(caretx_corr, newdata = testCorr)

  summary(caretx_multi, newdata = testMulti)

  ## extractors

  nobs(caretx_corr)

  ## model QC

  residuals(caretx_class, newdata = testClass)

  residuals(caretx_corr, newdata = testCorr)

  residuals(caretx_multi)

  # confusion matrix

  confusion(test_class_pred$train, scale = 'fraction')

  confusion(test_corr_pred$train)

  confusion(test_multi_pred$cv)

  confusion(caretx_class, scale = 'fraction', newdata = testClass)

  confusion(caretx_multi, scale = 'none', newdata = testMulti)

  ## extractor

  components(caretx_corr, newdata = testCorr, what = 'fit')

  ## plotting of the fitted values

  plot(x = test_corr_pred$test, type = 'regression')

  plot(x = test_class_pred$test, type = 'confusion')

  plot(x = test_multi_pred$test, type = 'fit')

  plot(caretx_class, type = 'fit', newdata = testClass, plot_title = c('Training', 'CV', 'Test'))

  plot(caretx_corr,
       type = 'fit',
       newdata = testCorr,
       plot_title = c('Training', 'CV', 'Test'),
       cust_theme = ggplot2::theme_light() + theme(plot.tag.position = 'bottom'))

  plot(caretx_multi,
       type = 'fit',
       newdata = testMulti,
       plot_title = c('Training', 'CV', 'Test'),
       cust_theme = ggplot2::theme_light() + theme(plot.tag.position = 'bottom'))

  ## calibration

  calibration(caretx_corr, qu = 0.5)

  test_cal <- calibration(caretx_corr,
                          newdata = testCorr,
                          qu = c(0.2, 0.4, 0.6))

  purrr::map(test_cal[c("train", "cv", "test")],
             plot, 'fit')

# END ------
