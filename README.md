[![R](https://github.com/PiotrTymoszuk/caretExtra/actions/workflows/r.yml/badge.svg)](https://github.com/PiotrTymoszuk/caretExtra/actions/workflows/r.yml)

<img src="https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/e5fc7557-ece3-4e38-ac16-b3bf97d1be6a" width="20%" height="20%" align = "right">

# caretExtra
Tools for quality control, prediction and plotting of caret models

## Description

The `caretExtra` package includes a bunch of methods for user friendly extraction of predictions in the train, cross-validation and test data, fit statistic calulation, diagnostic via residuals plots and graphical representation of the results as scatter, ROC and heat map plots. In addition, regression models may be calibrated by the quantile GAM (generalized additive model) method.

Currently, it works only with cross-validated Caret models (CV or repreated CV) created from formula objects. Solution for bootstrap and holdout model construction methods are on the way.

Some concepts of modeling, validatin and evaluation of models followed by the `caretExtra` package are presented in [a presentation](https://github.com/PiotrTymoszuk/caretExtra/blob/6b0b0bb461cd31dec92eecdad1b05a708682e1f0/inst/concepts/ml_workshop.pdf)

## Installation

You may easily fetch the package and its dependencies `somKernels` and `clustTools` with `devtools`: 

```r
## dependencies

devtools::install_github('PiotrTymoszuk/clustTools')
devtools::install_github('PiotrTymoszuk/somKernels')

## the package

devtools::install_github('PiotrTymoszuk/caretExtra')

```

## Terms of use

The package is available under a [GPL-3 license](https://github.com/PiotrTymoszuk/caretExtra/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

`caretExtra` uses tools provided by the [rlang](https://rlang.r-lib.org/), [tidyverse](https://www.tidyverse.org/), [caret](https://topepo.github.io/caret/), [coxed](https://cran.r-project.org/web/packages/coxed/index.html), [ggrepel](https://ggrepel.slowkow.com/), [generics](https://github.com/r-lib/generics), [DescTools](https://andrisignorell.github.io/DescTools/), [plotROC](https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html), [qgam](https://mfasiolo.github.io/qgam/), and [survial](https://cran.r-project.org/web/packages/survival/index.html). Many thanks to their developers, maintainers and contributors.

## Usage

### Fitting of models

<details>

In the example of basic usage of the `caretExtra` tools, I'll use three published data sets: 

* `Boston` provided by the R package `MASS` consisting of data on environmental, geographic and socioeconomic features of houses in Boston as well as their value. This data set will be used for modeling of the house value with regression

* `biopsy` included in the `MASS` package consisting of nine pathological and cytologic properties of breast lesion biopsies rated by the pathologist with a 1 - 10 point scale. With this data set I'll construct a binary classifier predicting the biopsy sample classification as benign or malignant tissue

* `wines` included in the `kohonen` package. and consisting of physical and chemical properties of various Italian wines. This data set will be used to model vintage class with a multi-category classifier

For each data set, randomly selected two-thirds of observations will be used for tuning of the models with 5-repeats 5 fold cross-validation. The remaining observations will be put aside for the final evaluation of the model performance. 

Let's start with few packages which are required for the current analysis:

```r
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
```
Preprocessing is in many cases a tedious step preceding the 'real' analysis. For the `Boston` data set, this includes recoding of the dummy chas variable and normalization of numeric explanatory features as well as selection of the training and test subset observations with the `createDataPartition()` function from the `caret` package. 

```r
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
```
```r
> my_boston %>% map(head)
$train
             crim          zn      indus chas        nox         rm         age       dis        rad        tax    ptratio
obs_2  -0.4169267 -0.48724019 -0.5927944   no -0.7395304  0.1940824  0.36680343 0.5566090 -0.8670245 -0.9863534 -0.3027945
obs_6  -0.4166314 -0.48724019 -1.3055857   no -0.8344581  0.2068916 -0.35080997 1.0766711 -0.7521778 -1.1050216  0.1129203
obs_7  -0.4098372  0.04872402 -0.4761823   no -0.2648919 -0.3880270 -0.07015919 0.8384142 -0.5224844 -0.5769480 -1.5037485
obs_8  -0.4032966  0.04872402 -0.4761823   no -0.2648919 -0.1603069  0.97784057 1.0236249 -0.5224844 -0.5769480 -1.5037485
obs_10 -0.4003331  0.04872402 -0.4761823   no -0.2648919 -0.3994130  0.61548134 1.3283202 -0.5224844 -0.5769480 -1.5037485
obs_11 -0.3939564  0.04872402 -0.4761823   no -0.2648919  0.1314594  0.91389483 1.2117800 -0.5224844 -0.5769480 -1.5037485
           black       lstat medv
obs_2  0.4406159 -0.49195252 21.6
obs_6  0.4101651 -1.04229087 28.7
obs_7  0.4263763 -0.03123671 22.9
obs_8  0.4406159  0.90979986 27.1
obs_10 0.3289995  0.62272769 18.9
obs_11 0.3926395  1.09184562 15.0

$test
             crim          zn      indus chas        nox         rm        age      dis        rad        tax    ptratio     black
obs_1  -0.4193669  0.28454827 -1.2866362   no -0.1440749  0.4132629 -0.1198948 0.140075 -0.9818712 -0.6659492 -1.4575580 0.4406159
obs_3  -0.4169290 -0.48724019 -0.5927944   no -0.7395304  1.2814456 -0.2655490 0.556609 -0.8670245 -0.9863534 -0.3027945 0.3960351
obs_4  -0.4163384 -0.48724019 -1.3055857   no -0.8344581  1.0152978 -0.8090878 1.076671 -0.7521778 -1.1050216  0.1129203 0.4157514
obs_5  -0.4120741 -0.48724019 -1.3055857   no -0.8344581  1.2273620 -0.5106743 1.076671 -0.7521778 -1.1050216  0.1129203 0.4406159
obs_9  -0.3955433  0.04872402 -0.4761823   no -0.2648919 -0.9302853  1.1163897 1.086122 -0.5224844 -0.5769480 -1.5037485 0.3281233
obs_12 -0.4064448  0.04872402 -0.4761823   no -0.2648919 -0.3922967  0.5089051 1.154792 -0.5224844 -0.5769480 -1.5037485 0.4406159
             lstat medv
obs_1  -1.07449897 24.0
obs_3  -1.20753241 34.7
obs_4  -1.36017078 33.4
obs_5  -1.02548665 36.2
obs_9   2.41937935 16.5
obs_12  0.08639286 18.9
```

Pre-processing of the `biopsy` data set is quite easy: I'm not going to normalize the numeric explanatory variables because they are anyway on the same scale. Still, I'm setting unique observation names and selecting the training and test observations with `createDataPartition()`:

```r
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
```

```r
> my_biopsy %>% map(head)
$train
                  V1 V2 V3 V4 V5 V6 V7 V8 V9  class
1000025_sample_1   5  1  1  1  2  1  3  1  1 benign
1002945_sample_2   5  4  4  5  7 10  3  2  1 benign
1016277_sample_4   6  8  8  1  3  4  3  7  1 benign
1017023_sample_5   4  1  1  3  2  1  3  1  1 benign
1035283_sample_11  1  1  1  1  1  1  3  1  1 benign
1036172_sample_12  2  1  1  1  2  1  2  1  1 benign

$test
                  V1 V2 V3 V4 V5 V6 V7 V8 V9     class
1015425_sample_3   3  1  1  1  2  2  3  1  1    benign
1017122_sample_6   8 10 10  8  7 10  9  7  1 malignant
1018099_sample_7   1  1  1  1  2 10  3  1  1    benign
1018561_sample_8   2  1  2  1  2  1  3  1  1    benign
1033078_sample_9   2  1  1  1  2  1  1  1  5    benign
1033078_sample_10  4  2  1  1  2  1  2  1  1    benign
```

In the `wines` data set I need to rename variables to be compatible with R formulas, normalize explanatory variables and select the training and test observations in a similar way as for the remaining data sets:

```r
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

```
```r
> my_wines %>% map(head)
$train
         alcohol  malic.acid        ash ash.alkalinity   magnesium tot..phenols flavonoids non.flav..phenols    proanth
wine_1 0.2551008 -0.50020530 -0.8221529     -2.4930372  0.02909756    0.5710456  0.7375437        -0.8208101 -0.5370519
wine_2 0.2056453  0.01796903  1.1045562     -0.2748590  0.09964918    0.8104843  1.2181890        -0.4999191  2.1399040
wine_3 1.7016732 -0.34832662  0.4865552     -0.8144158  0.94626865    2.4865554  1.4685250        -0.9812556  1.0376281
wine_6 1.7264010 -0.41979894  0.3047901     -1.4738742 -0.25310893    0.3316069  0.4972211        -0.4999191  0.6876992
wine_7 1.3183934 -0.16964581  0.8864382     -0.5746128  1.51068164    0.4912327  0.4872077        -0.4196964 -0.5895412
wine_8 2.2704111 -0.62528187 -0.7130939     -1.6537265 -0.18255731    0.8104843  0.9578395        -0.5801419  0.6876992
          col..int.   col..hue  OD.ratio   proline vintage
wine_1 -0.290306650  0.4059482 1.1284966 0.9683055  Barolo
wine_2  0.268966296  0.3186634 0.8023031 1.3970348  Barolo
wine_3  1.181011408 -0.4232572 1.1994082 2.3338876  Barolo
wine_6  0.083976014  0.2750210 1.3837785 1.7304908  Barolo
wine_7 -0.002065978  0.4495905 1.3837785 1.7463697  Barolo
wine_8  0.062465516  0.5368753 0.3484686 0.9524266  Barolo

$test
          alcohol malic.acid        ash ash.alkalinity  magnesium tot..phenols flavonoids non.flav..phenols     proanth   col..int.
wine_4  0.3045563  0.2234520  1.8316163      0.4445501  1.2990268    0.8104843  0.6674496         0.2220856  0.40775610 -0.31611925
wine_5  1.4914875 -0.5180734  0.3047901     -1.2940219  0.8757170    1.5607256  1.3683906        -0.1790282  0.67020276  0.72929095
wine_11 1.3925766 -0.7682265 -0.1677989     -0.8144158 -0.3236606   -0.1472706  0.4071002        -0.8208101 -0.02965499 -0.02357648
wine_16 1.6151262 -0.3751287  1.2863212      0.1447963  1.4401300    0.8104843  1.1180545        -0.2592509  0.67020276  0.49267547
wine_24 0.6260168 -0.4734032  0.8864382      0.1447963 -0.2531089    0.3794946  0.5873421        -0.6603646  0.12781300 -0.66028721
wine_26 0.4900143 -0.5091393  0.9227912     -1.0242435 -0.4647638    0.8902972  0.9177857        -0.1790282 -0.23961231 -0.10961847
          col..hue   OD.ratio     proline vintage
wine_4   0.3623058 0.46192721 -0.03206274  Barolo
wine_5   0.4059482 0.34846859  2.23861439  Barolo
wine_11  0.9296568 0.30592161  1.69873311  Barolo
wine_16  0.4932329 0.06482205  1.69873311  Barolo
wine_24  0.7114449 1.72415433  0.31727220  Barolo
wine_26 -0.1614029 0.87321470  1.42879247  Barolo
```

`caret` models to be analyzed with the `caretExtra` tools need to meet few requirements. They need to be constructed with a formula, tuned in a cross-validation setting (single or repeated), contain the training data, final predictions and performance metrics in final resamples. Specifically for classification models, propabilities of class assignment need to be included in the `caret` object. To make sure that all those elements are included in modeling results, we customize the `trainControl` object as presented below: 

```r

  train_control <- trainControl(method = 'repeatedcv',
                                number = 5,
                                repeats = 5,
                                savePredictions = 'final',
                                returnData = TRUE,
                                returnResamp = 'final',
                                classProbs = TRUE)
```
Setting `savePredictions = 'final'`, `returnData = TRUE`, `returnResamp = 'final'` and, for classification models, `classProbs = TRUE` is absolutely crucial for subsequent analysis. Calling `as_caretx()` for caret models without these components raises an error.

Construction of `caret` models is done as usual with the `train()` function. Importantly, the models must be built with formulas and not with the `x` and `y` variable matrices, and the customized `trainControl` object needs to be passed to the `trControl` argument. In the current example, I will fit the requested models with the gradient boosted machine (GBM) algorithm and default tune grids. You are however welcome to test a richer set of combinations of the tuning parameters.

```r
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

```
The final step of the model construction is simple: I'm just calling `as_caretx()` for the models. Of importance, the function returns objects of the `caretx` class, which inherit most of the methods from traditional `caret` models. 

```r

  my_models <- my_models %>%
    map(as_caretx)

```

My personal experience with the anyway excellent `caret` package was that regression and classification models required different and quite often project-specific approaches to diagnostic, performance evaluation and visualization. This was my prime motivation to develop the `caretExtra` package. Another motivation was to create a framework compatible with `tidyverse` environment and `ggplot` graphic interface. For this reasons, the package offers a relatively simple S3 method interface (`model.frame()`, `plot()`, `summary()`, `components()` etc.), returns numeric statistics in `tibble` form and generates `ggplot` graphical objects that can be easily customized by the user.

For instance, `model.frame()` and `formula()` extract respectively the training data and the formula from the model.

```r
> head(model.frame(my_models$regression))

             crim          zn      indus chas        nox         rm         age       dis        rad        tax    ptratio
obs_2  -0.4169267 -0.48724019 -0.5927944   no -0.7395304  0.1940824  0.36680343 0.5566090 -0.8670245 -0.9863534 -0.3027945
obs_6  -0.4166314 -0.48724019 -1.3055857   no -0.8344581  0.2068916 -0.35080997 1.0766711 -0.7521778 -1.1050216  0.1129203
obs_7  -0.4098372  0.04872402 -0.4761823   no -0.2648919 -0.3880270 -0.07015919 0.8384142 -0.5224844 -0.5769480 -1.5037485
obs_8  -0.4032966  0.04872402 -0.4761823   no -0.2648919 -0.1603069  0.97784057 1.0236249 -0.5224844 -0.5769480 -1.5037485
obs_10 -0.4003331  0.04872402 -0.4761823   no -0.2648919 -0.3994130  0.61548134 1.3283202 -0.5224844 -0.5769480 -1.5037485
obs_11 -0.3939564  0.04872402 -0.4761823   no -0.2648919  0.1314594  0.91389483 1.2117800 -0.5224844 -0.5769480 -1.5037485
           black       lstat medv
obs_2  0.4406159 -0.49195252 21.6
obs_6  0.4101651 -1.04229087 28.7
obs_7  0.4263763 -0.03123671 22.9
obs_8  0.4406159  0.90979986 27.1
obs_10 0.3289995  0.62272769 18.9
obs_11 0.3926395  1.09184562 15.0

> formula(my_models$binary)

class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9
attr(,"variables")
list(class, V1, V2, V3, V4, V5, V6, V7, V8, V9)
attr(,"factors")
      V1 V2 V3 V4 V5 V6 V7 V8 V9
class  0  0  0  0  0  0  0  0  0
V1     1  0  0  0  0  0  0  0  0
V2     0  1  0  0  0  0  0  0  0
V3     0  0  1  0  0  0  0  0  0
V4     0  0  0  1  0  0  0  0  0
V5     0  0  0  0  1  0  0  0  0
V6     0  0  0  0  0  1  0  0  0
V7     0  0  0  0  0  0  1  0  0
V8     0  0  0  0  0  0  0  1  0
V9     0  0  0  0  0  0  0  0  1
attr(,"term.labels")
[1] "V1" "V2" "V3" "V4" "V5" "V6" "V7" "V8" "V9"
attr(,"order")
[1] 1 1 1 1 1 1 1 1 1
attr(,"intercept")
[1] 1
attr(,"response")
[1] 1
attr(,"predvars")
list(class, V1, V2, V3, V4, V5, V6, V7, V8, V9)
attr(,"dataClasses")
    class        V1        V2        V3        V4        V5        V6        V7        V8        V9 
 "factor" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"

```
Model residuals are computed by `residuals()`, while `augment()` returns the actual outcome (`.outcome`) with predictions in the training data and out-of-fold predictions in resamples (`.fitted`) together with class assignment probabilities and the explanatory variables. By providing the `residuals()` and `augment()` functions with a test data set passed to the optional `newdata` argument, test set residuals and predictions can be calculated as well:

```r
> residuals(my_models$regression, newdata = my_boston$test)

$train
# A tibble: 338 × 8
   .observation .outcome .fitted .resid .std.resid .sq.std.resid .candidate_missfit .expect.norm
          <int>    <dbl>   <dbl>  <dbl>      <dbl>         <dbl> <chr>                     <dbl>
 1          246     50      34.3 -15.7       -7.66         58.6  yes                       -2.97
 2            4     27.1    20.6  -6.52      -3.18         10.1  yes                       -2.62
 3          244     50      43.7  -6.25      -3.05          9.31 yes                       -2.44
 4          111     36.2    30.1  -6.11      -2.98          8.88 yes                       -2.31
 5          259     23.2    17.2  -5.96      -2.91          8.47 yes                       -2.22
 6          277     17.9    13.3  -4.57      -2.23          4.97 yes                       -2.14
 7          171     50      45.4  -4.56      -2.23          4.95 yes                       -2.07
 8          131     23.7    19.2  -4.49      -2.19          4.79 yes                       -2.01
 9          122     50      45.5  -4.46      -2.17          4.73 yes                       -1.96
10           58     28.4    24.9  -3.45      -1.69          2.84 no                        -1.91
# … with 328 more rows
# ℹ Use `print(n = ...)` to see more rows

$cv
# A tibble: 1,690 × 9
   .observation .outcome .fitted .resample  .resid .std.resid .sq.std.resid .candidate_missfit .expect.norm
          <int>    <dbl>   <dbl> <chr>       <dbl>      <dbl>         <dbl> <chr>                     <dbl>
 1          246       50    23.5 Fold5.Rep3  -26.5      -7.52          56.6 yes                       -3.44
 2          246       50    24.9 Fold1.Rep5  -25.1      -7.12          50.7 yes                       -3.13
 3          246       50    25.5 Fold5.Rep4  -24.5      -6.94          48.2 yes                       -2.97
 4          246       50    25.7 Fold4.Rep2  -24.3      -6.88          47.4 yes                       -2.87
 5          246       50    27.1 Fold4.Rep1  -22.9      -6.49          42.2 yes                       -2.79
 6          244       50    31.2 Fold2.Rep4  -18.8      -5.34          28.5 yes                       -2.72
 7          244       50    32.6 Fold3.Rep1  -17.4      -4.92          24.2 yes                       -2.67
 8          245       50    34.1 Fold2.Rep4  -15.9      -4.49          20.2 yes                       -2.62
 9          245       50    34.6 Fold3.Rep1  -15.4      -4.36          19.0 yes                       -2.57
10          245       50    36.8 Fold5.Rep3  -13.2      -3.73          13.9 yes                       -2.54
# … with 1,680 more rows
# ℹ Use `print(n = ...)` to see more rows

$test
# A tibble: 168 × 8
   .observation .outcome .fitted .resid .std.resid .sq.std.resid .candidate_missfit .expect.norm
          <int>    <dbl>   <dbl>  <dbl>      <dbl>         <dbl> <chr>                     <dbl>
 1          127     50      30.4 -19.6       -5.13         26.4  yes                       -2.75
 2           74     37      30.2  -6.77      -1.75          3.06 no                        -2.37
 3           86     30.1    23.4  -6.72      -1.73          3.01 no                        -2.17
 4           87     50      43.7  -6.28      -1.62          2.62 no                        -2.04
 5           73     32      26.4  -5.55      -1.43          2.03 no                        -1.93
 6          160     19.1    14.3  -4.82      -1.23          1.52 no                        -1.84
 7           77     34.9    30.1  -4.77      -1.22          1.49 no                        -1.77
 8          100     29.1    24.4  -4.70      -1.20          1.44 no                        -1.70
 9           76     34.6    29.9  -4.66      -1.19          1.42 no                        -1.64
10           81     50      45.6  -4.43      -1.13          1.28 no                        -1.58
# … with 158 more rows
# ℹ Use `print(n = ...)` to see more rows

```
```r
> augment(my_models$multi_class)

$train
# A tibble: 119 × 19
   .observ…¹ .outc…² .fitted Barbera Barolo Grign…³ alcohol malic…⁴     ash ash.a…⁵ magne…⁶ tot..…⁷ flavo…⁸ non.f…⁹ proanth col..i…˟
       <int> <fct>   <fct>     <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
 1         1 Barolo  Barolo  1.40e-7   1.00 4.03e-6   0.255 -0.500  -0.822   -2.49   0.0291   0.571   0.738  -0.821  -0.537 -0.290  
 2         2 Barolo  Barolo  5.79e-6   1.00 1.95e-6   0.206  0.0180  1.10    -0.275  0.0996   0.810   1.22   -0.500   2.14   0.269  
 3         3 Barolo  Barolo  3.83e-6   1.00 6.18e-6   1.70  -0.348   0.487   -0.814  0.946    2.49    1.47   -0.981   1.04   1.18   
 4         4 Barolo  Barolo  6.27e-7   1.00 1.97e-6   1.73  -0.420   0.305   -1.47  -0.253    0.332   0.497  -0.500   0.688  0.0840 
 5         5 Barolo  Barolo  1.37e-6   1.00 1.16e-6   1.32  -0.170   0.886   -0.575  1.51     0.491   0.487  -0.420  -0.590 -0.00207
 6         6 Barolo  Barolo  4.92e-7   1.00 8.38e-6   2.27  -0.625  -0.713   -1.65  -0.183    0.810   0.958  -0.580   0.688  0.0625 
 7         7 Barolo  Barolo  9.03e-7   1.00 7.42e-5   1.07  -0.884  -0.350   -1.05  -0.112    1.10    1.13   -1.14    0.460  0.931  
 8         8 Barolo  Barolo  7.73e-6   1.00 5.34e-5   1.37  -0.161  -0.241   -0.455  0.382    1.05    1.30   -1.14    1.39   0.299  
 9         9 Barolo  Barolo  5.56e-7   1.00 2.52e-6   0.935 -0.545   0.159   -1.05  -0.747    0.491   0.738  -0.580   0.390  0.235  
10        10 Barolo  Barolo  8.32e-6   1.00 2.83e-4   2.17  -0.545   0.0867  -2.43  -0.606    1.29    1.67    0.543   2.14   0.149  
# … with 109 more rows, 3 more variables: col..hue <dbl>, OD.ratio <dbl>, proline <dbl>, and abbreviated variable names
#   ¹​.observation, ²​.outcome, ³​Grignolino, ⁴​malic.acid, ⁵​ash.alkalinity, ⁶​magnesium, ⁷​tot..phenols, ⁸​flavonoids,
#   ⁹​non.flav..phenols, ˟​col..int.
# ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

$cv
# A tibble: 595 × 20
   .observa…¹ .outc…² .fitted .resa…³  Barbera Barolo Grign…⁴ alcohol malic…⁵    ash ash.a…⁶ magne…⁷ tot..…⁸ flavo…⁹ non.f…˟ proanth
        <int> <fct>   <fct>   <chr>      <dbl>  <dbl>   <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
 1          1 Barolo  Barolo  Fold5.… 4.01e- 7   1.00 1.77e-5   0.255 -0.500  -0.822  -2.49   0.0291   0.571   0.738  -0.821  -0.537
 2          1 Barolo  Barolo  Fold3.… 2.07e- 7   1.00 1.42e-6   0.255 -0.500  -0.822  -2.49   0.0291   0.571   0.738  -0.821  -0.537
 3          1 Barolo  Barolo  Fold5.… 7.50e-10   1.00 1.86e-7   0.255 -0.500  -0.822  -2.49   0.0291   0.571   0.738  -0.821  -0.537
 4          1 Barolo  Barolo  Fold4.… 1.01e- 7   1.00 3.10e-5   0.255 -0.500  -0.822  -2.49   0.0291   0.571   0.738  -0.821  -0.537
 5          1 Barolo  Barolo  Fold3.… 2.77e- 9   1.00 1.05e-6   0.255 -0.500  -0.822  -2.49   0.0291   0.571   0.738  -0.821  -0.537
 6          2 Barolo  Barolo  Fold1.… 2.27e- 6   1.00 1.12e-6   0.206  0.0180  1.10   -0.275  0.0996   0.810   1.22   -0.500   2.14 
 7          2 Barolo  Barolo  Fold1.… 1.25e- 6   1.00 1.64e-6   0.206  0.0180  1.10   -0.275  0.0996   0.810   1.22   -0.500   2.14 
 8          2 Barolo  Barolo  Fold2.… 5.31e- 7   1.00 3.59e-7   0.206  0.0180  1.10   -0.275  0.0996   0.810   1.22   -0.500   2.14 
 9          2 Barolo  Barolo  Fold3.… 2.19e- 5   1.00 8.17e-6   0.206  0.0180  1.10   -0.275  0.0996   0.810   1.22   -0.500   2.14 
10          2 Barolo  Barolo  Fold5.… 2.74e- 6   1.00 2.02e-6   0.206  0.0180  1.10   -0.275  0.0996   0.810   1.22   -0.500   2.14 
# … with 585 more rows, 4 more variables: col..int. <dbl>, col..hue <dbl>, OD.ratio <dbl>, proline <dbl>, and abbreviated variable
#   names ¹​.observation, ²​.outcome, ³​.resample, ⁴​Grignolino, ⁵​malic.acid, ⁶​ash.alkalinity, ⁷​magnesium, ⁸​tot..phenols, ⁹​flavonoids,
#   ˟​non.flav..phenols
# ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

```

Many other elements of the model like tuning results, square distances to the outcome and confusion matrices can be extracted from the model with the `components()` method. As described above, if `newdata` is specified, the requested metrics and objects are generated not only for the training and cross-validaiton data sets but also for the test data:

```r
> components(my_models$multi_class, what = 'tuning')

  shrinkage interaction.depth n.minobsinnode n.trees  Accuracy     Kappa AccuracySD    KappaSD
1       0.1                 1             10      50 0.9549478 0.9318877 0.03798866 0.05703971
4       0.1                 2             10      50 0.9648870 0.9468514 0.03080134 0.04637411
7       0.1                 3             10      50 0.9598021 0.9389724 0.02532945 0.03850012
2       0.1                 1             10     100 0.9599478 0.9394119 0.03921166 0.05894816
5       0.1                 2             10     100 0.9664870 0.9492163 0.02368318 0.03580442
8       0.1                 3             10     100 0.9596029 0.9386257 0.03318834 0.05055427
3       0.1                 1             10     150 0.9682986 0.9519018 0.03001619 0.04548033
6       0.1                 2             10     150 0.9664870 0.9490494 0.03156632 0.04809489
9       0.1                 3             10     150 0.9613420 0.9414862 0.03433323 0.05161714

```

```r
> components(my_models$binary, what = 'square_dist')

$train
# A tibble: 456 × 4
   .observation .outcome  .fitted   square_dist
          <int> <fct>     <fct>           <dbl>
 1            1 benign    benign    0.00000120 
 2            2 benign    benign    0.0519     
 3            3 benign    benign    0.0759     
 4            4 benign    benign    0.00000704 
 5            5 benign    benign    0.0000145  
 6            6 benign    benign    0.000000831
 7            7 malignant malignant 0.0232     
 8            8 benign    benign    0.000129   
 9            9 malignant malignant 0.000000131
10           10 benign    benign    0.00000120 
# … with 446 more rows
# ℹ Use `print(n = ...)` to see more rows

$cv
# A tibble: 2,280 × 5
   .observation .resample  .outcome .fitted   square_dist
          <int> <chr>      <fct>    <fct>           <dbl>
 1            1 Fold1.Rep3 benign   benign    0.000000516
 2            1 Fold4.Rep1 benign   benign    0.000000222
 3            1 Fold3.Rep2 benign   benign    0.000000922
 4            1 Fold5.Rep4 benign   benign    0.000000351
 5            1 Fold2.Rep5 benign   benign    0.000000645
 6            2 Fold1.Rep4 benign   malignant 0.980      
 7            2 Fold4.Rep1 benign   malignant 0.981      
 8            2 Fold5.Rep2 benign   malignant 0.946      
 9            2 Fold3.Rep3 benign   malignant 0.981      
10            2 Fold5.Rep5 benign   malignant 0.984      
# … with 2,270 more rows
# ℹ Use `print(n = ...)` to see more rows
```
```r
>   components(my_models$binary,
+              what = 'confusion',
+              newdata = my_biopsy$test)

$train
           .fitted
.outcome    benign malignant
  benign       295         1
  malignant      1       159

$cv
           .fitted
.outcome    benign malignant
  benign      1440        40
  malignant     30       770

$test
           .fitted
.outcome    benign malignant
  benign       144         4
  malignant      5        74
```


</details>

### Diagnostic plots and visualization of modeling results

<details>

#### Regression models

By calling `plot(type = 'diagnostic')` for a regression model, a list of `ggplot` objects with diagnostic plots of residuals is returned. Such list includes plots of residuals versus fitted (`resid_fitted`), standardized residuals versus fitted (`std.resid_fitted`), square residuals versus fitted (`sq.resid_fitted`) and quantile-quantile plot of residuals, which can be helpful at assessment of residuals' normality (`qq.std.resid`). Candidate outliers identified with the $2 \times SD$ cutoff are highlighted in red. As before, if `newdata` is specified, diagnostic plots will be generated for the test data set as well.

```r
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
```
```r
  regression_resid_fitted_plots$train +
    regression_resid_fitted_plots$cv +
    regression_resid_fitted_plots$test +
    plot_layout(ncol = 2)
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/324d4988-a47c-405b-b573-709547a7c4ac)

```r
  regression_qqplots$train +
    regression_qqplots$cv +
    regression_qqplots$test +
    plot_layout(ncol = 2)
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/b4954e3a-146f-463f-a864-8bec093f59e2)

In many instances plots of predicted and observed values of the outcome variable are useful, e.g. for assessing model calibration. They are generated by calling `plot(type = 'fit')`. In such plots, the ideal calibration is represented by a slope 1 dashed line. By default, LOESS or GAM trends (`geom_smooth()` from `ggplot2`) are displayed as well; R^2 and RMSE values are displayed in the plot subtitle and numbers of complete observations are indicated in the plot tag:

```r
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
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/aff30e66-4ac9-4429-9953-df9cad1e52b9)

A graphical representation of the key performance statistics: R^2, RMSE and Spearman's $\rho$ coefficient of correlation between the outcome and predictions can be plotted with `plot(type = 'performance')`:

```r
  regression_performance_plots <- plot(my_models$regression,
                                       newdata = my_boston$test,
                                       type = 'performance')

  regression_performance_plots +
    scale_size_area(limits = c(0, 1))
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/46bab64a-ede7-4215-b019-25144e18dbcd)

#### Binary classifiers

Class assignment probability and square distance to the outcome may serve as quality measures for classification models. They can be obtained by calling `plot(type = 'class_p')`. In this case, a list of scatter plots of sorted class assignment probabilities (`winner_p`) and squared distance to the outcome (`square_dist`) is returned. Misclassified observations are highlighted in red. By default, overall accuracy, Cohen's $\kappa$ and Brier score (BS) are displayed in the plot subtitles.

```r
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
```

```r
  binary_sq_dist_plots$train  +
    binary_sq_dist_plots$cv +
    binary_sq_dist_plots$test +
    plot_layout(ncol = 2)
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/9ec8d044-2e24-4b38-af3b-019f1ce4fd9f)

```r
  binary_p_plots$train +
    binary_p_plots$cv +
    binary_p_plots$test +
    plot_layout(ncol = 2)
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/9cf40621-fb99-4dcd-b0bd-5324baf1de3b)

Heat map representations of confusion matrices are fetched by `plot(type = 'confusion')`. By default, such heat maps of confusion matrices visualize observation counts. To make them show percentages, the user can specify `scale = 'percent'`:

```r
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
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/460bfdf0-442e-45f3-9892-1e65b2946f47)

Graphical representation of results of receiver operating characteristic, so called 'ROC curves', is a traditional way to visualize sensitivity, specificity and overall accuracy (so called area under the curve or AUC) of a classifier. Such plots are obtained with `plot(type = 'roc')`. Again, by specifying `newdata`, ROC curves will be plotted also for the test data set. Sensitivity (Se), specificity (Sp) at the `p = 0.5` class assignment probability cutoff as well as AUC are presented in the plots.

```r
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
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/02488e87-ee6b-4398-9abf-c56b56b419aa)

By calling `plot(type = 'performance')`, the most essential performance statistics, overall accuracy, Cohen's $\kappa$ and Brier score, can be displayed in a bubble plot. As with any `ggplot2` object returned by the package's tools, feel free to modify it!

```r
  binary_performance_plots <- plot(my_models$binary,
                                   newdata = my_biopsy$test,
                                   type = 'performance')

  ## indicating kappa and Brier score values expected
  ## for a dummy classifier

  binary_performance_plots +
    geom_hline(yintercept = 0.75, linetype = 'dashed') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    scale_size_area(limits = c(0.5, 1))
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/e69f4e9a-b795-4ff8-b175-85a9ceb4afa6)

#### Multi-category classifiers

In general, the repertoire of plots for multi-category classifiers is similar to binary classifer plots with exception of ROC curves which are available only for the later.

```r
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
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/579bfd08-ac5c-464a-bee4-23062bab9665)

```r
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
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/2d2637b7-2001-4a2e-b539-0f12698926e9)

  
</details>

### Numeric statistics of model performance

<details>

The most crucial model performance statistics can be computed for `caretex` models with the `summary()` method: 

* _regression models_: mean absolute error (`MAE`), mean square error (`MSE`), root mean square error (`RMSE`), pseudo-R^2 metric of explained variance (`rsq`), R^2 computed as square Pearson's correlation coefficient (`caret_rsq`) as well as coefficients of correlation between the outcome and predictions (Pearson's r: `pearson`, Spearman's $\rho$: `spearman`, Kendall's $\tau B$: `kendall`)

* _binary classifiers_: Harrel's concordance index (`c_index`), log loss (`log_loss`), area under the ROC curve (`AUC`), area under the precision-recall curve (`prAUC`), overall accuracy (`correct_rate`), Cohen's $\kappa$ (`kappa`), F1 score (`F1`), sensitivity and specificity (`Se` and `Sp`), positive and negative prdiction value (`PPV` and `NPV`), precision, recall, detection rate (`detection_rate`), balanced accuaracy (`balanced_accuracy`), Brier score (`brier_score`), as well as mean assignment probabilty in the outcome and fitted classes (`class_p_outcome` and `class_p_fitted`)

* _multi-category classifiers_: as above except of Harrel's concordance index. Note that ROC metrics are averaged over all classes. Additionally, Brier score is calculated in a bit different way for multi-category classification models as compared with binary classifiers (see: the note by Goldstein-Greenwood in the reference list)

The function computes the performance statistics for predictions in the training data set and out-of-fold predictions. By specifying the optional `newdata` argument, evaluation of model performance in the test data set will be done as well.

```r
  regression_stats <- summary(my_models$regression,
                              newdata = my_boston$test)

  binary_stats <- summary(my_models$binary,
                          newdata = my_biopsy$test)

  multi_class_stats <- summary(my_models$multi_class,
                               newdata = my_wines$test)
```

```r
> regression_stats

$train
# A tibble: 8 × 4
  statistic estimate lower_ci upper_ci
  <chr>        <dbl> <lgl>    <lgl>   
1 MAE          1.46  NA       NA      
2 MSE          4.19  NA       NA      
3 RMSE         2.05  NA       NA      
4 rsq          0.949 NA       NA      
5 caret_rsq    0.950 NA       NA      
6 pearson      0.975 NA       NA      
7 spearman     0.962 NA       NA      
8 kendall      0.849 NA       NA      

$cv
# A tibble: 8 × 4
  statistic estimate lower_ci upper_ci
  <chr>        <dbl>    <dbl>    <dbl>
1 MAE          2.44     1.97     3.11 
2 MSE         12.4      6.32    22.5  
3 RMSE         3.45     2.51     4.74 
4 rsq          0.850    0.700    0.916
5 caret_rsq    0.855    0.716    0.917
6 pearson      0.924    0.846    0.958
7 spearman     0.908    0.857    0.956
8 kendall      0.762    0.701    0.829

$test
# A tibble: 8 × 4
  statistic estimate lower_ci upper_ci
  <chr>        <dbl> <lgl>    <lgl>   
1 MAE          2.46  NA       NA      
2 MSE         14.3   NA       NA      
3 RMSE         3.79  NA       NA      
4 rsq          0.839 NA       NA      
5 caret_rsq    0.838 NA       NA      
6 pearson      0.916 NA       NA      
7 spearman     0.903 NA       NA      
8 kendall      0.756 NA       NA 
```
```r
> binary_stats

$train
# A tibble: 18 × 4
   statistic         estimate lower_ci upper_ci
   <chr>                <dbl> <lgl>    <lgl>   
 1 c_index            0.995   NA       NA      
 2 log_loss           0.0244  NA       NA      
 3 AUC                1.00    NA       NA      
 4 prAUC              0.990   NA       NA      
 5 correct_rate       0.996   NA       NA      
 6 kappa              0.990   NA       NA      
 7 F1                 0.994   NA       NA      
 8 Se                 0.994   NA       NA      
 9 Sp                 0.997   NA       NA      
10 PPV                0.994   NA       NA      
11 NPV                0.997   NA       NA      
12 precision          0.994   NA       NA      
13 recall             0.994   NA       NA      
14 detection_rate     0.349   NA       NA      
15 balanced_accuracy  0.995   NA       NA      
16 brier_score        0.00527 NA       NA      
17 class_p_outcome    0.983   NA       NA      
18 class_p_fitted     0.983   NA       NA      

$cv
# A tibble: 18 × 4
   statistic         estimate lower_ci upper_ci
   <chr>                <dbl>    <dbl>    <dbl>
 1 c_index             0.968   0.926     1     
 2 log_loss            0.128   0.0242    0.306 
 3 AUC                 0.990   0.973     1     
 4 prAUC               0.929   0.782     0.975 
 5 correct_rate        0.969   0.930     1     
 6 kappa               0.933   0.846     1     
 7 F1                  0.957   0.901     1     
 8 Se                  0.962   0.913     1     
 9 Sp                  0.973   0.932     1     
10 PPV                 0.952   0.884     1     
11 NPV                 0.980   0.952     1     
12 precision           0.952   0.884     1     
13 recall              0.962   0.913     1     
14 detection_rate      0.338   0.321     0.352 
15 balanced_accuracy   0.968   0.926     1     
16 brier_score         0.0284  0.00516   0.0654
17 class_p_outcome     0.983   0.970     0.990 
18 class_p_fitted      0.982   0.968     0.991 

$test
# A tibble: 18 × 4
   statistic         estimate lower_ci upper_ci
   <chr>                <dbl> <lgl>    <lgl>   
 1 c_index             0.955  NA       NA      
 2 log_loss            0.0972 NA       NA      
 3 AUC                 0.995  NA       NA      
 4 prAUC               0.984  NA       NA      
 5 correct_rate        0.960  NA       NA      
 6 kappa               0.912  NA       NA      
 7 F1                  0.943  NA       NA      
 8 Se                  0.937  NA       NA      
 9 Sp                  0.973  NA       NA      
10 PPV                 0.949  NA       NA      
11 NPV                 0.966  NA       NA      
12 precision           0.949  NA       NA      
13 recall              0.937  NA       NA      
14 detection_rate      0.326  NA       NA      
15 balanced_accuracy   0.955  NA       NA      
16 brier_score         0.0300 NA       NA      
17 class_p_outcome     0.979  NA       NA      
18 class_p_fitted      0.978  NA       NA
```

```r
> multi_class_stats

$train
# A tibble: 17 × 4
   statistic           estimate lower_ci upper_ci
   <chr>                  <dbl> <lgl>    <lgl>   
 1 log_loss          0.000368   NA       NA      
 2 AUC               1          NA       NA      
 3 prAUC             0.974      NA       NA      
 4 correct_rate      1          NA       NA      
 5 kappa             1          NA       NA      
 6 F1                1          NA       NA      
 7 Se                1          NA       NA      
 8 Sp                1          NA       NA      
 9 PPV               1          NA       NA      
10 NPV               1          NA       NA      
11 precision         1          NA       NA      
12 recall            1          NA       NA      
13 detection_rate    0.333      NA       NA      
14 balanced_accuracy 1          NA       NA      
15 brier_score       0.00000195 NA       NA      
16 class_p_outcome   1.00       NA       NA      
17 class_p_fitted    1.00       NA       NA      

$cv
# A tibble: 17 × 4
   statistic         estimate lower_ci upper_ci
   <chr>                <dbl>    <dbl>    <dbl>
 1 log_loss            0.145   0.0103     0.533
 2 AUC                 0.998   0.987      1    
 3 prAUC               0.868   0.856      0.877
 4 correct_rate        0.968   0.915      1    
 5 kappa               0.952   0.871      1    
 6 F1                  0.969   0.919      1    
 7 Se                  0.971   0.925      1    
 8 Sp                  0.984   0.957      1    
 9 PPV                 0.970   0.917      1    
10 NPV                 0.984   0.955      1    
11 precision           0.970   0.917      1    
12 recall              0.971   0.925      1    
13 detection_rate      0.323   0.305      0.333
14 balanced_accuracy   0.978   0.941      1    
15 brier_score         0.0596  0.00241    0.164
16 class_p_outcome     0.975   0.945      0.995
17 class_p_fitted      0.975   0.943      0.994

$test
# A tibble: 17 × 4
   statistic         estimate lower_ci upper_ci
   <chr>                <dbl> <lgl>    <lgl>   
 1 log_loss           0.00710 NA       NA      
 2 AUC                1       NA       NA      
 3 prAUC              0.947   NA       NA      
 4 correct_rate       1       NA       NA      
 5 kappa              1       NA       NA      
 6 F1                 1       NA       NA      
 7 Se                 1       NA       NA      
 8 Sp                 1       NA       NA      
 9 PPV                1       NA       NA      
10 NPV                1       NA       NA      
11 precision          1       NA       NA      
12 recall             1       NA       NA      
13 detection_rate     0.333   NA       NA      
14 balanced_accuracy  1       NA       NA      
15 brier_score        0.00179 NA       NA      
16 class_p_outcome    0.992   NA       NA      
17 class_p_fitted     0.992   NA       NA 
```
  
</details>

### Evaluation of performance in data subsets

<details>

There are cases, when you would like to have a more detailed look at predictions of a machine learnig model in a particular suset of subsets of the data. This can be conveniently done by `split()` applied to a `caretx` model. The splitting factor - a categorical variable present in the training and, optionally, test data set - is speficied by the `f` argument. The `split()` method returns a plain list of prediction objects, which can be plotted or evaluated with `plot()` and `summary()` as described above. In this particular example, we would like to know, how the Boston house price model wors for objects located at and beyond the Charles River bank (coded by the `chas` variable in the `Boston` data set):

```r
  regression_chas <- split(my_models$regression,
                           f = chas,
                           newdata = my_boston$test)

  regression_chas_stats <- regression_chas %>%
    map(summary)
```

```r
  regression_chas_stats[c("cv.no", "cv.yes")] %>%
    map(filter, statistic == 'MAE')

$cv.no
# A tibble: 1 × 4
  statistic estimate lower_ci upper_ci
  <chr>        <dbl>    <dbl>    <dbl>
1 MAE           2.46     2.00     3.06

$cv.yes
# A tibble: 1 × 4
  statistic estimate lower_ci upper_ci
  <chr>        <dbl>    <dbl>    <dbl>
1 MAE           2.18    0.897     4.96
```
</details>

### Quality of detection in outcome variable classes

<details>
In many cases it is worthwhile to check how a multi-category classification model performs in the outcome variable classes. ROC metrics for particular classes obtained by the one versus rest comparisons can be easily computed with `clstats()`. Additionally, by calling `clplots()` for a multi-class model, ROC plots for all classes of the response variable are generated. Both functions will return the statistics or plots in a list for the training and out-of-fold predictions. By providing a data frame to the `newdata` argument, the ROC metrics and plots will be returned for the test data set as well.

```r
> clstats(my_models$multi_class, newdata = my_wines$test)

$train
# A tibble: 3 × 14
  .outcome   correct_rate kappa    F1    Se    Sp   PPV   NPV precision recall detection_rate balanced_accuracy  brier_score class_p
  <fct>             <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl>          <dbl>             <dbl>        <dbl>   <dbl>
1 Barbera               1     1     1     1     1     1     1         1      1          0.269                 1 0.0000000177    1.00
2 Barolo                1     1     1     1     1     1     1         1      1          0.328                 1 0.0000000177    1.00
3 Grignolino            1     1     1     1     1     1     1         1      1          0.403                 1 0.00000772      1.00

$cv
# A tibble: 3 × 14
  .outcome   correct_rate kappa    F1    Se    Sp   PPV   NPV precision recall detection_rate balanced_accuracy brier_score class_p
  <fct>             <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl>          <dbl>             <dbl>       <dbl>   <dbl>
1 Barbera           0.988 0.970 0.979 1     0.984 0.958 1         0.958  1              0.269             0.992    0.000302   0.997
2 Barolo            0.997 0.992 0.995 0.995 0.998 0.995 0.998     0.995  0.995          0.326             0.996    0.0103     0.988
3 Grignolino        0.988 0.975 0.985 0.971 1     1     0.981     1      0.971          0.392             0.985    0.0501     0.977

$test
# A tibble: 3 × 14
  .outcome   correct_rate kappa    F1    Se    Sp   PPV   NPV precision recall detection_rate balanced_accuracy brier_score class_p
  <fct>             <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl>          <dbl>             <dbl>       <dbl>   <dbl>
1 Barbera           1     1     1     1     1      1    1          1     1              0.276             1        0.00531    0.986
2 Barolo            0.983 0.961 0.974 1     0.974  0.95 1          0.95  1              0.328             0.987    0.000272   0.997
3 Grignolino        0.983 0.964 0.978 0.957 1      1    0.972      1     0.957          0.379             0.978    0.0873     0.995

```

```r
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
```
![image](https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/9aac3cf6-7ef4-424e-a29c-f940424d3fdd)
  
</details>

## References
1. Kuhn M. Building predictive models in R using the caret package. J Stat Softw (2008) 28:1–26. doi:10.18637/jss.v028.i05
2. Friedman JH. Greedy function approximation: A gradient boosting machine. https://doi.org/101214/aos/1013203451 (2001) 29:1189–1232. doi:10.1214/AOS/1013203451
3. Cohen J. A Coefficient of Agreement for Nominal Scales. Educ Psychol Meas (1960) 20:37–46. doi:10.1177/001316446002000104
4. McHugh ML. Interrater reliability: the kappa statistic. Biochem Medica (2012) 22:276. doi:10.11613/bm.2012.031
5. Brier GW. VERIFICATION OF FORECASTS EXPRESSED IN TERMS OF PROBABILITY. Mon Weather Rev (1950) 78:1–3. doi:10.1175/1520-0493(1950)078<0001:vofeit>2.0.co;2
6. Goldstein-Greenwood J. A Brief on Brier Scores | UVA Library. (2021) Available at: https://library.virginia.edu/data/articles/a-brief-on-brier-scores [Accessed September 5, 2023]
