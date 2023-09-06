<img src="https://github.com/PiotrTymoszuk/caretExtra/assets/80723424/e5fc7557-ece3-4e38-ac16-b3bf97d1be6a" width="20%" height="20%" align = "right">

# caretExtra
Tools for quality control, prediction and plotting for caret models

## Description

The `caretExtra` package includes a bunch of methods for user friendly extraction of the predictions in the train, cross-validation and test data, fit statistic calulation, diagnostic via residuals plots and graphical representation of the results as scatter, ROC and heat map plots. In addition, regression models may be calibrated by quantile GAM (generalized additive model) method.

Currently, it works only with cross-validated Caret models (CV or repreated CV) created from formula objects. Solution for bootstrap and holdout model construction methods are on the way.

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

`clustTools` uses tools provided by the [rlang](https://rlang.r-lib.org/), [tidyverse](https://www.tidyverse.org/), [caret](https://topepo.github.io/caret/), [coxed](https://cran.r-project.org/web/packages/coxed/index.html), [ggrepel](https://ggrepel.slowkow.com/), [generics](https://github.com/r-lib/generics), [DescTools](https://andrisignorell.github.io/DescTools/), [plotROC](https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html), [qgam](https://mfasiolo.github.io/qgam/), and [survial](https://cran.r-project.org/web/packages/survival/index.html). Many thanks to their developers, maintainers and contributors.

