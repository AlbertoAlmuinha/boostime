
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boostime

<img src="vignettes/logo-boostime.png" width="147" height="170" align="right"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/boostime)](https://CRAN.R-project.org/package=boostime)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/AlbertoAlmuinha/boostime/workflows/R-CMD-check/badge.svg)](https://github.com/AlbertoAlmuinha/boostime/actions)
<!-- badges: end -->

> The Tidymodels Extension for Time Series Boosting Models

## Tutorials

-   [**Getting Started with
    Boostime**](https://albertoalmuinha.github.io/boostime/articles/getting-started.html):
    A walkthrough of the tidy modeling approach with the package.

## Installation

Not on CRAN yet:

``` r
#install.packages("boostime")
```

Development version:

``` r
# install.packages("devtools")
devtools::install_github("AlbertoAlmuinha/boostime")
```

## Why Boostime?

> Boostime unlocks boosting methods to improve modeling errors
> (residuals) on time series analysis.

The following algorithms are available:

-   **Arima + Catboost**: You can use either an automatic version of
    Arima (auto.arima, in which orders are selected from KPSS unit root
    tests or the manual version) in conjunction with Catboost to model
    the residuals. One of the great advantages of this model over
    XGBoost is that it can deal with categorical variables and use GPUs.

-   **Prophet + Catboost**: It uses Prophet and Catboost to model the
    residuals. One advantage of Prophet over Arima is that it can handle
    multiple seasonalities.

-   **Arima + LightGBM**: You can use either an automatic version of
    Arima (auto.arima, in which orders are selected from KPSS unit root
    tests or the manual version) in conjunction with LightGBM to model
    the residuals.

-   **Prophet + LightGBM**: It uses Prophet and LightGBM to model the
    residuals. One advantage of Prophet over Arima is that it can handle
    multiple seasonalities.
