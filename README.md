
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boostime

<img src="vignettes/logo-boostime.png" width="147" height="170" align="right"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/boostime)](https://CRAN.R-project.org/package=boostime)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/AlbertoAlmuinha/boostime/workflows/R-CMD-check/badge.svg)](https://github.com/AlbertoAlmuinha/boostime/actions)
<!-- badges: end -->

> The Tidymodels Extension for Time Series Boosting Models

## Tutorials 📚

-   [**Getting Started with
    Boostime**](https://albertoalmuinha.github.io/boostime/articles/getting-started.html):
    A walkthrough of the tidy modeling approach with the package.

-   [**Boostime: Combining ARIMA with
    Catboost**](https://albertoalmuinha.com/posts/2021-05-30-boostime_arima_catboost/boostime-arima-catboost/)
    Introduction to Boostime with a practical example using ARIMA and
    Catboost.

-   [**Hyperparameter Tuning with
    Boostime**](https://albertoalmuinha.com/posts/2021-06-28-boostime-tuning/parameter-tuning-boostime/)
    Learn how to select the best hyperparameters for the Boostime
    algorithms.

## Installation ⚙️

Not on CRAN yet (and predictably won’t be because Catboost doesn’t look
like it’s going to be any time soon)

``` r
#install.packages("boostime")
```

Development version:

``` r
# install.packages("devtools")
devtools::install_github("AlbertoAlmuinha/boostime")
```

## Why Boostime? 📊

> Boostime unlocks boosting methods to improve modeling errors
> (residuals) on time series analysis.

The following algorithms are available:

-   **Arima + Catboost**: You can use either an automatic version of
    Arima (auto.arima, in which orders are selected from KPSS unit root
    tests or the manual version) in conjunction with Catboost to model
    the residuals. One of the great advantages of this model over
    XGBoost is that it can deal with categorical variables and you can
    use GPU without any configuration effort.

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

## Acknowledgments 👏

I would especially like to thank Matt Dancho for his contribution to the
world of time series, in particular for the creation of the
[Modeltime](https://github.com/business-science/modeltime) package.
Without this package, **Boostime** would never have been possible.
THANKS!

## Contact ✉

Alberto Almuiña,
[Linkedin](https://www.linkedin.com/in/alberto-almui%C3%B1a-b1176881/),
[Twitter](https://twitter.com/AlmuinaAlberto),
[Github](https://github.com/AlbertoAlmuinha),
[Blog](https://albertoalmuinha.com/es/).
