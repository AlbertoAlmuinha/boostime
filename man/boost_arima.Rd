% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsnip-arima_boost.R
\name{boost_arima}
\alias{boost_arima}
\title{General Interface for Boosted ARIMA Regression Models}
\usage{
boost_arima(
  mode = "regression",
  seasonal_period = NULL,
  non_seasonal_ar = NULL,
  non_seasonal_differences = NULL,
  non_seasonal_ma = NULL,
  seasonal_ar = NULL,
  seasonal_differences = NULL,
  seasonal_ma = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  sample_size = NULL,
  loss_reduction = NULL
)
}
\arguments{
\item{mode}{A single character string for the type of model.
The only possible value for this model is "regression".}

\item{seasonal_period}{A seasonal frequency. Uses "auto" by default.
A character phrase of "auto" or time-based phrase of "2 weeks"
can be used if a date or date-time variable is provided.
See Fit Details below.}

\item{non_seasonal_ar}{The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.}

\item{non_seasonal_differences}{The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.}

\item{non_seasonal_ma}{The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.}

\item{seasonal_ar}{The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.}

\item{seasonal_differences}{The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.}

\item{seasonal_ma}{The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.}

\item{tree_depth}{The maximum depth of the tree (i.e. number of splits).}

\item{learn_rate}{The rate at which the boosting algorithm adapts from iteration-to-iteration.}

\item{mtry}{The number of predictors that will be randomly sampled at each split when creating the tree models.}

\item{trees}{The number of trees contained in the ensemble.}

\item{min_n}{The minimum number of data points in a node that is required for the node to be split further.}

\item{sample_size}{The amount of data exposed to the fitting routine.}

\item{loss_reduction}{The reduction in the loss function required to split further.}
}
\description{
\code{boost_arima()} is a way to generate a \emph{specification} of a time series model
that uses boosting to improve modeling errors (residuals) on Exogenous Regressors.
It works with both "automated" ARIMA (\code{auto.arima}) and standard ARIMA (\code{arima}).
The main algorithms are:
\itemize{
\item Auto ARIMA + Catboost Errors (engine = \code{auto_arima_catboost}, default)
\item ARIMA + Catboost Errors (engine = \code{arima_catboost})
\item Auto ARIMA + LightGBM Errors (engine = \code{auto_arima_lightgbm})
\item ARIMA + LightGBM Errors (engine = \code{arima_lightgbm})
}
}
\details{
The data given to the function are not saved and are only used
to determine the \emph{mode} of the model. For \code{boost_arima()}, the
mode will always be "regression".

The model can be created using the \code{fit()} function using the
following \emph{engines}:
\itemize{
\item "auto_arima_catboost" (default) - Connects to \code{\link[forecast:auto.arima]{forecast::auto.arima()}} and \link[catboost:catboost.train]{catboost::catboost.train}
\item "arima_catboost" - Connects to \code{\link[forecast:Arima]{forecast::Arima()}} and \link[catboost:catboost.train]{catboost::catboost.train}
\item "auto_arima_lightgbm" - Connects to \code{\link[forecast:auto.arima]{forecast::auto.arima()}} and \code{\link[lightgbm:lgb.train]{lightgbm::lgb.train()}}
\item "arima_lightgbm" - Connects to \code{\link[forecast:Arima]{forecast::Arima()}} and \code{\link[lightgbm:lgb.train]{lightgbm::lgb.train()}}
}

\strong{Main Arguments}

The main arguments (tuning parameters) for the \strong{ARIMA model} are:
\itemize{
\item \code{seasonal_period}: The periodic nature of the seasonality. Uses "auto" by default.
\item \code{non_seasonal_ar}: The order of the non-seasonal auto-regressive (AR) terms.
\item \code{non_seasonal_differences}: The order of integration for non-seasonal differencing.
\item \code{non_seasonal_ma}: The order of the non-seasonal moving average (MA) terms.
\item \code{seasonal_ar}: The order of the seasonal auto-regressive (SAR) terms.
\item \code{seasonal_differences}: The order of integration for seasonal differencing.
\item \code{seasonal_ma}: The order of the seasonal moving average (SMA) terms.
}

The main arguments (tuning parameters) for the model \strong{Catboost/LightGBM model} are:
\itemize{
\item \code{tree_depth}: The maximum depth of the tree (i.e. number of splits).
\item \code{learn_rate}: The rate at which the boosting algorithm adapts from iteration-to-iteration.
\item \code{mtry}: The number of predictors that will be randomly sampled at each split when creating the tree models.
\item \code{trees}: The number of trees contained in the ensemble.
\item \code{min_n}: The minimum number of data points in a node that is required for the node to be split further.
\item \code{sample_size}: The amount of data exposed to the fitting routine.
\item \code{loss_reduction}: The reduction in the loss function required to split further.
}

These arguments are converted to their specific names at the
time that the model is fit.

Other options and argument can be
set using \code{set_engine()} (See Engine Details below).

If parameters need to be modified, \code{update()} can be used
in lieu of recreating the object from scratch.
}
\section{Engine Details}{


The standardized parameter names in \code{boostime} can be mapped to their original names in each engine:

Model 1: ARIMA:\tabular{lll}{
   boostime \tab forecast::auto.arima \tab forecast::Arima \cr
   seasonal_period \tab ts(frequency) \tab ts(frequency) \cr
   non_seasonal_ar, non_seasonal_differences, non_seasonal_ma \tab max.p(5), max.d(2), max.q(5) \tab order = c(p(0), d(0), q(0)) \cr
   seasonal_ar, seasonal_differences, seasonal_ma \tab max.P(2), max.D(1), max.Q(2) \tab seasonal = c(P(0), D(0), Q(0)) \cr
}


Model 2: Catboost / LightGBM:\tabular{lll}{
   boostime \tab catboost::catboost.train \tab lightgbm::lgb.train \cr
   tree_depth \tab depth \tab max_depth \cr
   learn_rate \tab learning_rate \tab learning_rate \cr
   mtry \tab rsm \tab feature_fraction \cr
   trees \tab iterations \tab num_iterations \cr
   min_n \tab min_data_in_leaf \tab min_data_in_leaf \cr
   loss_reduction \tab None \tab min_gain_to_split \cr
   sample_size \tab subsample \tab bagging_fraction \cr
}


Other options can be set using \code{set_engine()}.

\strong{auto_arima_catboost (default engine)}

Model 1: Auto ARIMA (\code{forecast::auto.arima}):\preformatted{## function (y, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, 
##     max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1, 
##     start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = c("aicc", "aic", 
##         "bic"), stepwise = TRUE, nmodels = 94, trace = FALSE, approximation = (length(x) > 
##         150 | frequency(x) > 12), method = NULL, truncate = NULL, xreg = NULL, 
##     test = c("kpss", "adf", "pp"), test.args = list(), seasonal.test = c("seas", 
##         "ocsb", "hegy", "ch"), seasonal.test.args = list(), allowdrift = TRUE, 
##     allowmean = TRUE, lambda = NULL, biasadj = FALSE, parallel = FALSE, 
##     num.cores = 2, x = y, ...)
}

Parameter Notes:
\itemize{
\item All values of nonseasonal pdq and seasonal PDQ are maximums.
The \code{auto.arima} will select a value using these as an upper limit.
\item \code{xreg} - This should not be used since Catboost will be doing the regression
}

Model 2: Catboost (\code{catboost::catboost.train}):\preformatted{## function (learn_pool, test_pool = NULL, params = list())
}

Parameter Notes:
\itemize{
\item Catboost uses a \code{params = list()} to capture.
Parsnip / Timeboost automatically sends any args provided as \code{...} inside of \code{set_engine()} to
the \code{params = list(...)}.
}
}

\section{Fit Details}{


\strong{Date and Date-Time Variable}

It's a requirement to have a date or date-time variable as a predictor.
The \code{fit()} interface accepts date and date-time features and handles them internally.
\itemize{
\item \code{fit(y ~ date)}
}

\emph{Seasonal Period Specification}

The period can be non-seasonal (\code{seasonal_period = 1}) or seasonal (e.g. \code{seasonal_period = 12} or \code{seasonal_period = "12 months"}).
There are 3 ways to specify:
\enumerate{
\item \code{seasonal_period = "auto"}: A period is selected based on the periodicity of the data (e.g. 12 if monthly)
\item \code{seasonal_period = 12}: A numeric frequency. For example, 12 is common for monthly data
\item \code{seasonal_period = "1 year"}: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
}

\strong{Univariate (No xregs, Exogenous Regressors):}

For univariate analysis, you must include a date or date-time feature. Simply use:
\itemize{
\item Formula Interface (recommended): \code{fit(y ~ date)} will ignore xreg's.
}

\strong{Multivariate (xregs, Exogenous Regressors)}

The \code{xreg} parameter is populated using the \code{fit()} or \code{fit_xy()} function:
\itemize{
\item Only \code{factor}, \verb{ordered factor}, and \code{numeric} data will be used as xregs.
\item Date and Date-time variables are not used as xregs
\item \code{character} data should be converted to factor.
}

\emph{Xreg Example:} Suppose you have 3 features:
\enumerate{
\item \code{y} (target)
\item \code{date} (time stamp),
\item \code{month.lbl} (labeled month as a ordered factor).
}

The \code{month.lbl} is an exogenous regressor that can be passed to the \code{arima_boost()} using
\code{fit()}:
\itemize{
\item \code{fit(y ~ date + month.lbl)} will pass \code{month.lbl} on as an exogenous regressor.
\item \code{fit_xy(data[,c("date", "month.lbl")], y = data$y)} will pass x, where x is a data frame containing \code{month.lbl}
and the \code{date} feature. Only \code{month.lbl} will be used as an exogenous regressor.
}

Note that date or date-time class values are excluded from \code{xreg}.
}

\examples{
library(tidyverse)
library(lubridate)
library(parsnip)
library(rsample)
library(timetk)
library(boostime)


# Data
m750 <- m4_monthly \%>\% filter(id == "M750")

# Split Data 80/20
splits <- initial_time_split(m750, prop = 0.9)

# MODEL SPEC ----

# Set engine and boosting parameters
model_spec <- boost_arima(

    # ARIMA args
    seasonal_period = 12,
    non_seasonal_ar = 0,
    non_seasonal_differences = 1,
    non_seasonal_ma = 1,
    seasonal_ar     = 0,
    seasonal_differences = 1,
    seasonal_ma     = 1,

    # Catboost Args
    tree_depth = 6,
    learn_rate = 0.1
) \%>\%
    set_engine(engine = "arima_catboost")

# FIT ----
model_fit_boosted <- model_spec \%>\%
    fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

model_fit_boosted


}
\seealso{
\code{\link[=fit.model_spec]{fit.model_spec()}}, \code{\link[=set_engine]{set_engine()}}
}
