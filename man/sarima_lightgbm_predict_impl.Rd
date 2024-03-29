% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsnip-arima_boost.R
\name{sarima_lightgbm_predict_impl}
\alias{sarima_lightgbm_predict_impl}
\title{Bridge prediction Function for ARIMA-Lightgbm Models}
\usage{
sarima_lightgbm_predict_impl(object, new_data, categorical_cols, ...)
}
\arguments{
\item{object}{An object of class \code{model_fit}}

\item{new_data}{A rectangular data object, such as a data frame.}

\item{categorical_cols}{Categorical variables}

\item{...}{Additional arguments passed to \code{catboost.predict()}}
}
\description{
Bridge prediction Function for ARIMA-Lightgbm Models
}
