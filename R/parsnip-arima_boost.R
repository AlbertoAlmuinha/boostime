# ARIMA BOOST ----

#' General Interface for Boosted ARIMA Regression Models
#'
#' `boost_arima()` is a way to generate a _specification_ of a time series model
#'  that uses boosting to improve modeling errors (residuals) on Exogenous Regressors.
#'  It works with both "automated" ARIMA (`auto.arima`) and standard ARIMA (`arima`).
#'  The main algorithms are:
#'  - Auto ARIMA + Catboost Errors (engine = `auto_arima_catboost`, default)
#'  - ARIMA + Catboost Errors (engine = `arima_catboost`)
#'  - Auto ARIMA + LightGBM Errors (engine = `auto_arima_lightgbm`)
#'  - ARIMA + LightGBM Errors (engine = `arima_lightgbm`)
#'
#'
#' @inheritParams parsnip::boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param seasonal_period A seasonal frequency. Uses "auto" by default.
#'  A character phrase of "auto" or time-based phrase of "2 weeks"
#'  can be used if a date or date-time variable is provided.
#'  See Fit Details below.
#' @param non_seasonal_ar The order of the non-seasonal auto-regressive (AR) terms. Often denoted "p" in pdq-notation.
#' @param non_seasonal_differences The order of integration for non-seasonal differencing. Often denoted "d" in pdq-notation.
#' @param non_seasonal_ma The order of the non-seasonal moving average (MA) terms. Often denoted "q" in pdq-notation.
#' @param seasonal_ar The order of the seasonal auto-regressive (SAR) terms. Often denoted "P" in PDQ-notation.
#' @param seasonal_differences The order of integration for seasonal differencing. Often denoted "D" in PDQ-notation.
#' @param seasonal_ma The order of the seasonal moving average (SMA) terms. Often denoted "Q" in PDQ-notation.
#' @param tree_depth The maximum depth of the tree (i.e. number of splits).
#' @param learn_rate The rate at which the boosting algorithm adapts from iteration-to-iteration.
#' @param mtry The number of predictors that will be randomly sampled at each split when creating the tree models.
#' @param trees The number of trees contained in the ensemble.
#' @param min_n The minimum number of data points in a node that is required for the node to be split further.
#' @param sample_size The amount of data exposed to the fitting routine.
#' @param loss_reduction The reduction in the loss function required to split further.
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `boost_arima()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "auto_arima_catboost" (default) - Connects to [forecast::auto.arima()] and [catboost::catboost.train]
#'  
#'  - "arima_catboost" - Connects to [forecast::Arima()] and [catboost::catboost.train]
#'  
#'  - "auto_arima_lightgbm" - Connects to [forecast::auto.arima()] and [lightgbm::lgb.train()]
#'  
#'  - "arima_lightgbm" - Connects to [forecast::Arima()] and [lightgbm::lgb.train()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the __ARIMA model__ are:
#'
#'  - `seasonal_period`: The periodic nature of the seasonality. Uses "auto" by default.
#'  - `non_seasonal_ar`: The order of the non-seasonal auto-regressive (AR) terms.
#'  - `non_seasonal_differences`: The order of integration for non-seasonal differencing.
#'  - `non_seasonal_ma`: The order of the non-seasonal moving average (MA) terms.
#'  - `seasonal_ar`: The order of the seasonal auto-regressive (SAR) terms.
#'  - `seasonal_differences`: The order of integration for seasonal differencing.
#'  - `seasonal_ma`: The order of the seasonal moving average (SMA) terms.
#'
#' The main arguments (tuning parameters) for the model __Catboost/LightGBM model__ are:
#'
#'  - `tree_depth`: The maximum depth of the tree (i.e. number of splits).
#'  - `learn_rate`: The rate at which the boosting algorithm adapts from iteration-to-iteration.
#'  - `mtry`: The number of predictors that will be randomly sampled at each split when creating the tree models.
#'  - `trees`: The number of trees contained in the ensemble.
#'  - `min_n`: The minimum number of data points in a node that is required for the node to be split further.
#'  - `sample_size`: The amount of data exposed to the fitting routine.
#'  - `loss_reduction`: The reduction in the loss function required to split further.
#'
#'
#' These arguments are converted to their specific names at the
#'  time that the model is fit.
#'
#' Other options and argument can be
#'  set using `set_engine()` (See Engine Details below).
#'
#' If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `boostime` can be mapped to their original names in each engine:
#'
#' Model 1: ARIMA:
#'
#' ```{r echo = FALSE}
#' # parsnip::convert_args("arima_reg")
#' tibble::tribble(
#'     ~ "boostime", ~ "forecast::auto.arima", ~ "forecast::Arima",
#'     "seasonal_period", "ts(frequency)", "ts(frequency)",
#'     "non_seasonal_ar, non_seasonal_differences, non_seasonal_ma", "max.p(5), max.d(2), max.q(5)", "order = c(p(0), d(0), q(0))",
#'     "seasonal_ar, seasonal_differences, seasonal_ma", "max.P(2), max.D(1), max.Q(2)", "seasonal = c(P(0), D(0), Q(0))"
#' ) %>% knitr::kable()
#' ```
#'
#' Model 2: Catboost / LightGBM:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "boostime", ~ "catboost::catboost.train", ~ "lightgbm::lgb.train",
#'     "tree_depth", "depth", "max_depth",
#'     "learn_rate", "learning_rate", "learning_rate",
#'     "mtry", "rsm", "feature_fraction",
#'     "trees", "iterations", "num_iterations",
#'     "min_n", "min_data_in_leaf", "min_data_in_leaf",
#'     "loss_reduction", "None", "min_gain_to_split",
#'     "sample_size", "subsample", "bagging_fraction"
#' ) %>% knitr::kable()
#' ```
#' 
#' Other options can be set using `set_engine()`.
#'
#' __auto_arima_catboost (default engine)__
#'
#' Model 1: Auto ARIMA (`forecast::auto.arima`):
#' ```{r echo = FALSE}
#' str(forecast::auto.arima)
#' ```
#'
#' Parameter Notes:
#' - All values of nonseasonal pdq and seasonal PDQ are maximums.
#'  The `auto.arima` will select a value using these as an upper limit.
#' - `xreg` - This should not be used since Catboost will be doing the regression
#'
#' Model 2: Catboost (`catboost::catboost.train`):
#' ```{r echo = FALSE}
#' str(catboost::catboost.train)
#' ```
#'
#' Parameter Notes:
#' - Catboost uses a `params = list()` to capture.
#'  Parsnip / Timeboost automatically sends any args provided as `...` inside of `set_engine()` to
#'  the `params = list(...)`.
#'
#'
#'
#' @section Fit Details:
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#' - `fit(y ~ date)`
#'
#' _Seasonal Period Specification_
#'
#' The period can be non-seasonal (`seasonal_period = 1`) or seasonal (e.g. `seasonal_period = 12` or `seasonal_period = "12 months"`).
#' There are 3 ways to specify:
#'
#' 1. `seasonal_period = "auto"`: A period is selected based on the periodicity of the data (e.g. 12 if monthly)
#' 2. `seasonal_period = 12`: A numeric frequency. For example, 12 is common for monthly data
#' 3. `seasonal_period = "1 year"`: A time-based phrase. For example, "1 year" would convert to 12 for monthly data.
#'
#'
#' __Univariate (No xregs, Exogenous Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'
#' __Multivariate (xregs, Exogenous Regressors)__
#'
#'  The `xreg` parameter is populated using the `fit()` or `fit_xy()` function:
#'
#'  - Only `factor`, `ordered factor`, and `numeric` data will be used as xregs.
#'  - Date and Date-time variables are not used as xregs
#'  - `character` data should be converted to factor.
#'
#'  _Xreg Example:_ Suppose you have 3 features:
#'
#'  1. `y` (target)
#'  2. `date` (time stamp),
#'  3. `month.lbl` (labeled month as a ordered factor).
#'
#'  The `month.lbl` is an exogenous regressor that can be passed to the `arima_boost()` using
#'  `fit()`:
#'
#'  - `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous regressor.
#'  - `fit_xy(data[,c("date", "month.lbl")], y = data$y)` will pass x, where x is a data frame containing `month.lbl`
#'   and the `date` feature. Only `month.lbl` will be used as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(boostime)
#'
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.9)
#'
#' # MODEL SPEC ----
#'
#' # Set engine and boosting parameters
#' model_spec <- boost_arima(
#'
#'     # ARIMA args
#'     seasonal_period = 12,
#'     non_seasonal_ar = 0,
#'     non_seasonal_differences = 1,
#'     non_seasonal_ma = 1,
#'     seasonal_ar     = 0,
#'     seasonal_differences = 1,
#'     seasonal_ma     = 1,
#'
#'     # Catboost Args
#'     tree_depth = 6,
#'     learn_rate = 0.1
#' ) %>%
#'     set_engine(engine = "arima_catboost")
#'
#' # FIT ----
#' model_fit_boosted <- model_spec %>%
#'     fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
#'         data = training(splits))
#'
#' model_fit_boosted
#'
#'
#' @export
boost_arima <- function(mode = "regression", seasonal_period = NULL,
                        non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                        seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL,
                        tree_depth = NULL, learn_rate = NULL, mtry = NULL, trees = NULL, min_n = NULL,
                        sample_size = NULL, loss_reduction = NULL) {
    
    args <- list(
        
        # ARIMA
        seasonal_period           = rlang::enquo(seasonal_period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma),
        
        # Catboost/LightGBM
        tree_depth                = rlang::enquo(tree_depth),
        learn_rate                = rlang::enquo(learn_rate),
        mtry                      = rlang::enquo(mtry),
        trees                     = rlang::enquo(trees),
        min_n                     = rlang::enquo(min_n),
        sample_size               = rlang::enquo(sample_size),
        loss_reduction            = rlang::enquo(loss_reduction)
    )
    
    parsnip::new_model_spec(
        "boost_arima",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.boost_arima <- function(x, ...) {
    cat("Sarima Model w/ Catboost Error Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.boost_arima <- function(object,
                               parameters = NULL, seasonal_period = NULL,
                               non_seasonal_ar = NULL, non_seasonal_differences = NULL, non_seasonal_ma = NULL,
                               seasonal_ar = NULL, seasonal_differences = NULL, seasonal_ma = NULL,
                               tree_depth = NULL, learn_rate = NULL, mtry = NULL, trees = NULL, min_n = NULL,
                               sample_size = NULL, loss_reduction = NULL, fresh = FALSE, ...) {
    
    args <- list(
        
        # ARIMA
        seasonal_period           = rlang::enquo(seasonal_period),
        non_seasonal_ar           = rlang::enquo(non_seasonal_ar),
        non_seasonal_differences  = rlang::enquo(non_seasonal_differences),
        non_seasonal_ma           = rlang::enquo(non_seasonal_ma),
        seasonal_ar               = rlang::enquo(seasonal_ar),
        seasonal_differences      = rlang::enquo(seasonal_differences),
        seasonal_ma               = rlang::enquo(seasonal_ma),
        
        # Catboost/LightGBM
        tree_depth                = rlang::enquo(tree_depth),
        learn_rate                = rlang::enquo(learn_rate),
        mtry                      = rlang::enquo(mtry),
        trees                     = rlang::enquo(trees),
        min_n                     = rlang::enquo(min_n),
        sample_size               = rlang::enquo(sample_size),
        loss_reduction            = rlang::enquo(loss_reduction)
    )
    
    parsnip::update_spec(
        object = object,
        parameters = parameters,
        args_enquo_list = args,
        fresh = fresh,
        cls = "boost_arima",
        ...
    )
    

}


#' @export
#' @importFrom parsnip translate
translate.boost_arima <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'auto_arima_catboost'` for translation.")
        engine <- "auto_arima_catboost"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}



# FIT BRIDGE - AUTO ARIMA ----

#' Bridge ARIMA-Catboost Modeling function
#'
#' @inheritParams forecast::auto.arima
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param max.p The maximum order of the non-seasonal auto-regressive (AR) terms.
#' @param max.d The maximum order of integration for non-seasonal differencing.
#' @param max.q The maximum order of the non-seasonal moving average (MA) terms.
#' @param max.P The maximum order of the seasonal auto-regressive (SAR) terms.
#' @param max.D The maximum order of integration for seasonal differencing.
#' @param max.Q The maximum order of the seasonal moving average (SMA) terms.
#' @param depth The maximum depth of the tree (i.e. number of splits).
#' @param eta The rate at which the boosting algorithm adapts from iteration-to-iteration.
#' @param iterations The number of trees contained in the ensemble.
#' @param min_data_in_leaf The minimum number of data points in a node that is required for the node to be split further.
#' @param subsample The amount of data exposed to the fitting routine.
#' @param rsm The number of predictors that will be randomly sampled at each split when creating the tree models.
#' @param ... Additional arguments passed to `catboost::catboost.train`
#'
#'
#' @export
#' @importFrom stats frequency
auto_sarima_catboost_fit_impl <- function(x, y, period = "auto",
                                        max.p = 5, max.d = 2, max.q = 5,
                                        max.P = 2, max.D = 1, max.Q = 2,
                                        
                                        max.order = 5, d = NA, D = NA,
                                        start.p = 2,
                                        start.q = 2,
                                        start.P = 1,
                                        start.Q = 1,
                                        stationary = FALSE,
                                        seasonal = TRUE,
                                        ic = c("aicc", "aic", "bic"),
                                        stepwise = TRUE,
                                        nmodels = 94,
                                        trace = FALSE,
                                        approximation = (length(x) > 150 | frequency(x) > 12),
                                        method = NULL,
                                        truncate = NULL,
                                        test = c("kpss", "adf", "pp"),
                                        test.args = list(),
                                        seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                                        seasonal.test.args = list(),
                                        allowdrift = TRUE,
                                        allowmean = TRUE,
                                        lambda = NULL,
                                        biasadj = FALSE,
                                        # stats::arima
                                        # SSinit = c("Gardner1980", "Rossignol2011"),
                                        # optim.method = "BFGS",
                                        # optim.control = list(), kappa = 1e6,
                                        
                                        # catboost params
                                        depth = 6, eta  = 0.3, rsm = 1, iterations = 1000, min_data_in_leaf = 1, subsample = 1,
                                        ...) {
    
    args <- list(...)
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome <- y
    predictors <- x
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    # auto.arima
    fit_arima   <- forecast::auto.arima(outcome,
                                        max.p = max.p, max.d = max.d, max.q = max.q,
                                        max.P = max.P, max.D = max.D, max.Q = max.Q,
                                        max.order = max.order, d = d, D = D,
                                        start.p = start.p, start.q = start.q,
                                        start.P = start.P, start.Q = start.Q,
                                        stationary = stationary, seasonal = seasonal,
                                        ic = ic, stepwise = stepwise,
                                        nmodels = nmodels, trace = trace,
                                        approximation = approximation,
                                        method = method, truncate = truncate,
                                        test = test, test.args = test.args,
                                        seasonal.test = seasonal.test, seasonal.test.args = seasonal.test.args,
                                        allowdrift = allowdrift, allowmean = allowmean,
                                        lambda = lambda, biasadj = biasadj
    )
    
    arima_residuals <- as.numeric(fit_arima$residuals)
    arima_fitted    <- as.numeric(fit_arima$fitted)
    
    #Catboost
    
    prepare_df_catboost <- function(x, y = NULL, categorical_cols= NULL) {
        if(is.null(categorical_cols)){
            # auto detect the categorical columns from data.frame
            # Not strictly necessary but good form.
            categorical_cols <- categorical_columns(x)
        }
        
        # catboost uses 0-indexed feature cols
        if(!is.null(categorical_cols)){categorical_cols <- categorical_cols-1}
        
        if (is.null(y))
            return(x)
        
        catboost::catboost.load_pool(
            data = x,
            label = y,
            cat_features = categorical_cols
        )
    }
    
    predictors <- predictors %>% dplyr::select(-dplyr::all_of(idx_col))
    
    # Catboost
    if (!is.null(predictors)) {
        
        d <- prepare_df_catboost(predictors, y = arima_residuals, categorical_cols = NULL)
        
        args[["iterations"]] <- iterations
        args[["min_data_in_leaf"]] <- min_data_in_leaf
        args[["subsample"]] <- if (subsample > 1) 1 else subsample
        args[["learning_rate"]] <- eta
        args[["depth"]] <- depth
        
        if (!any(names(args) %in% "task_type")){
            args[["rsm"]] <- if (rsm > 1) 1 else rsm/ncol(x)
        }
        
        train_args <- list()
        
        train_args[["params"]] <- args
        train_args[["learn_pool"]] <- d
        
        call <- parsnip::make_call(fun = "catboost.train", ns = "catboost", train_args)
        
        fit_catboost <- rlang::eval_tidy(call, env = rlang::current_env())
        catboost_fitted <- catboost::catboost.predict(fit_catboost, d)
        
    } else {
        fit_catboost       <- NULL
        catboost_fitted    <- rep(0, length(arima_residuals))
    }
    
    # RETURN A NEW MODELTIME BRIDGE
    
    # Class - Add a class for the model
    class <- "auto_sarima_catboost_fit_impl"
    
    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_arima,
        model_2 = fit_catboost
    )
    
    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  arima_fitted + catboost_fitted,
            .residuals = .actual - .fitted
        )
    
    # Extras - Pass on transformation recipe
    extras <- list(
        date_col   = idx_col,
        predictors = predictors
    )
    
    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0(modeltime::get_arima_description(fit_arima),
                   ifelse(is.null(fit_catboost), "", " w/ Catboost Errors"))
    
    # Create new model
    modeltime::new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )
}

#' @export
print.auto_sarima_catboost_fit_impl <- function(x, ...) {
    
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: Auto ARIMA\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: Catboost Errors\n\n")
    print(x$models$model_2)
    invisible(x)
}


# FIT BRIDGE - STANDARD ARIMA ----

#' Bridge ARIMA-Catboost Modeling function
#'
#' @inheritParams forecast::Arima
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param p The order of the non-seasonal auto-regressive (AR) terms.
#' @param d The order of integration for non-seasonal differencing.
#' @param q The order of the non-seasonal moving average (MA) terms.
#' @param P The order of the seasonal auto-regressive (SAR) terms.
#' @param D The order of integration for seasonal differencing.
#' @param Q The order of the seasonal moving average (SMA) terms.
#' @param depth The maximum depth of the tree (i.e. number of splits).
#' @param eta The rate at which the boosting algorithm adapts from iteration-to-iteration.
#' @param iterations The number of trees contained in the ensemble.
#' @param min_data_in_leaf The minimum number of data points in a node that is required for the node to be split further.
#' @param subsample The amount of data exposed to the fitting routine.
#' @param rsm The number of predictors that will be randomly sampled at each split when creating the tree models.
#' @param ... Additional arguments passed to `catboost::catboost.train`
#' 
#' @export
#' @importFrom stats frequency
sarima_catboost_fit_impl <- function(x, y, period = "auto",
                                   p = 0, d = 0, q = 0,
                                   P = 0, D = 0, Q = 0,
                                   include.mean = TRUE,
                                   include.drift = FALSE,
                                   include.constant,
                                   lambda = model$lambda,
                                   biasadj = FALSE,
                                   method = c("CSS-ML", "ML", "CSS"),
                                   model = NULL,
                                   
                                   # catboost params
                                   depth = 6, eta  = 0.3, rsm = 1, iterations = 1000, min_data_in_leaf = 1, subsample = 1,
                                   ...) {
    
    args <- list(...)
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    
    outcome <- y
    predictors <- x
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    # auto.arima
    fit_arima   <- forecast::Arima(outcome,
                                   order = c(p, d, q),
                                   seasonal = c(P, D, Q),
                                   include.mean = include.mean,
                                   include.drift = include.drift,
                                   include.constant = include.constant,
                                   lambda = model$lambda,
                                   biasadj = biasadj,
                                   method = method,
                                   model = model
    )
    
    arima_residuals <- as.numeric(fit_arima$residuals)
    arima_fitted    <- as.numeric(fit_arima$fitted)
    
    prepare_df_catboost <- function(x, y = NULL, categorical_cols= NULL) {
        if(is.null(categorical_cols)){
            # auto detect the categorical columns from data.frame
            # Not strictly necessary but good form.
            categorical_cols <- categorical_columns(x)
        }
        
        # catboost uses 0-indexed feature cols
        if(!is.null(categorical_cols)){categorical_cols <- categorical_cols-1}
        
        if (is.null(y))
            return(x)
        
        catboost::catboost.load_pool(
            data = x,
            label = y,
            cat_features = categorical_cols
        )
    }
    
    predictors <- predictors %>% dplyr::select(-dplyr::all_of(idx_col))
    
    # catboost
    if (!is.null(predictors)) {
        
        d <- prepare_df_catboost(predictors, y = arima_residuals, categorical_cols = NULL)
        
        args[["iterations"]] <- iterations
        args[["min_data_in_leaf"]] <- min_data_in_leaf
        args[["subsample"]] <- if (subsample > 1) 1 else subsample
        args[["learning_rate"]] <- eta
        args[["depth"]] <- depth
        
        if (!any(names(args) %in% "task_type")){
            args[["rsm"]] <- if (rsm > 1) 1 else rsm/ncol(x)
        }
        
        train_args <- list()
        
        train_args[["params"]] <- args
        train_args[["learn_pool"]] <- d
        
        call <- parsnip::make_call(fun = "catboost.train", ns = "catboost", train_args)
        
        fit_catboost <- rlang::eval_tidy(call, env = rlang::current_env())
        catboost_fitted <- catboost::catboost.predict(fit_catboost, d)
        
    } else {
        fit_catboost       <- NULL
        catboost_fitted    <- rep(0, length(arima_residuals))
    }
    
    # RETURN A NEW MODELTIME BRIDGE
    
    # Class - Add a class for the model
    class <- "sarima_catboost_fit_impl"
    
    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_arima,
        model_2 = fit_catboost
    )
    
    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  outcome,
            .fitted    =  arima_fitted + catboost_fitted,
            .residuals = .actual - .fitted
        )
    
    # Extras - Pass on transformation recipe
    extras <- list(
        date_col   = idx_col,
        predictors = predictors
    )
    
    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0(modeltime::get_arima_description(fit_arima),
                   ifelse(is.null(fit_catboost), "", " w/ Catboost Errors"))
    
    # Create new model
    modeltime::new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )
    
}

#' @export
print.sarima_catboost_fit_impl <- function(x, ...) {
    
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: Standard ARIMA\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: Catboost Errors\n\n")
    print(x$models$model_2)
    invisible(x)
}

# FIT BRIDGE - AUTO ARIMA Lightgbm ----

#' Bridge ARIMA-Lightgbm Modeling function
#'
#' @inheritParams forecast::auto.arima
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param max.p The maximum order of the non-seasonal auto-regressive (AR) terms.
#' @param max.d The maximum order of integration for non-seasonal differencing.
#' @param max.q The maximum order of the non-seasonal moving average (MA) terms.
#' @param max.P The maximum order of the seasonal auto-regressive (SAR) terms.
#' @param max.D The maximum order of integration for seasonal differencing.
#' @param max.Q The maximum order of the seasonal moving average (SMA) terms.
#' @param max_depth The maximum depth of the tree (i.e. number of splits).
#' @param learning_rate The rate at which the boosting algorithm adapts from iteration-to-iteration.
#' @param num_iterations The number of trees contained in the ensemble.
#' @param min_data_in_leaf The minimum number of data points in a node that is required for the node to be split further.
#' @param bagging_fraction The amount of data exposed to the fitting routine.
#' @param feature_fraction The number of predictors that will be randomly sampled at each split when creating the tree models.
#' @param min_gain_to_split The reduction in the loss function required to split further.
#' @param ... Additional arguments passed to `lightgbm::lgb.train`
#'
#'
#' @export
#' @importFrom stats frequency
auto_sarima_lightgbm_fit_impl <- function(x, y, period = "auto",
                                          max.p = 5, max.d = 2, max.q = 5,
                                          max.P = 2, max.D = 1, max.Q = 2,
                                          
                                          max.order = 5, d = NA, D = NA,
                                          start.p = 2,
                                          start.q = 2,
                                          start.P = 1,
                                          start.Q = 1,
                                          stationary = FALSE,
                                          seasonal = TRUE,
                                          ic = c("aicc", "aic", "bic"),
                                          stepwise = TRUE,
                                          nmodels = 94,
                                          trace = FALSE,
                                          approximation = (length(x) > 150 | frequency(x) > 12),
                                          method = NULL,
                                          truncate = NULL,
                                          test = c("kpss", "adf", "pp"),
                                          test.args = list(),
                                          seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                                          seasonal.test.args = list(),
                                          allowdrift = TRUE,
                                          allowmean = TRUE,
                                          lambda = NULL,
                                          biasadj = FALSE,
                                          # stats::arima
                                          # SSinit = c("Gardner1980", "Rossignol2011"),
                                          # optim.method = "BFGS",
                                          # optim.control = list(), kappa = 1e6,
                                          
                                          # lightgbm params
                                          max_depth = 17, learning_rate  = 0.1, num_iterations = 10, min_data_in_leaf = 20, 
                                          min_gain_to_split = 0, bagging_fraction = 1, feature_fraction = 1, ...) {
    
    others <- list(...)
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome <- y
    predictors <- x
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    # auto.arima
    fit_arima   <- forecast::auto.arima(outcome,
                                        max.p = max.p, max.d = max.d, max.q = max.q,
                                        max.P = max.P, max.D = max.D, max.Q = max.Q,
                                        max.order = max.order, d = d, D = D,
                                        start.p = start.p, start.q = start.q,
                                        start.P = start.P, start.Q = start.Q,
                                        stationary = stationary, seasonal = seasonal,
                                        ic = ic, stepwise = stepwise,
                                        nmodels = nmodels, trace = trace,
                                        approximation = approximation,
                                        method = method, truncate = truncate,
                                        test = test, test.args = test.args,
                                        seasonal.test = seasonal.test, seasonal.test.args = seasonal.test.args,
                                        allowdrift = allowdrift, allowmean = allowmean,
                                        lambda = lambda, biasadj = biasadj
    )
    
    arima_residuals <- as.numeric(fit_arima$residuals)
    arima_fitted    <- as.numeric(fit_arima$fitted)
    
    #LightGBM
    
    categorical_features_to_int <- function(x, cat_indices){
        for (i in cat_indices){
            x[[i]] <- as.integer(x[[i]]) -1
        }
        x
    }
    
    predictors <- predictors %>% dplyr::select(-dplyr::all_of(idx_col))
    
    if (!is.null(predictors)) {
        
        prepare_df_lgbm <- function(x, y = NULL) {
            categorical_cols <- categorical_columns(x)
            x <- categorical_features_to_int(x, categorical_cols)
            x <- as.matrix(x)
            return(x)
        }
        
        if(!is.null(feature_fraction)) {
            feature_fraction <- feature_fraction/ncol(x)
        }
        if(feature_fraction > 1) {
            feature_fraction <- 1
        }
        
        # subsample -----------------------
        if (bagging_fraction > 1) {
            bagging_fraction <- 1
        }
        
        # loss and num_class -------------------------
        if (!any(names(others) %in% c("objective"))) {
            if (is.numeric(y)) {
                others$num_class <- 1
                others$objective <- "regression"
            } else {
                lvl <- levels(y)
                lvls <- length(lvl)
                y <- as.numeric(y) - 1
                if (lvls == 2) {
                    others$num_class <- 1
                    others$objective <- "binary"
                } else {
                    others$num_class <- lvls
                    others$objective <- "multiclass"
                }
            }
        }
        
        arg_list <- list(
            num_iterations = num_iterations,
            learning_rate = learning_rate,
            max_depth = max_depth,
            feature_fraction = feature_fraction,
            min_data_in_leaf = min_data_in_leaf,
            min_gain_to_split = min_gain_to_split,
            bagging_fraction = bagging_fraction
        )
        
        # override or add some other args
        others <- others[!(names(others) %in% c("data", names(arg_list)))]
        
        # parallelism should be explicitly specified by the user
        if(all(sapply(others[c("num_threads", "num_thread", "nthread", "nthreads", "n_jobs")], is.null))) others$num_threads <- 1L
        
        if(max_depth > 17) {
            warning("max_depth > 17, num_leaves truncated to 2^17 - 1")
            max_depth <- 17
        }
        
        if(is.null(others$num_leaves)) {
            others$num_leaves = max(2^max_depth - 1, 2)
        }
        
        arg_list <- purrr::compact(c(arg_list, others))
        
        
        # train ------------------------
        d <- lightgbm::lgb.Dataset(
            data = prepare_df_lgbm(predictors),
            label = arima_residuals,
            categorical_feature = categorical_columns(predictors),
            feature_pre_filter = FALSE
        )
        
        main_args <- list(
            data = quote(d),
            params = arg_list
        )
        
        call <- parsnip::make_call(fun = "lgb.train", ns = "lightgbm", main_args)
        fit_lightgbm <- rlang::eval_tidy(call, env = rlang::current_env())
        
        lightgbm_fitted <- stats::predict(fit_lightgbm, prepare_df_lgbm(predictors))
        
    } else {
        fit_lightgbm       <- NULL
        lightgbm_fitted    <- rep(0, length(prophet_residuals))
    }
    
    # RETURN A NEW MODELTIME BRIDGE
    
    # Class - Add a class for the model
    class <- "auto_sarima_lightgbm_fit_impl"
    
    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_arima,
        model_2 = fit_lightgbm
    )
    
    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  arima_fitted + lightgbm_fitted,
            .residuals = .actual - .fitted
        )
    
    # Extras - Pass on transformation recipe
    extras <- list(
        date_col   = idx_col,
        predictors = predictors
    )
    
    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0(modeltime::get_arima_description(fit_arima),
                   ifelse(is.null(fit_lightgbm), "", " w/ LightGBM Errors"))
    
    # Create new model
    modeltime::new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )
}

#' @export
print.auto_sarima_lightgbm_fit_impl <- function(x, ...) {
    
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: Auto ARIMA\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: LightGBM Errors\n\n")
    print(x$models$model_2)
    invisible(x)
}

# FIT BRIDGE - STANDARD ARIMA LightGBM ----

#' Bridge ARIMA-LightGBM Modeling function
#'
#' @inheritParams forecast::Arima
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param period A seasonal frequency. Uses "auto" by default. A character phrase
#'  of "auto" or time-based phrase of "2 weeks" can be used if a date or date-time variable is provided.
#' @param p The order of the non-seasonal auto-regressive (AR) terms.
#' @param d The order of integration for non-seasonal differencing.
#' @param q The order of the non-seasonal moving average (MA) terms.
#' @param P The order of the seasonal auto-regressive (SAR) terms.
#' @param D The order of integration for seasonal differencing.
#' @param Q The order of the seasonal moving average (SMA) terms.
#' @param max_depth The maximum depth of the tree (i.e. number of splits).
#' @param learning_rate The rate at which the boosting algorithm adapts from iteration-to-iteration.
#' @param num_iterations The number of trees contained in the ensemble.
#' @param min_data_in_leaf The minimum number of data points in a node that is required for the node to be split further.
#' @param bagging_fraction The amount of data exposed to the fitting routine.
#' @param feature_fraction The number of predictors that will be randomly sampled at each split when creating the tree models.
#' @param min_gain_to_split The reduction in the loss function required to split further.
#' @param ... Additional arguments passed to `lightgbm::lgb.train`
#' 
#' @export
#' @importFrom stats frequency
sarima_lightgbm_fit_impl <- function(x, y, period = "auto",
                                     p = 0, d = 0, q = 0,
                                     P = 0, D = 0, Q = 0,
                                     include.mean = TRUE,
                                     include.drift = FALSE,
                                     include.constant,
                                     lambda = model$lambda,
                                     biasadj = FALSE,
                                     method = c("CSS-ML", "ML", "CSS"),
                                     model = NULL,
                                     
                                     # lightgbm params
                                     max_depth = 17, learning_rate  = 0.1, num_iterations = 10, min_data_in_leaf = 20, 
                                     min_gain_to_split = 0, bagging_fraction = 1, feature_fraction = 1, ...) {
    
    others <- list(...)
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    
    outcome <- y
    predictors <- x
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # FIT
    outcome <- stats::ts(outcome, frequency = period)
    
    # auto.arima
    fit_arima   <- forecast::Arima(outcome,
                                   order = c(p, d, q),
                                   seasonal = c(P, D, Q),
                                   include.mean = include.mean,
                                   include.drift = include.drift,
                                   include.constant = include.constant,
                                   lambda = model$lambda,
                                   biasadj = biasadj,
                                   method = method,
                                   model = model
    )
    
    arima_residuals <- as.numeric(fit_arima$residuals)
    arima_fitted    <- as.numeric(fit_arima$fitted)
    
    predictors <- predictors %>% dplyr::select(-dplyr::all_of(idx_col))
    
    #LightGBM
    
    categorical_features_to_int <- function(x, cat_indices){
        for (i in cat_indices){
            x[[i]] <- as.integer(x[[i]]) -1
        }
        x
    }
    
    predictors <- predictors %>% dplyr::select(-dplyr::all_of(idx_col))
    
    if (!is.null(predictors)) {
        
        prepare_df_lgbm <- function(x, y = NULL) {
            categorical_cols <- categorical_columns(x)
            x <- categorical_features_to_int(x, categorical_cols)
            x <- as.matrix(x)
            return(x)
        }
        
        if(!is.null(feature_fraction)) {
            feature_fraction <- feature_fraction/ncol(x)
        }
        if(feature_fraction > 1) {
            feature_fraction <- 1
        }
        
        # subsample -----------------------
        if (bagging_fraction > 1) {
            bagging_fraction <- 1
        }
        
        # loss and num_class -------------------------
        if (!any(names(others) %in% c("objective"))) {
            if (is.numeric(y)) {
                others$num_class <- 1
                others$objective <- "regression"
            } else {
                lvl <- levels(y)
                lvls <- length(lvl)
                y <- as.numeric(y) - 1
                if (lvls == 2) {
                    others$num_class <- 1
                    others$objective <- "binary"
                } else {
                    others$num_class <- lvls
                    others$objective <- "multiclass"
                }
            }
        }
        
        arg_list <- list(
            num_iterations = num_iterations,
            learning_rate = learning_rate,
            max_depth = max_depth,
            feature_fraction = feature_fraction,
            min_data_in_leaf = min_data_in_leaf,
            min_gain_to_split = min_gain_to_split,
            bagging_fraction = bagging_fraction
        )
        
        # override or add some other args
        others <- others[!(names(others) %in% c("data", names(arg_list)))]
        
        # parallelism should be explicitly specified by the user
        if(all(sapply(others[c("num_threads", "num_thread", "nthread", "nthreads", "n_jobs")], is.null))) others$num_threads <- 1L
        
        if(max_depth > 17) {
            warning("max_depth > 17, num_leaves truncated to 2^17 - 1")
            max_depth <- 17
        }
        
        if(is.null(others$num_leaves)) {
            others$num_leaves = max(2^max_depth - 1, 2)
        }
        
        arg_list <- purrr::compact(c(arg_list, others))
        
        
        # train ------------------------
        d <- lightgbm::lgb.Dataset(
            data = prepare_df_lgbm(predictors),
            label = arima_residuals,
            categorical_feature = categorical_columns(predictors),
            feature_pre_filter = FALSE
        )
        
        main_args <- list(
            data = quote(d),
            params = arg_list
        )
        
        call <- parsnip::make_call(fun = "lgb.train", ns = "lightgbm", main_args)
        fit_lightgbm <- rlang::eval_tidy(call, env = rlang::current_env())
        
        lightgbm_fitted <- stats::predict(fit_lightgbm, prepare_df_lgbm(predictors))
        
    } else {
        fit_lightgbm       <- NULL
        lightgbm_fitted    <- rep(0, length(prophet_residuals))
    }
    
    # RETURN A NEW MODELTIME BRIDGE
    
    # Class - Add a class for the model
    class <- "sarima_lightgbm_fit_impl"
    
    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_arima,
        model_2 = fit_lightgbm
    )
    
    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  outcome,
            .fitted    =  arima_fitted + lightgbm_fitted,
            .residuals = .actual - .fitted
        )
    
    # Extras - Pass on transformation recipe
    extras <- list(
        date_col   = idx_col,
        predictors = predictors
    )
    
    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0(modeltime::get_arima_description(fit_arima),
                   ifelse(is.null(fit_lightgbm), "", " w/ Catboost Errors"))
    
    # Create new model
    modeltime::new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )
    
}

#' @export
print.sarima_lightgbm_fit_impl <- function(x, ...) {
    
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: Standard ARIMA\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: LightGBM Errors\n\n")
    print(x$models$model_2)
    invisible(x)
}

# PREDICT BRIDGE ----

#' @export
predict.auto_sarima_catboost_fit_impl <- function(object, new_data, categorical_cols = NULL, ...) {
    sarima_catboost_predict_impl(object, new_data, categorical_cols, ...)
}

#' @export
predict.sarima_catboost_fit_impl <- function(object, new_data, categorical_cols = NULL, ...) {
    sarima_catboost_predict_impl(object, new_data, categorical_cols, ...)
}

#' @export
predict.auto_sarima_lightgbm_fit_impl <- function(object, new_data, categorical_cols = NULL, ...) {
    sarima_lightgbm_predict_impl(object, new_data, categorical_cols, ...)
}

#' @export
predict.sarima_lightgbm_fit_impl <- function(object, new_data, categorical_cols = NULL, ...) {
    sarima_lightgbm_predict_impl(object, new_data, categorical_cols, ...)
}

#' Bridge prediction Function for ARIMA-Catboost Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param categorical_cols Categorical variables
#' @param ... Additional arguments passed to `catboost.predict()`
#'
#' @export
sarima_catboost_predict_impl <- function(object, new_data, categorical_cols, ...) {
    
    # PREPARE INPUTS
    arima_model    <- object$models$model_1
    catboost_model <- object$models$model_2
    date_col       <- object$extras$date_col
    predictors     <- object$extras$predictors
    h_horizon      <- nrow(new_data)
    
    # PREDICTIONS
    
    # arima
    preds_arima <- forecast::forecast(arima_model, h = h_horizon) %>%
        tibble::as_tibble() %>%
        purrr::pluck(1) %>%
        as.numeric()
    
    # catboost
    if (!is.null(predictors)) {
        
        new_data <- new_data %>% dplyr::select(-dplyr::all_of(date_col))
        preds_catboost <- stats::predict(catboost_model, new_data, categorical_cols = categorical_cols, ...)
        
    } else {
        preds_catboost <- rep(0, h_horizon)
    }
    
    # Return predictions as numeric vector
    preds <- preds_arima + preds_catboost
    
    return(preds)
    
}

#' Bridge prediction Function for ARIMA-Lightgbm Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param categorical_cols Categorical variables
#' @param ... Additional arguments passed to `catboost.predict()`
#'
#' @export
sarima_lightgbm_predict_impl <- function(object, new_data, categorical_cols, ...) {
    
    # PREPARE INPUTS
    arima_model    <- object$models$model_1
    catboost_model <- object$models$model_2
    date_col       <- object$extras$date_col
    predictors     <- object$extras$predictors
    h_horizon      <- nrow(new_data)
    
    # PREDICTIONS
    
    # arima
    preds_arima <- forecast::forecast(arima_model, h = h_horizon) %>%
        tibble::as_tibble() %>%
        purrr::pluck(1) %>%
        as.numeric()
    
    # LightGBM
    if (!is.null(predictors)) {
        
        new_data <- new_data %>% dplyr::select(-dplyr::all_of(date_col))
        
        predict_lightgbm_regression_numeric <- function(object, new_data, ...) {
            # train_colnames <- object$fit$.__enclos_env__$private$train_set$get_colnames()
            p <- stats::predict(object$fit, prepare_df_lgbm(new_data), reshape = TRUE, predict_disable_shape_check=TRUE, ...)
            p
        }
        
        prepare_df_lgbm <- function(x, y = NULL) {
            categorical_cols <- categorical_columns(x)
            x <- categorical_features_to_int(x, categorical_cols)
            x <- as.matrix(x)
            return(x)
        }
        
        categorical_features_to_int <- function(x, cat_indices){
            for (i in cat_indices){
                x[[i]] <- as.integer(x[[i]]) -1
            }
            x
        }
        
        preds_lightgbm <- predict_lightgbm_regression_numeric(lightgbm_model, new_data, ...)
        
        
        
    } else {
        preds_lightgbm <- rep(0, h_horizon)
    }
    
    # Return predictions as numeric vector
    preds <- preds_arima + preds_lightgbm
    
    return(preds)
    
}

#' Retrieve the indices of categorical (factor) columns
#'
#' Utility function to help identify factors in data.frame.
#' Does only identify the columns, nothing else.
#' @noRd
categorical_columns <- function(x){
    categorical_cols <- NULL
    for (i in seq_along(x)) {
        if (is.factor(x[[i]])) {
            categorical_cols <- c(categorical_cols, i)
        }
    }
    categorical_cols
}


#' @export
predict.catboost.Model <- function(object, new_data, type = "RawFormulaVal", categorical_cols = NULL, ...) {
    
    prepare_df_catboost <- function(x, y = NULL, categorical_cols= NULL) {
        if(is.null(categorical_cols)){
            # auto detect the categorical columns from data.frame
            # Not strictly necessary but good form.
            categorical_cols <- categorical_columns(x)
        }
        
        # catboost uses 0-indexed feature cols
        if(!is.null(categorical_cols)){categorical_cols <- categorical_cols-1}
        
        if (is.null(y))
            return(x)
        
        catboost::catboost.load_pool(
            data = x,
            label = y,
            cat_features = categorical_cols
        )
    }
    
    if (!inherits(new_data, "catboost.Pool")) {
        d <- prepare_df_catboost(new_data, categorical_cols = categorical_cols)
        new_data <- catboost::catboost.load_pool(d, cat_features = categorical_cols)
    }
    
    prediction_type <- switch (
        type,
        "raw" = "RawFormulaVal",
        "numeric" = "RawFormulaVal",
        "class" = "Class",
        "prob" = "Probability",
        type
    )
    
    catboost::catboost.predict(object, new_data, prediction_type = prediction_type, ...)
}