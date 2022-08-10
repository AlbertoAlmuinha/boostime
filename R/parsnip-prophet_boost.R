# PROPHET BOOST ----

#' General Interface for Boosted PROPHET Time Series Models
#'
#' `boost_prophet()` is a way to generate a _specification_ of a Boosted PROPHET model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `prophet`.
#'
#' @inheritParams boost_arima
#' @inheritParams modeltime::prophet_reg
#'
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `boost_prophet()`, the
#'  mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#'
#'  - "prophet_catboost" (default) - Connects to [prophet::prophet()] and [catboost::catboost.train()]
#'  - "prophet_lightgbm" - Connects to [prophet::prophet()] and [lightgbm::lgb.train()]
#'
#' __Main Arguments__
#'
#' The main arguments (tuning parameters) for the __PROPHET__ model are:
#'
#' - `growth`: String 'linear' or 'logistic' to specify a linear or logistic trend.
#' - `changepoint_num`: Number of potential changepoints to include for modeling trend.
#' - `changepoint_range`: Range changepoints that adjusts how close to the end
#'    the last changepoint can be located.
#' - `season`: 'additive' (default) or 'multiplicative'.
#' - `prior_scale_changepoints`: Parameter modulating the flexibility of the
#'   automatic changepoint selection. Large values will allow many changepoints,
#'   small values will allow few changepoints.
#' - `prior_scale_seasonality`: Parameter modulating the strength of the
#'  seasonality model. Larger values allow the model to fit larger seasonal
#'  fluctuations, smaller values dampen the seasonality.
#' - `prior_scale_holidays`: Parameter modulating the strength of the holiday components model,
#'  unless overridden in the holidays input.
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
#' The standardized parameter names in `boostime` can be mapped to their original
#' names in each engine:
#'
#' Model 1: PROPHET:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "boostime", ~ "prophet",
#'     "growth", "growth ('linear')",
#'     "changepoint_num", "n.changepoints (25)",
#'     "changepoint_range", "changepoints.range (0.8)",
#'     "seasonality_yearly", "yearly.seasonality ('auto')",
#'     "seasonality_weekly", "weekly.seasonality ('auto')",
#'     "seasonality_daily", "daily.seasonality ('auto')",
#'     "season", "seasonality.mode ('additive')",
#'     "prior_scale_changepoints", "changepoint.prior.scale (0.05)",
#'     "prior_scale_seasonality", "seasonality.prior.scale (10)",
#'     "prior_scale_holidays", "holidays.prior.scale (10)",
#'     "logistic_cap", "df$cap (NULL)",
#'     "logistic_floor", "df$floor (NULL)"
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
#'
#' Other options can be set using `set_engine()`.
#'
#'
#' __prophet_catboost__
#'
#'
#' Model 1: PROPHET (`prophet::prophet`):
#' ```{r echo = FALSE}
#' str(prophet::prophet)
#' ```
#'
#' Parameter Notes:
#' 
#' - `df`: This is supplied via the parsnip / boostime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'  
#' - `holidays`: A data.frame of holidays can be supplied via `set_engine()`
#' 
#' - `uncertainty.samples`: The default is set to 0 because the prophet
#'  uncertainty intervals are not used as part of the Modeltime Workflow.
#'  You can override this setting if you plan to use prophet's uncertainty tools.
#'
#' Logistic Growth and Saturation Levels:
#' 
#' - For `growth = "logistic"`, simply add numeric values for `logistic_cap` and / or
#'   `logistic_floor`. There is _no need_ to add additional columns
#'   for "cap" and "floor" to your data frame.
#'
#' Limitations:
#' 
#' - `prophet::add_seasonality()` is not currently implemented. It's used to
#'  specify non-standard seasonalities using fourier series. An alternative is to use
#'  `step_fourier()` and supply custom seasonalities as Extra Regressors.
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
#' __prophet_lightgbm__
#'
#'
#' Model 1: PROPHET (`prophet::prophet`):
#' ```{r echo = FALSE}
#' str(prophet::prophet)
#' ```
#'
#' Parameter Notes:
#' 
#' - `df`: This is supplied via the parsnip / boostime `fit()` interface
#'  (so don't provide this manually). See Fit Details (below).
#'  
#' - `holidays`: A data.frame of holidays can be supplied via `set_engine()`
#' 
#' - `uncertainty.samples`: The default is set to 0 because the prophet
#'  uncertainty intervals are not used as part of the Modeltime Workflow.
#'  You can override this setting if you plan to use prophet's uncertainty tools.
#'
#' Logistic Growth and Saturation Levels:
#' 
#' - For `growth = "logistic"`, simply add numeric values for `logistic_cap` and / or
#'   `logistic_floor`. There is _no need_ to add additional columns
#'   for "cap" and "floor" to your data frame.
#'
#' Limitations:
#' 
#' - `prophet::add_seasonality()` is not currently implemented. It's used to
#'  specify non-standard seasonalities using fourier series. An alternative is to use
#'  `step_fourier()` and supply custom seasonalities as Extra Regressors.
#'
#' Model 2: Lightgbm (`catboost::catboost.train`):
#' ```{r echo = FALSE}
#' str(lightgbm::lgb.train)
#' ```
#'
#' Parameter Notes:
#' - Lightgbm uses a `params = list()` to capture.
#'  Parsnip / Timeboost automatically sends any args provided as `...` inside of `set_engine()` to
#'  the `params = list(...)`.
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
#'
#' __Univariate (No Extra Regressors):__
#'
#' For univariate analysis, you must include a date or date-time feature. Simply use:
#'
#'  - Formula Interface (recommended): `fit(y ~ date)` will ignore xreg's.
#'
#' __Multivariate (Extra Regressors)__
#'
#'  Extra Regressors parameter is populated using the `fit()` or `fit_xy()` function:
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
#'  The `month.lbl` is an exogenous regressor that can be passed to the `arima_reg()` using
#'  `fit()`:
#'
#'  - `fit(y ~ date + month.lbl)` will pass `month.lbl` on as an exogenous regressor.
#'
#'  Note that date or date-time class values are excluded from `xreg`.
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#' library(parsnip)
#' library(rsample)
#' library(timetk)
#' library(boostime)
#'
#' # Data
#' m750 <- m4_monthly %>% filter(id == "M750")
#' m750
#'
#' # Split Data 80/20
#' splits <- initial_time_split(m750, prop = 0.8)
#'
#' # ---- PROPHET ----
#'
#' # Model Spec
#' model_spec <- boost_prophet(
#'     learn_rate = 0.1
#' ) %>%
#'     set_engine("prophet_catboost")
#'
#' # Fit Spec
#' model_fit <- model_spec %>%
#'     fit(log(value) ~ date + as.numeric(date) + month(date, label = TRUE),
#'         data = training(splits))
#' model_fit
#'
#' @export
boost_prophet <- function(mode = "regression", growth = NULL, changepoint_num = NULL, changepoint_range = NULL,
                          seasonality_yearly = NULL, seasonality_weekly = NULL, seasonality_daily = NULL,
                          season = NULL, prior_scale_changepoints = NULL, prior_scale_seasonality = NULL,
                          prior_scale_holidays = NULL, logistic_cap = NULL, logistic_floor = NULL,
                          tree_depth = NULL, learn_rate = NULL, mtry = NULL, trees = NULL, min_n = NULL,
                          sample_size = NULL, loss_reduction = NULL) {
    
    args <- list(
        
        # Prophet
        growth                    = rlang::enquo(growth),
        changepoint_num           = rlang::enquo(changepoint_num),
        changepoint_range         = rlang::enquo(changepoint_range),
        seasonality_yearly        = rlang::enquo(seasonality_yearly),
        seasonality_weekly        = rlang::enquo(seasonality_weekly),
        seasonality_daily         = rlang::enquo(seasonality_daily),
        season                    = rlang::enquo(season),
        prior_scale_changepoints  = rlang::enquo(prior_scale_changepoints),
        prior_scale_seasonality   = rlang::enquo(prior_scale_seasonality),
        prior_scale_holidays      = rlang::enquo(prior_scale_holidays),
        logistic_cap              = rlang::enquo(logistic_cap),
        logistic_floor            = rlang::enquo(logistic_floor),
        
        # Catboost/LightGBM
        tree_depth                = rlang::enquo(tree_depth),
        learn_rate                = rlang::enquo(learn_rate),
        mtry                      = rlang::enquo(mtry),
        trees                     = rlang::enquo(trees),
        min_n                     = rlang::enquo(min_n),
        loss_reduction            = rlang::enquo(loss_reduction),
        sample_size               = rlang::enquo(sample_size)
    )
    
    parsnip::new_model_spec(
        "boost_prophet",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.boost_prophet <- function(x, ...) {
    cat("Prophet Model w/ Error Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.boost_prophet <- function(object,
                               parameters = NULL, growth = NULL, changepoint_num = NULL, changepoint_range = NULL,
                               seasonality_yearly = NULL, seasonality_weekly = NULL, seasonality_daily = NULL,
                               season = NULL, prior_scale_changepoints = NULL, prior_scale_seasonality = NULL,
                               prior_scale_holidays = NULL, logistic_cap = NULL, logistic_floor = NULL,
                               tree_depth = NULL, learn_rate = NULL, mtry = NULL, trees = NULL, min_n = NULL,
                               sample_size = NULL, loss_reduction = NULL, fresh = FALSE, ...) {
    
    args <- list(
        
        # Prophet
        growth                    = rlang::enquo(growth),
        changepoint_num           = rlang::enquo(changepoint_num),
        changepoint_range         = rlang::enquo(changepoint_range),
        seasonality_yearly        = rlang::enquo(seasonality_yearly),
        seasonality_weekly        = rlang::enquo(seasonality_weekly),
        seasonality_daily         = rlang::enquo(seasonality_daily),
        season                    = rlang::enquo(season),
        prior_scale_changepoints  = rlang::enquo(prior_scale_changepoints),
        prior_scale_seasonality   = rlang::enquo(prior_scale_seasonality),
        prior_scale_holidays      = rlang::enquo(prior_scale_holidays),
        logistic_cap              = rlang::enquo(logistic_cap),
        logistic_floor            = rlang::enquo(logistic_floor),
        
        # Catboost/LightGBM
        tree_depth                = rlang::enquo(tree_depth),
        learn_rate                = rlang::enquo(learn_rate),
        mtry                      = rlang::enquo(mtry),
        trees                     = rlang::enquo(trees),
        min_n                     = rlang::enquo(min_n),
        loss_reduction            = rlang::enquo(loss_reduction),
        sample_size               = rlang::enquo(sample_size)
    )
    
    parsnip::update_spec(
        object = object,
        parameters = parameters,
        args_enquo_list = args,
        fresh = fresh,
        cls = "boost_prophet",
        ...
    )
    

}


#' @export
#' @importFrom parsnip translate
translate.boost_prophet <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'prophet_catboost'` for translation.")
        engine <- "prophet_catboost"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}



# FIT BRIDGE - PROPHET ----

#' Bridge Prophet-Catboost Modeling function
#'
#' @inheritParams prophet::prophet
#' @inheritParams boost_prophet
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
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
prophet_catboost_fit_impl <- function(x, y, 
                                          df = NULL,
                                          growth = "linear",
                                          changepoints = NULL,
                                          n.changepoints = 25,
                                          changepoint.range = 0.8,
                                          yearly.seasonality = "auto",
                                          weekly.seasonality = "auto",
                                          daily.seasonality = "auto",
                                          holidays = NULL,
                                          seasonality.mode = "additive",
                                          seasonality.prior.scale = 10,
                                          holidays.prior.scale = 10,
                                          changepoint.prior.scale = 0.05,
                                          logistic_cap = NULL,
                                          logistic_floor = NULL,
                                          mcmc.samples = 0,
                                          interval.width = 0.8,
                                          uncertainty.samples = 1000,
                                          fit = TRUE,
                                          
                                          # catboost params
                                          depth = 6, eta  = 0.3, rsm = 1, iterations = 1000, min_data_in_leaf = 1, subsample = 1,
                                          ...) {
    
    args <- list(...)
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictors  <- x
    
    growth <- tolower(growth)
    
    if (!growth[1] %in% c("linear", "logistic")) {
        message("growth must be 'linear' or 'logistic'. Defaulting to 'linear'.")
        growth <- 'linear'
    }
    
    if (!seasonality.mode[1] %in% c("additive", "multiplicative")) {
        message("seasonality.mode must be 'additive' or 'multiplicative'. Defaulting to 'additive'.")
        seasonality.mode <- 'additive'
    }
    
    if (growth == "logistic") {
        if (all(c(is.null(logistic_cap), is.null(logistic_floor)))) {
            glubort("Capacities must be supplied for `growth = 'logistic'`. Try specifying at least one of 'logistic_cap' or 'logistic_floor'")
        }
    }
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    # period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # FIT
    
    # Construct Data Frame
    df <- tibble::tibble(
        y  = outcome,
        ds = idx
    )
    
    # Add logistic cap / floor
    if (growth == "logistic") {
        df$cap   <- logistic_cap
        df$floor <- logistic_floor
    }
    
    # Construct model
    # Fit model
    fit_prophet <- prophet::prophet(
        df = df,
        growth = growth,
        changepoints = changepoints,
        n.changepoints = n.changepoints,
        changepoint.range = changepoint.range,
        yearly.seasonality = yearly.seasonality,
        weekly.seasonality = weekly.seasonality,
        daily.seasonality = daily.seasonality,
        holidays = holidays,
        seasonality.mode = seasonality.mode,
        seasonality.prior.scale = seasonality.prior.scale,
        holidays.prior.scale = holidays.prior.scale,
        changepoint.prior.scale = changepoint.prior.scale,
        mcmc.samples = mcmc.samples,
        interval.width = interval.width,
        uncertainty.samples = uncertainty.samples,
        fit = fit
    )
    
    
    # In-sample Predictions
    prophet_fitted    <- stats::predict(fit_prophet, df) %>% dplyr::pull(yhat)
    prophet_residuals <- outcome - prophet_fitted
    
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
        
        d <- prepare_df_catboost(predictors, y = prophet_residuals, categorical_cols = NULL)
        
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
    class <- "prophet_catboost_fit_impl"
    
    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_prophet,
        model_2 = fit_catboost
    )
    
    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  prophet_fitted + catboost_fitted,
            .residuals = .actual - .fitted
        )
    
    # Extras - Pass on transformation recipe
    extras <- list(
        date_col   = idx_col,
        predictors = predictors,
        logistic_params = list(
            growth         = growth,
            logistic_cap   = logistic_cap,
            logistic_floor = logistic_floor
        )
        
    )
    
    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0("Prophet Model w/ Catboost Error Specification")
    
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
print.prophet_catboost_fit_impl <- function(x, ...) {
    
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: PROPHET\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: Catboost Errors\n\n")
    print(x$models$model_2)
    invisible(x)
}

#' Bridge Prophet-Lightgbm Modeling function
#'
#' @inheritParams prophet::prophet
#' @inheritParams boost_prophet
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
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
prophet_lightgbm_fit_impl <- function(x, y, 
                                      df = NULL,
                                      growth = "linear",
                                      changepoints = NULL,
                                      n.changepoints = 25,
                                      changepoint.range = 0.8,
                                      yearly.seasonality = "auto",
                                      weekly.seasonality = "auto",
                                      daily.seasonality = "auto",
                                      holidays = NULL,
                                      seasonality.mode = "additive",
                                      seasonality.prior.scale = 10,
                                      holidays.prior.scale = 10,
                                      changepoint.prior.scale = 0.05,
                                      logistic_cap = NULL,
                                      logistic_floor = NULL,
                                      mcmc.samples = 0,
                                      interval.width = 0.8,
                                      uncertainty.samples = 1000,
                                      fit = TRUE,
                                      
                                      # lightgbm params
                                      max_depth = 17, learning_rate  = 0.1, num_iterations = 10, min_data_in_leaf = 20, 
                                      min_gain_to_split = 0, bagging_fraction = 1, feature_fraction = 1, ...) {
    
    others <- list(...)
    
    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictors  <- x
    
    growth <- tolower(growth)
    
    if (!growth[1] %in% c("linear", "logistic")) {
        message("growth must be 'linear' or 'logistic'. Defaulting to 'linear'.")
        growth <- 'linear'
    }
    
    if (!seasonality.mode[1] %in% c("additive", "multiplicative")) {
        message("seasonality.mode must be 'additive' or 'multiplicative'. Defaulting to 'additive'.")
        seasonality.mode <- 'additive'
    }
    
    if (growth == "logistic") {
        if (all(c(is.null(logistic_cap), is.null(logistic_floor)))) {
            glubort("Capacities must be supplied for `growth = 'logistic'`. Try specifying at least one of 'logistic_cap' or 'logistic_floor'")
        }
    }
    
    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictors)
    # period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)
    
    # FIT
    
    # Construct Data Frame
    df <- tibble::tibble(
        y  = outcome,
        ds = idx
    )
    
    # Add logistic cap / floor
    if (growth == "logistic") {
        df$cap   <- logistic_cap
        df$floor <- logistic_floor
    }
    
    # Construct model
    # Fit model
    fit_prophet <- prophet::prophet(
        df = df,
        growth = growth,
        changepoints = changepoints,
        n.changepoints = n.changepoints,
        changepoint.range = changepoint.range,
        yearly.seasonality = yearly.seasonality,
        weekly.seasonality = weekly.seasonality,
        daily.seasonality = daily.seasonality,
        holidays = holidays,
        seasonality.mode = seasonality.mode,
        seasonality.prior.scale = seasonality.prior.scale,
        holidays.prior.scale = holidays.prior.scale,
        changepoint.prior.scale = changepoint.prior.scale,
        mcmc.samples = mcmc.samples,
        interval.width = interval.width,
        uncertainty.samples = uncertainty.samples,
        fit = fit
    )
    
    
    # In-sample Predictions
    prophet_fitted    <- stats::predict(fit_prophet, df) %>% dplyr::pull(yhat)
    prophet_residuals <- outcome - prophet_fitted
    
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
            label = prophet_residuals,
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
    class <- "prophet_lightgbm_fit_impl"
    
    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = fit_prophet,
        model_2 = fit_lightgbm
    )
    
    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  prophet_fitted + lightgbm_fitted,
            .residuals = .actual - .fitted
        )
    
    # Extras - Pass on transformation recipe
    extras <- list(
        date_col   = idx_col,
        predictors = predictors,
        logistic_params = list(
            growth         = growth,
            logistic_cap   = logistic_cap,
            logistic_floor = logistic_floor
        )
        
    )
    
    # Model Description - Gets printed to describe the high-level model structure
    desc <- paste0("Prophet Model w/ Lightgbm Error Specification")
    
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
print.prophet_lightgbm_fit_impl <- function(x, ...) {
    
    if (!is.null(x$desc)) cat(paste0(x$desc,"\n"))
    cat("---\n")
    cat("Model 1: PROPHET\n")
    print(x$models$model_1)
    cat("\n---\n")
    cat("Model 2: Lightgbm Errors\n\n")
    print(x$models$model_2)
    invisible(x)
}

# PREDICT BRIDGE ----

#' @export
predict.prophet_catboost_fit_impl <- function(object, new_data, categorical_cols = NULL, ...) {
    prophet_catboost_predict_impl(object, new_data, categorical_cols, ...)
}

#' @export
predict.prophet_lightgbm_fit_impl <- function(object, new_data, categorical_cols = NULL, ...) {
    prophet_lightgbm_predict_impl(object, new_data, categorical_cols, ...)
}

#' Bridge prediction Function for PROPHET-Catboost Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param categorical_cols Categorical variables
#' @param ... Additional arguments passed to `catboost.predict()`
#'
#' @export
prophet_catboost_predict_impl <- function(object, new_data, categorical_cols, ...) {
    
    prophet_model   <- object$models$model_1
    catboost_model  <- object$models$model_2
    idx_future      <- new_data %>% timetk::tk_index()
    logistic_params <- object$extras$logistic_params
    date_col        <- object$extras$date_col
    predictors      <- object$extras$predictors
    
    # Construct Future Frame
    df <- tibble::tibble(
        ds = idx_future
    )
    
    # Logistic Growth
    if (logistic_params$growth == "logistic") {
        df$cap   <- logistic_params$logistic_cap
        df$floor <- logistic_params$logistic_floor
    }
    
    # PREDICTIONS
    preds_prophet_df <- stats::predict(prophet_model, df)
    
    # Return predictions as numeric vector
    preds_prophet <- preds_prophet_df %>% dplyr::pull(yhat)
    
    # catboost
    if (!is.null(predictors)) {
        
        new_data <- new_data %>% dplyr::select(-dplyr::all_of(date_col))
        preds_catboost <- stats::predict(catboost_model, new_data, categorical_cols = categorical_cols, ...)
        
    } else {
        preds_catboost <- rep(0, h_horizon)
    }
    
    # Return predictions as numeric vector
    preds <- preds_prophet + preds_catboost
    
    return(preds)
    
}

#' Bridge prediction Function for PROPHET-Catboost Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param categorical_cols Categorical variables
#' @param ... Additional arguments passed to `catboost.predict()`
#'
#' @export
prophet_lightgbm_predict_impl <- function(object, new_data, categorical_cols, ...) {
    
    prophet_model   <- object$models$model_1
    lightgbm_model  <- object$models$model_2
    idx_future      <- new_data %>% timetk::tk_index()
    logistic_params <- object$extras$logistic_params
    date_col        <- object$extras$date_col
    predictors      <- object$extras$predictors
    
    # Construct Future Frame
    df <- tibble::tibble(
        ds = idx_future
    )
    
    # Logistic Growth
    if (logistic_params$growth == "logistic") {
        df$cap   <- logistic_params$logistic_cap
        df$floor <- logistic_params$logistic_floor
    }
    
    # PREDICTIONS
    preds_prophet_df <- stats::predict(prophet_model, df)
    
    # Return predictions as numeric vector
    preds_prophet <- preds_prophet_df %>% dplyr::pull(yhat)
    
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
    preds <- preds_prophet + preds_lightgbm
    
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