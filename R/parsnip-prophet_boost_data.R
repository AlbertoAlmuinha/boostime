# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_prophet_boost_catboost <- function() {
    
    parsnip::set_new_model("boost_prophet")
    parsnip::set_model_mode("boost_prophet", "regression")
    
    # prophet_catboost ----
    
    # * Model ----
    parsnip::set_model_engine("boost_prophet", mode = "regression", eng = "prophet_catboost")
    parsnip::set_dependency("boost_prophet", "prophet_catboost", "prophet")
    parsnip::set_dependency("boost_prophet", "prophet_catboost", "catboost")
    parsnip::set_dependency("boost_prophet", "prophet_catboost", "boostime")
    
    # * Args - Prophet ----
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "growth",
        original     = "growth",
        func         = list(pkg = "modeltime", fun = "growth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "changepoint_num",
        original     = "n.changepoints",
        func         = list(pkg = "modeltime", fun = "changepoint_num"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "changepoint_range",
        original     = "changepoint.range",
        func         = list(pkg = "modeltime", fun = "changepoint_range"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "seasonality_yearly",
        original     = "yearly.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_yearly"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "seasonality_weekly",
        original     = "weekly.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_weekly"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "seasonality_daily",
        original     = "daily.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_daily"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "season",
        original     = "seasonality.mode",
        func         = list(pkg = "modeltime", fun = "season"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "prior_scale_changepoints",
        original     = "changepoint.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_changepoints"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "prior_scale_seasonality",
        original     = "seasonality.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_seasonality"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "prior_scale_holidays",
        original     = "holidays.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_holidays"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "logistic_cap",
        original     = "logistic_cap",
        func         = list(pkg = "boostime", fun = "logistic_cap"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "logistic_floor",
        original     = "logistic_floor",
        func         = list(pkg = "boostime", fun = "logistic_floor"),
        has_submodel = FALSE
    )
    
    # * Args - Catboost ----
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "tree_depth",
        original     = "depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_catboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_catboost",
        parsnip = "mtry",
        original = "rsm",
        func = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_catboost",
        parsnip = "trees",
        original = "iterations",
        func = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_catboost",
        parsnip = "min_n",
        original = "min_data_in_leaf",
        func = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_catboost",
        parsnip = "sample_size",
        original = "subsample",
        func = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "boost_prophet",
        eng     = "prophet_catboost",
        mode    = "regression",
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    # * Fit ----
    parsnip::set_fit(
        model         = "boost_prophet",
        eng           = "prophet_catboost",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "prophet_catboost_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "boost_prophet",
        eng           = "prophet_catboost",
        mode          = "regression",
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL,
            func      = c(fun = "predict"),
            args      =
                list(
                    object   = quote(object$fit),
                    new_data = quote(new_data)
                )
        )
    )
    
    # Prophet LightGBM
    
    parsnip::set_model_engine("boost_prophet", mode = "regression", eng = "prophet_lightgbm")
    parsnip::set_dependency("boost_prophet", "prophet_lightgbm", "prophet")
    parsnip::set_dependency("boost_prophet", "prophet_lightgbm", "lightgbm")
    parsnip::set_dependency("boost_prophet", "prophet_lightgbm", "boostime")
    
    # * Args - Prophet ----
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "growth",
        original     = "growth",
        func         = list(pkg = "modeltime", fun = "growth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "changepoint_num",
        original     = "n.changepoints",
        func         = list(pkg = "modeltime", fun = "changepoint_num"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "changepoint_range",
        original     = "changepoint.range",
        func         = list(pkg = "modeltime", fun = "changepoint_range"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "seasonality_yearly",
        original     = "yearly.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_yearly"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "seasonality_weekly",
        original     = "weekly.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_weekly"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "seasonality_daily",
        original     = "daily.seasonality",
        func         = list(pkg = "modeltime", fun = "seasonality_daily"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "season",
        original     = "seasonality.mode",
        func         = list(pkg = "modeltime", fun = "season"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "prior_scale_changepoints",
        original     = "changepoint.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_changepoints"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "prior_scale_seasonality",
        original     = "seasonality.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_seasonality"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "prior_scale_holidays",
        original     = "holidays.prior.scale",
        func         = list(pkg = "modeltime", fun = "prior_scale_holidays"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "logistic_cap",
        original     = "logistic_cap",
        func         = list(pkg = "boostime", fun = "logistic_cap"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_prophet",
        eng          = "prophet_lightgbm",
        parsnip      = "logistic_floor",
        original     = "logistic_floor",
        func         = list(pkg = "boostime", fun = "logistic_floor"),
        has_submodel = FALSE
    )
    
    # * Args - Lightgbm ----
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "tree_depth",
        original = "max_depth",
        func = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "learn_rate",
        original = "learning_rate",
        func = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "mtry",
        original = "feature_fraction",
        func = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "trees",
        original = "num_iterations",
        func = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "loss_reduction",
        original = "min_gain_to_split",
        func = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "min_n",
        original = "min_data_in_leaf",
        func = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_prophet",
        eng = "prophet_lightgbm",
        parsnip = "sample_size",
        original = "bagging_fraction",
        func = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "boost_prophet",
        eng     = "prophet_lightgbm",
        mode    = "regression",
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    # * Fit ----
    parsnip::set_fit(
        model         = "boost_prophet",
        eng           = "prophet_lightgbm",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "prophet_lightgbm_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "boost_prophet",
        eng           = "prophet_lightgbm",
        mode          = "regression",
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL,
            func      = c(fun = "predict"),
            args      =
                list(
                    object   = quote(object$fit),
                    new_data = quote(new_data)
                )
        )
    )
    
    
}

# nocov end