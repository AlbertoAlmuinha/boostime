# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start


make_arima_boost_catboost <- function() {
    
    parsnip::set_new_model("boost_arima")
    parsnip::set_model_mode("boost_arima", "regression")
    
    # auto_arima_catboost ----
    
    # * Model ----
    parsnip::set_model_engine("boost_arima", mode = "regression", eng = "auto_arima_catboost")
    parsnip::set_dependency("boost_arima", "auto_arima_catboost", "forecast")
    parsnip::set_dependency("boost_arima", "auto_arima_catboost", "catboost")
    parsnip::set_dependency("boost_arima", "auto_arima_catboost", "boostime")
    
    # * Args - ARIMA ----
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "non_seasonal_ar",
        original     = "max.p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "non_seasonal_differences",
        original     = "max.d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "non_seasonal_ma",
        original     = "max.q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "seasonal_ar",
        original     = "max.P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "seasonal_differences",
        original     = "max.D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "seasonal_ma",
        original     = "max.Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )
    
    # * Args - Catboost ----
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "tree_depth",
        original     = "depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_catboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_catboost",
        parsnip = "mtry",
        original = "rsm",
        func = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_catboost",
        parsnip = "trees",
        original = "iterations",
        func = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_catboost",
        parsnip = "min_n",
        original = "min_data_in_leaf",
        func = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_catboost",
        parsnip = "sample_size",
        original = "subsample",
        func = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "boost_arima",
        eng     = "auto_arima_catboost",
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
        model         = "boost_arima",
        eng           = "auto_arima_catboost",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "auto_sarima_catboost_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "boost_arima",
        eng           = "auto_arima_catboost",
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
    
    
    # arima_catboost ----
    
    parsnip::set_model_engine("boost_arima", mode = "regression", eng = "arima_catboost")
    parsnip::set_dependency("boost_arima", "arima_catboost", "forecast")
    parsnip::set_dependency("boost_arima", "arima_catboost", "catboost")
    parsnip::set_dependency("boost_arima", "arima_catboost", "boostime")
    
    # * Args - ARIMA ----
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )
    
    # * Args - Catboost ----
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "tree_depth",
        original     = "depth",
        func         = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_catboost",
        parsnip      = "learn_rate",
        original     = "eta",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_catboost",
        parsnip = "mtry",
        original = "rsm",
        func = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_catboost",
        parsnip = "trees",
        original = "iterations",
        func = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_catboost",
        parsnip = "min_n",
        original = "min_data_in_leaf",
        func = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_catboost",
        parsnip = "sample_size",
        original = "subsample",
        func = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "boost_arima",
        eng     = "arima_catboost",
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
        model         = "boost_arima",
        eng           = "arima_catboost",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "sarima_catboost_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "boost_arima",
        eng           = "arima_catboost",
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
    
    # Arima LightGBM ----
    
    parsnip::set_model_engine("boost_arima", mode = "regression", eng = "arima_lightgbm")
    parsnip::set_dependency("boost_arima", "arima_lightgbm", "forecast")
    parsnip::set_dependency("boost_arima", "arima_lightgbm", "lightgbm")
    parsnip::set_dependency("boost_arima", "arima_lightgbm", "boostime")
    
    # * Args - ARIMA ----
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "non_seasonal_ar",
        original     = "p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "non_seasonal_differences",
        original     = "d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "non_seasonal_ma",
        original     = "q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "seasonal_ar",
        original     = "P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "seasonal_differences",
        original     = "D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "arima_lightgbm",
        parsnip      = "seasonal_ma",
        original     = "Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )
    
    # * Args - Lightgbm ----
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "tree_depth",
        original = "max_depth",
        func = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "learn_rate",
        original = "learning_rate",
        func = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "mtry",
        original = "feature_fraction",
        func = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "trees",
        original = "num_iterations",
        func = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "loss_reduction",
        original = "min_gain_to_split",
        func = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "min_n",
        original = "min_data_in_leaf",
        func = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "arima_lightgbm",
        parsnip = "sample_size",
        original = "bagging_fraction",
        func = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "boost_arima",
        eng     = "arima_lightgbm",
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
        model         = "boost_arima",
        eng           = "arima_lightgbm",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "sarima_lightgbm_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "boost_arima",
        eng           = "arima_lightgbm",
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
    
    # Auto Arima LightGBM ----
    
    parsnip::set_model_engine("boost_arima", mode = "regression", eng = "auto_arima_lightgbm")
    parsnip::set_dependency("boost_arima", "auto_arima_lightgbm", "forecast")
    parsnip::set_dependency("boost_arima", "auto_arima_lightgbm", "lightgbm")
    parsnip::set_dependency("boost_arima", "auto_arima_lightgbm", "boostime")
    
    # * Args - ARIMA ----
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "seasonal_period",
        original     = "period",
        func         = list(pkg = "modeltime", fun = "seasonal_period"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "non_seasonal_ar",
        original     = "max.p",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "non_seasonal_differences",
        original     = "max.d",
        func         = list(pkg = "modeltime", fun = "non_seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "non_seasonal_ma",
        original     = "max.q",
        func         = list(pkg = "modeltime", fun = "non_seasonal_ma"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "seasonal_ar",
        original     = "max.P",
        func         = list(pkg = "modeltime", fun = "seasonal_ar"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "seasonal_differences",
        original     = "max.D",
        func         = list(pkg = "modeltime", fun = "seasonal_differences"),
        has_submodel = FALSE
    )
    parsnip::set_model_arg(
        model        = "boost_arima",
        eng          = "auto_arima_lightgbm",
        parsnip      = "seasonal_ma",
        original     = "max.Q",
        func         = list(pkg = "modeltime", fun = "seasonal_ma"),
        has_submodel = FALSE
    )
    
    # * Args - Lightgbm ----
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "tree_depth",
        original = "max_depth",
        func = list(pkg = "dials", fun = "tree_depth"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "learn_rate",
        original = "learning_rate",
        func = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "mtry",
        original = "feature_fraction",
        func = list(pkg = "dials", fun = "mtry"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "trees",
        original = "num_iterations",
        func = list(pkg = "dials", fun = "trees"),
        has_submodel = TRUE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "loss_reduction",
        original = "min_gain_to_split",
        func = list(pkg = "dials", fun = "loss_reduction"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "min_n",
        original = "min_data_in_leaf",
        func = list(pkg = "dials", fun = "min_n"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model = "boost_arima",
        eng = "auto_arima_lightgbm",
        parsnip = "sample_size",
        original = "bagging_fraction",
        func = list(pkg = "dials", fun = "sample_size"),
        has_submodel = FALSE
    )
    
    # * Encoding ----
    parsnip::set_encoding(
        model   = "boost_arima",
        eng     = "auto_arima_lightgbm",
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
        model         = "boost_arima",
        eng           = "auto_arima_lightgbm",
        mode          = "regression",
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "auto_sarima_lightgbm_fit_impl"),
            defaults  = list()
        )
    )
    
    # * Predict ----
    parsnip::set_pred(
        model         = "boost_arima",
        eng           = "auto_arima_lightgbm",
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