#' Computes a wide variety of summary performance metrics from stock or portfolio returns
#'
#' Asset and portfolio performance analysis is a deep field with a wide range of theories and
#' methods for analyzing risk versus reward. The `PerformanceAnalytics` package
#' consolidates many of the most widely used performance metrics as functions that can
#' be applied to stock or portfolio returns. `tq_performance`
#' implements these performance analysis functions in a tidy way, enabling scaling
#' analysis using the split, apply, combine framework.
#'
#' @param data A `tibble` (tidy data frame) of returns in tidy format (i.e long format).
#' @param Ra The column of asset returns
#' @param Rb The column of baseline returns (for functions that require comparison to a baseline)
#' @param performance_fun The performance function from `PerformanceAnalytics`. See
#' `tq_performance_fun_options()` for a complete list of integrated functions.
#' @param ... Additional parameters passed to the `PerformanceAnalytics` function.
#'
#' @return Returns data in the form of a `tibble` object.
#'
#' @details
#'
#' \strong{Important concept}: Performance is based on the statistical properties of returns,
#' and as a result this function uses stock or portfolio returns as opposed
#' to stock prices.
#'
#' `tq_performance` is a wrapper for various `PerformanceAnalytics` functions
#' that return portfolio statistics.
#' The main advantage is the ability to scale with the `tidyverse`.
#'
#' `Ra` and `Rb` are the columns containing asset and baseline returns, respectively.
#' These columns are mapped to the `PerformanceAnalytics` functions. Note that `Rb`
#' is not always required, and in these instances the argument defaults to `Rb = NULL`.
#' The user can tell if `Rb` is required by researching the underlying performance function.
#'
#' `...` are additional arguments that are passed to the `PerformanceAnalytics`
#' function. Search the underlying function to see what arguments can be passed through.
#'
#' `tq_performance_fun_options` returns a list of compatible `PerformanceAnalytics` functions
#' that can be supplied to the `performance_fun` argument.
#'
#' @seealso
#' \itemize{
#'   \item [tq_transmute()] which can be used to calculate period returns from a
#'   set of stock prices. Use `mutate_fun = periodReturn` with the appropriate periodicity
#'   such as `period = "monthly"`.
#'   \item [tq_portfolio()] which can be used to aggregate period returns from
#'   multiple stocks to period returns for a portfolio.
#'   \item The `PerformanceAnalytics` package, which contains the underlying functions
#'   for the `performance_fun` argument. Additional parameters can be passed via `...`.
#' }
#'
#'
#' @name tq_performance
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#' library(dplyr)
#'
#' # Use FANG data set
#' data(FANG)
#'
#' # Get returns for individual stock components grouped by symbol
#' Ra <- FANG %>%
#'     group_by(symbol) %>%
#'     tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Ra")
#'
#' # Get returns for SP500 as baseline
#' Rb <- "^GSPC" %>%
#'     tq_get(get  = "stock.prices",
#'            from = "2010-01-01",
#'            to   = "2015-12-31") %>%
#'     tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")
#'
#' # Merge stock returns with baseline
#' RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
#'
#' ##### Performance Metrics #####
#'
#' # View options
#' tq_performance_fun_options()
#'
#' # Get performance metrics
#' RaRb %>%
#'     tq_performance(Ra = Ra, performance_fun = SharpeRatio, p = 0.95)
#'
#' RaRb %>%
#'     tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
#'

# tq_performance ------------------------------------------------------------------------------------------------

#' @rdname tq_performance
#' @export
tq_performance <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    tq_performance_(data            = data,
                    Ra              = lazyeval::expr_text(Ra),
                    Rb              = lazyeval::expr_text(Rb),
                    performance_fun = lazyeval::expr_text(performance_fun),
                    ...             = ...)
}

#' @rdname tq_performance
#' @export
tq_performance_ <- function(data, Ra, Rb = NULL, performance_fun, ...) {
    UseMethod("tq_performance_", data)
}

# tq_performance method dispatch --------------------------------------------------------------------------------

#' @export
tq_performance_.default <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_performance_.tbl_df <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    # Check mutate_fun in xts, quantmod or TTR
    check_performance_fun_options(performance_fun)

    # Check Ra and Rb
    check_x_y_valid(data, Ra, Rb)

    # Handle reserved names ("Ra" and "Rb")
    if (Ra == "Ra") {
        data <- data %>%
            dplyr::rename(.Ra = Ra)
        Ra <- ".Ra"
    }
    if (Rb == "Rb") {
        data <- data %>%
            dplyr::rename(.Rb = Rb)
        Rb <- ".Rb"
    }

    # Override weights TBD

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Drop any non-numeric columns except for date
    date_col <- dplyr::select(data, !!rlang::sym(date_col_name))
    Ra_col <- dplyr::select(data, !!rlang::sym(Ra))
    if (is.null(Rb) || Rb == "NULL")  {
        data <- dplyr::bind_cols(date_col, Ra_col)
    } else {
        Rb_col <- dplyr::select(data, !!rlang::sym(Rb))
        data <- dplyr::bind_cols(date_col, Ra_col, Rb_col)
    }

    # Convert inputs to functions
    fun_performance <- eval(parse(text = performance_fun))

    # Apply functions
    tryCatch({
        if (Rb == "NULL" || is.null(Rb)) {
            ret <- data %>%
                timetk::tk_xts(silent = TRUE) %$%
                fun_performance(eval(parse(text = Ra)), ...)
        } else {
            ret <- data %>%
                timetk::tk_xts(silent = TRUE) %$%
                fun_performance(eval(parse(text = Ra)),
                                eval(parse(text = Rb)),
                                ...)
        }

        ret <- as.matrix(ret)

        if (tibble::has_rownames(as.data.frame(ret)) == FALSE) {
            row_names <- paste0(performance_fun, ".", seq_along(nrow(ret)))
            rownames(ret) <- row_names
        }

        col_name <- "X1"
        colnames(ret)[[1]] <- col_name

        ret <- ret %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            dplyr::mutate(rowname = stringr::str_replace_all(rowname, pattern = " ", replacement = ""),
                          rowname = stringr::str_replace_all(rowname, pattern = ":", replacement = "")) %>%
            tidyr::spread(key = "rowname", value = "X1") %>%
            tibble::as_tibble()

        if (colnames(ret)[[1]] == Ra) colnames(ret)[[1]] <- performance_fun

        colnames(ret) <- colnames(ret) %>%
            stringr::str_replace_all(pattern = paste0("^", Ra), replacement = "") %>%
            stringr::str_replace_all(pattern = paste0("^", Rb), replacement = "")

    }, error = function(e) {

        warning(e)
        ret <- NA

    })

    ret
}

#' @export
tq_performance_.data.frame <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    # Convert data.frame to tibble
    data <- tibble::as_tibble(data)

    # tq_performance_ tbl_df version
    tq_performance_(data            = data,
                    Ra              = Ra,
                    Rb              = Rb,
                    performance_fun = performance_fun,
                    ...             = ...)
}

#' @export
tq_performance_.grouped_df <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    # Get groups
    group_names <- dplyr::group_vars(data)

    # Apply tq_performance_ to each group
    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x              = data,
            .f              = tq_performance_.tbl_df,
            Ra              = Ra,
            Rb              = Rb,
            performance_fun = performance_fun,
            ...)
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}

# Function options ---------------------------------------------------------------------------------------------

#' @rdname tq_performance
#' @export
tq_performance_fun_options <- function() {

    # Performance Analytics functions
    pkg_regex_table <- "^table"
    funs_table <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_table)
    funs_table <- ls("package:PerformanceAnalytics")[funs_table]
    funs_table <- funs_table[!stringr::str_detect(funs_table, "(Drawdowns$|CalendarReturns$|ProbOutPerformance$)")] # remove table.Drawdowns

    pkg_regex_capm <- "^CAPM"
    funs_capm <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_capm)
    funs_capm <- c(ls("package:PerformanceAnalytics")[funs_capm], "TimingRatio", "MarketTiming")

    pkg_regex_sfm <- "^SFM"
    funs_sfm <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_sfm)
    funs_sfm <- ls("package:PerformanceAnalytics")[funs_sfm]

    funs_VaR <- c("VaR", "ES", "ETL", "CDD", "CVaR")

    funs_descriptive <- c("mean", "sd", "min", "max", "cor", "mean.geometric", "mean.stderr", "mean.LCL", "mean.UCL")

    funs_annualized <- c("Return.annualized", "Return.annualized.excess", "sd.annualized", "SharpeRatio.annualized")

    funs_moments <- c("var", "cov", "skewness", "kurtosis", "CoVariance", "CoSkewness", "CoSkewnessMatrix",
                      "CoKurtosis", "CoKurtosisMatrix", "M3.MM", "M4.MM", "BetaCoVariance", "BetaCoSkewness", "BetaCoKurtosis")

    funs_drawdown <- c("AverageDrawdown", "AverageLength", "AverageRecovery", "DrawdownDeviation", "DrawdownPeak", "maxDrawdown")

    funs_risk <- c("MeanAbsoluteDeviation", "Frequency", "SharpeRatio", "MSquared", "MSquaredExcess", "HurstIndex")

    funs_regression <- c("CAPM.alpha", "CAPM.beta", "CAPM.epsilon", "CAPM.jensenAlpha", "SystematicRisk",
                         "SpecificRisk", "TotalRisk", "TreynorRatio", "AppraisalRatio", "FamaBeta",
                         "Selectivity", "NetSelectivity")

    funs_rel_risk <- c("ActivePremium", "ActiveReturn", "TrackingError", "InformationRatio")

    funs_drw_dn <- c("PainIndex", "PainRatio", "CalmarRatio", "SterlingRatio", "BurkeRatio", "MartinRatio", "UlcerIndex")

    funs_dside_risk <- c("DownsideDeviation", "DownsidePotential", "DownsideFrequency", "SemiDeviation", "SemiVariance",
                         "UpsideRisk", "UpsidePotentialRatio", "UpsideFrequency",
                         "BernardoLedoitRatio", "DRatio", "Omega", "OmegaSharpeRatio", "OmegaExcessReturn", "SortinoRatio", "M2Sortino", "Kappa",
                         "VolatilitySkewness", "AdjustedSharpeRatio", "SkewnessKurtosisRatio", "ProspectRatio")

    funs_misc <- c("KellyRatio", "Modigliani", "UpDownRatios")

    fun_options <- list(table.funs                     = funs_table,
                        CAPM.funs                      = funs_capm,
                        SFM.funs                       = funs_sfm,
                        descriptive.funs               = funs_descriptive,
                        annualized.funs                = funs_annualized,
                        VaR.funs                       = funs_VaR,
                        moment.funs                    = funs_moments,
                        drawdown.funs                  = funs_drawdown,
                        Bacon.risk.funs                = funs_risk,
                        Bacon.regression.funs          = funs_regression,
                        Bacon.relative.risk.funs       = funs_rel_risk,
                        Bacon.drawdown.funs            = funs_drw_dn,
                        Bacon.downside.risk.funs       = funs_dside_risk,
                        misc.funs                      = funs_misc)

    fun_options
}

# Utility ---------------------------------------------------------------------------------------------------

check_performance_fun_options <- function(fun) {
    fun_options <- tq_performance_fun_options() %>%
        unlist()
    if (!(fun %in% fun_options)) {
        stop(paste0("fun = ", fun, " not a valid option."))
    }
}
