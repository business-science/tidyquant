#' Aggregates a group of returns by security into performance returns
#'
#' @param data A \code{tibble} (tidy data frame) of returns in tidy format (i.e long format).
#' @param Ra The column of asset returns
#' @param Rb The column of baseline returns (for functions that require comarison to a baseline)
#' @param performance_fun The performance function from \code{PerformanceAnalytics}. See
#' \code{tq_performance_fun_options()} for a complete list of integrated functions.

#' @param ... Additional parameters passed to the \code{PerformanceAnalytics} function.
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details \code{tq_performance} is a wrapper for various \code{PerformanceAnalytics} functions
#' that return portfolio statistics.
#' The main advantage is the ability to scale with the \code{tidyverse}.
#'
#' \code{Ra} and \code{Rb} are the columns containing asset and baseline returns, respectively.
#' These columns are mapped to the \code{PerformanceAnalytics} functions.
#'
#' \code{tq_performance_fun_options} returns a list of \code{PerformanceAnalytics} functions
#' that can be supplied to the \code{performance_fun} argument.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{tq_portfolio}} which can be used to aggregate period returns from
#'   multiple stocks to period returns for a portfolio.
#'   \item The \code{PerformanceAnalytics} package, which is contains the underlying functions
#'   for the \code{performance_fun} argument. Additional parameters can be passed via \code{...}.
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
#'
#' # TBD
#'




# PRIMARY FUNCTIONS ----

#' @rdname tq_performance
#' @export
tq_performance <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    # Convert to NSE
    Ra <- deparse(substitute(Ra))
    Rb <- deparse(substitute(Rb))
    performance_fun <- deparse(substitute(performance_fun))

    tq_performance_base_(data = data, Ra = Ra, Rb = Rb,
                         performance_fun = performance_fun, ...)

}

#' @rdname tq_performance
#' @export
tq_performance_ <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    tq_performance_base_(data = data, Ra = Ra, Rb = Rb,
                         performance_fun = performance_fun, ...)

}


tq_performance_base_ <- function(data, Ra, Rb, performance_fun, ...) {

    # Check transform_fun in xts, quantmod or TTR
    check_performance_fun_options(performance_fun)

    # Check data
    check_data_is_data_frame(data)

    # Check Ra and Rb
    check_x_y_valid(data, Ra, Rb)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Drop any non-numeric columns except for date
    date_col <- dplyr::select_(data, date_col_name)
    numeric_cols <- data %>%
        dplyr::select_if(is.numeric)
    data <- dplyr::bind_cols(date_col, numeric_cols)

    # Convert inputs to functions
    fun_performance <- eval(parse(text = performance_fun))

    # Apply functions
    if (Rb == "NULL" || is.null(Rb)) {
        ret <- data %>%
            as_xts_(date_col = date_col_name) %$%
            fun_performance(eval(parse(text = Ra)), ...)
    } else {
        ret <- data %>%
            as_xts_(date_col = date_col_name) %$%
            fun_performance(eval(parse(text = Ra)),
                            eval(parse(text = Rb)),
                            ...)
    }

    # Coerce to tibble and convert date / datetime
    # if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
    #                                               time_zone, col_rename)

    ret

}

#' @rdname tq_performance
#' @export
tq_performance_fun_options <- function() {

    # Performance Analytics functions
    pkg_regex_table <- "^table"
    funs_table <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_table)
    funs_table <- ls("package:PerformanceAnalytics")[funs_table]

    pkg_regex_capm <- "^CAPM"
    funs_capm <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_capm)
    funs_capm <- c(ls("package:PerformanceAnalytics")[funs_capm], "TimingRatio")

    funs_VaR <- c("VaR", "ES")

    funs_moments <- c("var", "cov", "skewness", "kurtosis", "CoVariance", "CoSkewness", "CoKurtosis",
                      "BetaCoVariance", "BetaCoSkewness", "BetaCoKurtosis")

    funs_drawdown <- c("AverageDrawdown", "DrawdownDeviation", "DrawdownPeak", "maxDrawdown")

    funs_risk <- c("MeanAbsoluteDeviation", "Frequency", "SharpeRatio", "MSquared", "MSquaredExcess")

    funs_regression <- c("CAPM.alpha", "CAPM.beta", "CAPM.epsilon", "CAPM.jensenAlpha", "SystematicRisk",
                         "SpecificRisk", "TotalRisk", "TreynorRatio", "AppraisalRatio", "FamaBeta",
                         "Selectivity", "NetSelectivity")

    funs_rel_risk <- c("ActivePremium", "TrackingError", "InformationRatio")

    funs_drw_dn <- c("PainIndex", "CalmarRatio", "SterlingRatio", "BurkeRatio", "MartinRatio", "PainRatio")

    funs_dside_risk <- c("DownsideDeviation", "DownsidePotential", "DownsideFrequency",
                         "UpsideRisk", "UpsidePotentialRatio", "UpsideFrequency",
                         "BernardoLedoitRatio", "DRatio", "OmegaSharpeRatio", "SortinoRatio", "Kappa",
                         "VolatilitySkewness", "AdjustedSharpeRatio", "SkewnessKurtosisRatio", "ProspectRatio")

    fun_options <- list(table.funs                     = funs_table,
                        CAPM.funs                      = funs_capm,
                        VaR.funs                       = funs_VaR,
                        moment.funs                    = funs_moments,
                        drawdown.funs                  = funs_drawdown,
                        Bacon.risk.funs                = funs_risk,
                        Bacon.regression.funs          = funs_regression,
                        Bacon.relative.risk.funs       = funs_rel_risk,
                        # Bacon.return.distribution.funs = funs_ret_dist,
                        Bacon.drawdown.funs            = funs_drw_dn,
                        Bacon.downside.risk.funs       = funs_dside_risk)

    fun_options

}


check_performance_fun_options <- function(fun) {
    fun_options <- tq_performance_fun_options() %>%
        unlist()
    if (!(fun %in% fun_options)) {
        stop(paste0("fun = ", fun, " not a valid option."))
    }
}
