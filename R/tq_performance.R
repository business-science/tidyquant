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
#'   \item The \code{PerformanceAnalytics} package, which contains the underlying functions
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

    # Patch for grouped data frames
    if (dplyr::is.grouped_df(data)) {

        tq_performance_grouped_df_(data = data, Ra = Ra, Rb = Rb,
                                   performance_fun = performance_fun, ...)

    } else {

        tq_performance_base_(data = data, Ra = Ra, Rb = Rb,
                             performance_fun = performance_fun, ...)

    }

}

#' @rdname tq_performance
#' @export
tq_performance_ <- function(data, Ra, Rb = NULL, performance_fun, ...) {

    # Patch for grouped data frames
    if (dplyr::is.grouped_df(data)) {

        tq_performance_grouped_df_(data = data, Ra = Ra, Rb = Rb,
                                   performance_fun = performance_fun, ...)

    } else {

        tq_performance_base_(data = data, Ra = Ra, Rb = Rb,
                             performance_fun = performance_fun, ...)

    }

}

tq_performance_grouped_df_ <- function(data, Ra, Rb, performance_fun, ...) {

    group_names <- dplyr::groups(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = data %>%
              purrr::map(~ tq_performance_base_(data = .x,
                                                Ra = Ra,
                                                Rb = Rb,
                                                performance_fun = performance_fun,
                                                ...))
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names)
}


tq_performance_base_ <- function(data, Ra, Rb, performance_fun, ...) {

    # Check transform_fun in xts, quantmod or TTR
    check_performance_fun_options(performance_fun)

    # Check data
    check_data_is_data_frame(data)

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

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Drop any non-numeric columns except for date
    date_col <- dplyr::select_(data, date_col_name)
    Ra_col <- dplyr::select_(data, Ra)
    if (is.null(Rb) || Rb == "NULL")  {
        data <- dplyr::bind_cols(date_col, Ra_col)
    } else {
        Rb_col <- dplyr::select_(data, Rb)
        data <- dplyr::bind_cols(date_col, Ra_col, Rb_col)
    }

    # Convert inputs to functions
    fun_performance <- eval(parse(text = performance_fun))

    # Apply functions
    tryCatch({
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
            stringr::str_replace_all(pattern = Ra, replacement = "") %>%
            stringr::str_replace_all(pattern = Rb, replacement = "")

    }, error = function(e) {

        warning(e)
        ret <- NA

    })

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
    funs_capm <- c(ls("package:PerformanceAnalytics")[funs_capm], "TimingRatio", "MarketTiming")

    pkg_regex_sfm <- "^SFM"
    funs_sfm <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_sfm)
    funs_sfm <- ls("package:PerformanceAnalytics")[funs_sfm]

    funs_VaR <- c("VaR", "ES", "ETL", "CDD", "CVaR")

    funs_descriptive <- c("mean", "sd", "min", "max", "cor", "mean.stderr", "mean.LCL", "mean.UCL")

    funs_annualized <- c("Return.annualized", "Return.annualized.excess", "sd.annualized", "SharpeRatio.annualized")

    funs_moments <- c("var", "cov", "skewness", "kurtosis", "CoVariance", "CoSkewness", "CoSkewnessMatrix",
                      "CoKurtosis", "CoKurtosisMatrix", "M3.MM", "M4.MM", "BetaCoVariance", "BetaCoSkewness", "BetaCoKurtosis")

    funs_drawdown <- c("AverageDrawdown", "AverageLength", "AverageRecovery", "DrawdownDeviation", "DrawdownPeak", "maxDrawdown")

    funs_risk <- c("MeanAbsoluteDeviation", "Frequency", "SharpeRatio", "MSquared", "MSquaredExcess", "HurstIndex", "UlcerIndex")

    funs_regression <- c("CAPM.alpha", "CAPM.beta", "CAPM.epsilon", "CAPM.jensenAlpha", "SystematicRisk",
                         "SpecificRisk", "TotalRisk", "TreynorRatio", "AppraisalRatio", "FamaBeta",
                         "Selectivity", "NetSelectivity")

    funs_rel_risk <- c("ActivePremium", "ActiveReturn", "TrackingError", "InformationRatio")

    funs_drw_dn <- c("PainIndex", "CalmarRatio", "SterlingRatio", "BurkeRatio", "MartinRatio", "PainRatio")

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


check_performance_fun_options <- function(fun) {
    fun_options <- tq_performance_fun_options() %>%
        unlist()
    if (!(fun %in% fun_options)) {
        stop(paste0("fun = ", fun, " not a valid option."))
    }
}
