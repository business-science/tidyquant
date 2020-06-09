#' DEPRECATED - Summarise each group by time
#'
#'
#' @description
#'
#' __Deprecation Instructions:__ Please use [timetk::summarise_by_time()] instead.
#'
#' `summarise_by_time()` Is a time-series variant of the popular `dplyr::summarise()` function.
#'
#' `summarise_by_time()` and `summarize_by_time()` are synonyms.
#'
#' @section Useful summary functions:
#'
#' * Sum: [SUM()]
#' * Center: [AVERAGE()], [MEDIAN()]
#' * Spread: [STDEV()], [VAR()]
#' * Range: [MIN()], [MAX()]
#' * Count: [COUNT()], [COUNT_UNIQUE()]
#' * Position: [FIRST()], [LAST()], [NTH()]
#' * Correlation: [COR()], [COV()]
#'
#'
#' @export
#' @param .data A `tbl` object or `data.frame`
#' @param .date_var A column of date or date-time (e.g. POSIXct) data class
#' @param ... Name-value pairs of summary functions.
#'   The name will be the name of the variable in the result.
#'
#'   The value can be:
#'
#'   * A vector of length 1, e.g. `min(x)`, `n()`, or `sum(is.na(y))`.
#'   * A vector of length `n`, e.g. `quantile()`.
#'   * A data frame, to add multiple columns from a single expression.
#' @param .by A time unit to summarise by.
#'   Time units are collapsed using `lubridate::floor_date()` or `lubridate::ceiling_date()`.
#'
#'   The value can be:
#'   - `second`
#'   - `minute`
#'   - `hour`
#'   - `day`
#'   - `week`
#'   - `month`
#'   - `bimonth`
#'   - `quarter`
#'   - `season`
#'   - `halfyear`
#'   - `year`
#'
#'   Arbitrary unique English abbreviations as in the `lubridate::period()` constructor are allowed.
#' @param .type One of "floor", "ceiling", or "round. Defaults to "floor". See `lubridate::round_date`.
#'
#' @family single table verbs
#'
#' @return
#' An object _usually_ of the same type as `.data`.
#'
#' * The rows come from the underlying `group_keys()`.
#' * The columns are a combination of the grouping keys and the summary
#'   expressions that you provide.
#' * If `x` is grouped by more than one variable, the output will be another
#'   [grouped_df] with the right-most group removed.
#' * If `x` is grouped by one variable, or is not grouped, the output will
#'   be a [tibble].
#' * Data frame attributes are **not** preserved, because `summarise()`
#'   fundamentally creates a new data frame.
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' @examples
#' # Libraries
#' library(tidyquant)
#' library(dplyr)
#'
#' # First adjusted price in each month
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by         = "month",
#'         adjusted   = FIRST(adjusted)
#'     )
#'
#' # Last adjused price in each month (day is last day of month with ceiling option)
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by        = "month",
#'         adjusted   = LAST(adjusted),
#'         .type      = "ceiling")
#'
#' # Total Volume each year (.by is set to "year" now)
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by        = "year",
#'         adjusted   = SUM(volume))
#'
#'
#' @export
summarise_by_time <- function(.data, .date_var = NULL, .by = "day", ...,
                              .type = c("floor", "ceiling", "round")) {

    # Deprecation message
    message("tidyquant::summarise_by_time() is deprecated. Don't fret! The timetk::summarise_by_time() replaces it.")

    UseMethod("summarise_by_time", .data)
}

#' @rdname summarise_by_time
#' @export
summarize_by_time <- summarise_by_time

#' @export
summarise_by_time.default <- function(.data, .date_var = NULL, .by = "day", ...,
                                      .type = c("floor", "ceiling", "round")) {

    stop("Object is not of class `data.frame`.", call. = FALSE)

}

#' @export
summarise_by_time.data.frame <- function(.data, .date_var = NULL, .by = "day", ...,
                                         .type = c("floor", "ceiling", "round")) {

    data_groups_expr   <- rlang::syms(dplyr::group_vars(.data))
    date_var_expr      <- rlang::enquo(.date_var)

    # Check date_var
    if (rlang::quo_is_null(date_var_expr)) {
        date_var_text <- timetk::tk_get_timeseries_variables(data)[1]
        message("Using date_var: ", date_var_text)
        date_var_expr <- rlang::sym(date_var_text)
    }

    # Choose lubridate function
    fun_type <- tolower(.type[[1]])
    if (fun_type == "floor") {
        .f <- lubridate::floor_date
    } else if (fun_type == "ceiling") {
        .f <- lubridate::ceiling_date
    } else {
        .f <- lubridate::round_date
    }

    # Time-based summarization logic
    ret_tbl <- .data %>%
        dplyr::mutate(!! date_var_expr := .f(!! date_var_expr, unit = .by)) %>%
        dplyr::group_by_at(.vars = dplyr::vars(!!! data_groups_expr, !! date_var_expr)) %>%
        dplyr::arrange(!! date_var_expr, .by_group = TRUE) %>%
        dplyr::summarize(...)

    return(ret_tbl)

}
