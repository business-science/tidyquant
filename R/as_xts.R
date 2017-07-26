#' DEPRECATED: Coerce objects to xts, designed to work with tibble and data.frame objects
#'
#' @description
#' Coerce objects to xts, designed to work with tibble and data.frame objects
#'
#' DEPRECATED: Use [timetk::tk_xts()] instead.
#'
#' @param x A data.frame (with date column), matrix, xts, zoo, timeSeries, etc object.
#' @param date_col Required for objects that inherit the `data.frame` class.
#' Must specify a date column that is of the `date` class. Unused for
#' non-data.frame objects.
#' @param ... Additional parameters passed to [timetk::tk_xts()].
#'
#' @return Returns a `xts` object.
#'
#' @details
#'
#' `as_xts` is a wrapper for `xts::as.xts`
#' that includes a `date_col` argument. When `date_col` is specified,
#' the date column is used as row names during coercion to `xts` class. The
#' date column must be in a date format (i.e. `date` class).
#'
#' `as_xts_` evaluates the `date_col` using Non-Standard Evaluation
#' (NSE). See `vignette("nse")` for details.
#'
#' It is possible to coerce non-data.frame-like objects including
#' `zoo`, `timeSeries`, `ts`, and `irts` objects.
#' There is no need to specify the `date_col` argument.
#'
#' @seealso
#' * [tk_xts()] - Coercion to xts, replaces [tidyquant::as_xts()]
#' * [tk_tbl()] - Coercion to tbl, replaces [tidyquant::as_tibble()]
#' * [as_tibble()] - Deprecated, use [tk_tbl()] instead.
#'
#'
#' @name as_xts
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' tq_get("AAPL", get = "stock.prices") %>%
#'     as_xts(date_col = date) # Deprecated: Use tk_xts()
#'
#'

# PRIMARY FUNCTIONS ----

as_xts <- function(x, date_col = NULL, ...) {

    warning("The `as_xts()` function is deprecated. Please use `timetk::tk_xts()` instead.")

    timetk::tk_xts(data = x, date_var = date_col, silent = TRUE, ...)

}

#' @rdname as_xts
#' @export

as_xts_ <- function(x, date_col = NULL, ...) {

    warning("The `as_xts_()` function is deprecated. Please use `timetk::tk_xts()_` instead.")

    timetk::tk_xts_(data = x, date_var = date_col, silent = TRUE, ...)

}


# UTILITY FUNCTIONS ----

# See utils.R

