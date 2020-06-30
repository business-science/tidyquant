#' Deprecated functions
#'
#' A record of functions that have been deprecated.
#'
#' @name deprecated
#'
#' @inheritParams tq_mutate
#'
#' @details
#'
#'   - `tq_transform()` - use [tq_transmute()]
#'   - `tq_transform_xy()` - use [tq_transmute_xy()]
#'   - `as_xts()` - use [timetk::tk_xts()]
#'   - `as_tibble()` - use [timetk::tk_tbl()]
#'   - `summarise_by_time()` - Moved to `timetk` package. Use [timetk::summarise_by_time()]
#'
#'
NULL

# tq_transform and tq_transform_xy - ----------------------------------------------------------------
# tq_transmute and tq_transmute_xy are now used.

#' @rdname deprecated
#' @export
tq_transform <- function(data, ohlc_fun = OHLCV, mutate_fun, col_rename = NULL, ...) {

    # Throw error
    .Defunct("tq_transmute",
             msg = "`tq_transform` is deprecated, please use `tq_transmute` instead.")
}

#' @rdname deprecated
#' @export
tq_transform_xy <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Throw error
    .Defunct("tq_transmute_xy",
             msg = "`tq_transform_xy` is deprecated, please use `tq_transmute_xy` instead.")
}
