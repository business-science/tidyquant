#' DEPRECATED: Coerce to tibble. Enable preserving row names when coercing matrix
#' and time-series-like objects with row names.
#'
#' @description
#'
#' Coerce to tibble. Enable preserving row names when coercing matrix
#' and time-series-like objects with row names.
#'
#' DEPRECATED: Use [timekit::tk_tbl()] instead.
#'
#' @param x A list, matrix, xts, zoo, timeSeries, etc object.
#' @param preserve_row_names Used during coercion from matrix, xts, zoo,
#' timeSeries, etc objects that have row names. When `TRUE`, creates
#' a `row.names` column with names of rows as character class.
#' @param ... Additional parameters passed to the appropriate
#' [timekit::tk_tbl()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `as_tibble` is a wrapper for `tibble::as_tibble`
#' that includes a `preserve_row_names` argument. The function is designed
#' to coerce `xts`, `zoo`, `timeSeries`, `ts`, and `irts`
#' objects that are used frequently in quantitative financial analysis.
#' When `preserve_row_names = TRUE` is specified, a new column,
#' `row.names`, is created during object coercion as a character class.
#'
#' @seealso
#' * [tk_xts()] - Coercion to xts, replaces [tidyquant::as_xts()]
#' * [tk_tbl()] - Coercion to tbl, replaces [tidyquant::as_tibble()]
#' * [as_xts()] - Deprecated, use [tk_xts()] instead.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' # Matrix coercion to tibble
#' m <- matrix(rnorm(50), ncol = 5)
#' colnames(m) <- c("a", "b", "c", "d", "e")
#' rownames(m) <- letters[1:nrow(m)]
#' m_tbl <- as_tibble(m, preserve_row_names = TRUE)
#'
#' # xts coercion to tibble
#' quantmod::getSymbols("AAPL", auto.assign = FALSE) %>%
#'     as_tibble(preserve_row_names = TRUE)
#'
as_tibble <- function(x, preserve_row_names = FALSE, ...) {

    warning("The `tidyquant::as_tibble()` function is deprecated. Please use `timekit::tk_tbl()` instead.")

    timekit::tk_tbl(data = x, preserve_index = preserve_row_names, silent = TRUE, ...)

}




