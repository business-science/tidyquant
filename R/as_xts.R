#' Coerce objects to xts, designed to work with tibble and data.frame objects
#'
#' @param x A data.frame (with date column), matrix, xts, zoo, timeSeries, etc object.
#' @param date_col Required for objects that inherit the \code{data.frame} class.
#' Must specify a date column that is of the \code{date} class. Unused for
#' non-data.frame objects.
#' @param ... Additional parameters passed to \code{xts::as.xts}.
#'
#' @return Returns a \code{xts} object.
#'
#' @details \code{as_xts} is a wrapper for \code{xts::as.xts}
#' that includes a \code{date_col} argument. When \code{date_col} is specified,
#' the date column is used as row names during coercion to \code{xts} class. The
#' date column must be in a date format (i.e. \code{date} class).
#'
#' \code{as_xts_} evaluates the \code{date_col} using Non-Standard Evaluation
#' (NSE). See \code{vignette("nse")} for details.
#'
#' It is possible to coerce non-data.frame-like objects including
#' \code{zoo}, \code{timeSeries}, \code{ts}, and \code{irts} objects.
#' There is no need to specify the \code{date_col} argument.
#'
#' @seealso \code{\link{as_tibble}}
#'
#' @name as_xts
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyverse)
#' library(tidyquant)
#'
#' tq_get("AAPL", get = "stock.prices") %>%
#'     as_xts(date_col = date)
#'
#' # Non-Standard Evaluation (NSE)
#' x <- "date"
#' tq_get("AAPL", get = "stock.prices") %>%
#'     as_xts_(date_col = x)
#'

# PRIMARY FUNCTIONS ----

as_xts <- function(x, date_col = NULL, ...) {

    date_col <- deparse(substitute(date_col))

    as_xts_(x, date_col, ...)

}

#' @rdname as_xts
#' @export

as_xts_ <- function(x, date_col = NULL, ...) {

    if (inherits(x, "data.frame")) {

        # Check date_col provided
        if (is.null(date_col)) stop("`date_col` required for coercion from data.frame to xts")

        # Check date_col valid
        if (!(date_col %in% names(x))) {
            stop(paste0("date_col = ", date_col, " is invalid within names(x)"))
        }

        # Select columns and reorder
        date <- x %>% dplyr::select_(date_col)
        not_date_names <- names(x)[names(x) != date_col]
        not_date <- x %>% dplyr::select_(.dots = as.list(not_date_names))
        x <- dplyr::bind_cols(date, not_date)

        # Format order.by
        eval(parse(text = "date_col"))
        order.by <- eval(parse(text = stringr::str_c("x$", date_col)))

        # Convert tibble to xts
        ret <- xts::xts(x[,-1], order.by = order.by)

    } else {

        ret <- xts::as.xts(x)

    }

    ret

}


# UTILITY FUNCTIONS ----

# See utils.R

