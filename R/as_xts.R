#' Coerce objects to xts, designed to work with tibble and data.frame objects
#'
#' @param x A data.frame (with date column), matrix, xts, zoo, timeSeries, etc object.
#' @param date_col For data.frame-like objects. Must specify a date column that
#' is in a standard unambigous date class.
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
#' \code{zoo}, \code{timeSeries}, \code{ts}, and \code{irts} objects. There is no
#' need to specify the \code{date_col} argument.
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
#'     as_xts(date_col = date)
#'
#' # Non-Standard Evaluation (NSE)
#' x <- "date"
#' tq_get("AAPL", get = "stock.prices") %>%
#'     as_xts_(date_col = x)
#'


as_xts_ <- function(x, date_col = NULL, ...) {

    if (inherits(x, "data.frame")) {

        ret <- tryCatch({

            # Select columns
            date <- x %>% dplyr::select_(date_col)
            not_date_names <- names(x)[names(x) != date_col]
            not_date <- x %>% dplyr::select_(.dots = as.list(not_date_names))
            x <- dplyr::bind_cols(date, not_date)

            # Format order.by
            eval(parse(text = "date_col"))
            order.by <- eval(parse(text = stringr::str_c("x$", date_col)))

            # Convert tibble to xts
            x_xts <- xts::xts(x[,-1], order.by = order.by)

        }, error = function(e) {

            suggestions <- find_date_cols(x)
            suggestions <- suggestions[suggestions == TRUE]
            suggestions <- stringr::str_c(names(suggestions), collapse = ", ")
            warn <- paste0("Must specify a `date_col` that is in a standard ",
                           "unambiguous date class. \nPossible date columns: ",
                           ifelse(length(suggestions) > 0, suggestions, "None found"))
            warning(warn)
            NA

        })

    } else {

        ret <- xts::as.xts(x)

    }

    ret

}


#' @rdname as_xts
#' @export
as_xts <- function(x, date_col = NULL, ...) {

    date_col <- col_name(substitute(date_col))

    as_xts_(x, date_col, ...)

}

# @rdname as_xts
# @export
# as.xts_ <- function(x, date_col = NULL, ...) {
#     as_xts_(x, date_col = NULL, ...)
# }
#
# @rdname as_xts
# @export
# as.xts <- function(x, date_col = NULL, ...) {
#     as_xts(x, date_col = NULL, ...)
# }

# UTILITY FUNCTIONS

find_date_cols <- function(x) {

    # Functions
    is_date_class <- function(x) inherits(x, 'Date')

    is_char_date <- function(col) {
        check_na <- col %>%
            as.character() %>%
            #as.Date(format = "%y/%m/%d") %>%
            lubridate::ymd() %>%
            is.na()
        !all(check_na) # check for any na's
    }

    # Check for date class
    ret <- sapply(x, function(x) is_date_class(x))

    # If none, check for character columns that can be converted
    if (sum(ret) == 0) {

        ret <- sapply(x, is_char_date)

    }

    ret

}

