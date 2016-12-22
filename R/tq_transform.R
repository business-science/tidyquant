#' Transforms quantitative data with results in \code{tibble} format
#'
#' @param x A \code{tibble} (tidy data frame) of data from \code{\link{tq_get}}.
#' @param col The column to use during transformation
#' @param transform A character string representing the type of transformation
#' to perform. Options include "lag", "next", "delt", "first"
#' "last", "to.period", "period.apply", and "period.return".
#' @param ... Additional parameters passed to the appropriate \code{quantmod}
#' function.
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details \code{tq_transform} is a wrapper for various \code{quantmod} functions
#' that returns the results as a \code{tibble}. The main advantage is the
#' function can be used with \code{purrr}, \code{tidyr}, and \code{dplyr} verbs.
#'
#' @name tq_transform
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyverse)
#' library(tidyquant)
#'
#' ##### Basic Functionality
#'
#' fb_stock_prices  <- tq_get("FB", get = "stock.prices")
#'
#' # lag
#'
#'
#' # next
#'
#'
#' # delt
#'
#' # first
#'
#'
#' # last
#'
#'
#' # to.period
#'
#'
#' # period.apply
#'
#'
#' # period.return
#' fb_stock_prices %>%
#'     tq_transform(col = adjusted, transform = "period.return", k = 1)
#'
#'
#' ##### Non-standard evaluation: Programming with tq_tranform_()
#' col_var <- "adjusted"
#' tq_transform_(fb_stock_prices, col = col_var, transform = "period.return")

tq_transform_ <- function(x, col, transform, ...) {

    # Check transform
    transform <- stringr::str_to_lower(transform) %>%
        stringr::str_replace_all("[[:punct:]]","")

    transform_list <-   c("lag",
                          "next",
                          "delt",
                          "first",
                          "last",
                          "toperiod",
                          "periodapply",
                          "periodreturn")
    if (!(transform %in% transform_list)) {
        stop("Error: `transform` must be a valid entry")
    }

    # Check x
    if (!tibble::is_tibble(x)) {
        stop("Error: x must be a tibble.")
    }

    # Check col

    # Setup switches based on transform
    ret <- switch(transform,
                  "lag"           = tq_transform_util_1(x, col, transform, ...),
                  "next"          = tq_transform_util_1(x, col, transform, ...),
                  "delt"          = tq_transform_delt(x, col, ...),
                  "first"         = tq_transform_first(x, col, ...),
                  "last"          = tq_transform_last(x, col, ...),
                  "toperiod"      = tq_transform_to_period(x, col, ...),
                  "periodapply"   = tq_transform_period_apply(x, col, ...),
                  "periodreturn"  = tq_transform_util_1(x, col, transform, ...)
                  )

    ret


}

#' @rdname tq_transform
#' @export
tq_transform <- function(x, col, transform, ...) {

    col = col_name(substitute(col))

    tq_transform_(x, col, transform, ...)

}
