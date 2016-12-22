# UTILITY FUNCTIONS FOR tq_transform()


# Util 1: Used for tq_transform() `transform` options:
#     lag,
#     next,
#     delt,
#     first
#     last
#     to.period
#     period.apply
#
tq_transform_util_1 <- function(x, col, transform, ...) {

    # Check input
    if (!sum(stringr::str_detect(names(x), pattern = paste0("^", col, "$"))) == 1) {
        stop(paste0("Error: x must contain a column named `", col, "`"))
    }
    if (!sum(stringr::str_detect(names(x), pattern = "^date$")) == 1) {
        stop("Error: x must contain a `date` column")
    }

    # Select columns
    x <- x %>%
        dplyr::select_("date", col)

    # Convert tibble to xts
    x <- xts::xts(x[,-1], order.by = x$date)

    # Setup switches based on transform
    vars <- switch(transform,
                   "lag"          = list(fun        = quantmod::Lag(x, ...),
                                         chr_fun    = "Lag",
                                         out_name   = "lag"),
                   "next"         = list(fun        = quantmod::Next(x, ...),
                                         chr_fun    = "Next",
                                         out_name   = "next"),
                   "delt"         = list(fun        = quantmod::getFX,
                                         chr_fun    = "getFX",
                                         out_name   = "exchange.rate"),
                   "first"        = list(fun        = quantmod::getDividends,
                                         chr_fun    = "getDividends",
                                         out_name   = "dividend"),
                   "last"         = list(fun        = quantmod::getFX,
                                         chr_fun    = "getFX",
                                         out_name   = "exchange.rate"),
                   "toperiod"     = list(fun        = quantmod::getDividends,
                                         chr_fun    = "getDividends",
                                         out_name   = "dividend"),
                   "periodapply"  = list(fun        = quantmod::getDividends,
                                         chr_fun    = "getDividends",
                                         out_name   = "dividend"),
                   "periodreturn" = list(fun        = quantmod::periodReturn(x, ...),
                                         chr_fun    = "periodReturn",
                                         out_name   = "period.return")
    )



    # Get periodReturn; Handle errors
    ret <- tryCatch({

        suppressWarnings(
            suppressMessages(
                vars$fun
            )
        )

    }, error = function(e) {

        warning(paste0("Error in ", vars$chr_fun,
                       ". See quantmod::", vars$chr_fun,
                       " documentation."))
        NA # Return NA on error

    })

    # Convert to tibble
    names(ret) <- vars$out_name
    ret <- ret %>%
        tidyquant::as_tibble(preserve_row_names = TRUE) %>%
        dplyr::rename(date = row.name) %>%
        dplyr::mutate(date = lubridate::ymd(date))

    ret

}







# tq_util_lag

tq_transform_lag <- function(x, col, k = 1) {

    # Check input
    if (!sum(stringr::str_detect(names(x), pattern = paste0("^", col, "$"))) == 1) {
        stop(paste0("Error: x must contain a column named `", col, "`"))
    }
    if (!sum(stringr::str_detect(names(x), pattern = "^date$")) == 1) {
        stop("Error: x must contain a `date` column")
    }

    # Select columns
    x <- x %>%
        dplyr::select_("date", col)

    # Convert tibble to xts
    x <- xts::xts(x[,-1], order.by = x$date)

    # Get periodReturn; Handle errors
    ret <- tryCatch({

        suppressWarnings(
            suppressMessages(
                quantmod::Lag(x, k)
            )
        )

    }, error = function(e) {

        warning("Error in Lag. See quantmod::Lag documentation.")
        NA # Return NA on error

    })

    # Convert to tibble
    names(ret) <- stringr::str_c("returns.", period, sep ="")
    ret <- ret %>%
        tidyquant::as_tibble(preserve_row_names = TRUE) %>%
        dplyr::rename(date = row.name) %>%
        dplyr::mutate(date = lubridate::ymd(date))

    ret


}

tq_transform_period_return <- function(x, col, period = "monthly",
                                       type = "arithmetic", leading = TRUE, ...) {

    # Check input
    if (!sum(stringr::str_detect(names(x), pattern = paste0("^", col, "$"))) == 1) {
        stop(paste0("Error: x must contain a column named `", col, "`"))
    }
    if (!sum(stringr::str_detect(names(x), pattern = "^date$")) == 1) {
        stop("Error: x must contain a `date` column")
    }

    # Select columns
    x <- x %>%
        dplyr::select_("date", col)

    # Convert tibble to xts
    x <- xts::xts(x[,-1], order.by = x$date)

    # Get periodReturn; Handle errors
    ret <- tryCatch({

        suppressWarnings(
            suppressMessages(
                quantmod::periodReturn(x, period = period, type = type, ...)
            )
        )

    }, error = function(e) {

        warning("Error in periodReturn. See quantmod::periodReturn documentation.")
        NA # Return NA on error

    })

    # Convert to tibble
    names(ret) <- stringr::str_c("returns.", period, sep ="")
    ret <- ret %>%
        tidyquant::as_tibble(preserve_row_names = TRUE) %>%
        dplyr::rename(date = row.name) %>%
        dplyr::mutate(date = lubridate::ymd(date))

    ret
}
