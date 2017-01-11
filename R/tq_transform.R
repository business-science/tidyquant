#' Transforms quantitative data (returns new variables in new tibble)
#'
#' @param data A \code{tibble} (tidy data frame) of data from \code{\link{tq_get}}.
#' @param ohlc_fun The \code{quantmod} function that identifes columns to pass to
#' the transformation function. OHLCV is \code{quantmod} terminology for
#' open, high, low, close, and volume. Options include c(Op, Hi, Lo, Cl, Vo, Ad,
#' HLC, OHLC, OHLCV).
#' @param x,y Column names of variables to be passed to the transformation
#' function (instead of OHLC functions).
#' @param transform_fun The transformation function from either the \code{xts},
#' \code{quantmod}, or \code{TTR} package. Execute \code{tq_transform_fun_options()}
#' to see the full list of options by package.
#' @param ... Additional parameters passed to the appropriate transformation
#' function.
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details \code{tq_transform} is a very flexible wrapper for various \code{xts},
#' \code{quantmod} and \code{TTR} functions. The main advantage is the
#' results are returned as a \code{tibble} and the
#' function can be used with the \code{tidyverse}.
#'
#' \code{ohlc_fun} is one of the various \code{quantmod} Open, High, Low, Close (OHLC) functions.
#' The function returns a column or set of columns from \code{data} that are passed to the
#' \code{transform_fun}. In Example 1 below, \code{Cl} returns the "close" price and sends
#' this to the transform function, \code{periodReturn}.
#'
#' \code{transform_fun} is the function that performs the work. In Example 1, this
#' is \code{periodReturn}, which calculates the period returns. The \code{...}
#' functions are additional arguments passed to the \code{transform_fun}. Think of
#' the whole operation in Example 1 as the close price, obtained by \code{ohlc_fun = Cl},
#' is being sent to the \code{periodReturn} function along
#' with additional arguments defining how to perform the period return, which
#' includes \code{period = "daily"} and \code{type = "log"}.
#'
#' \code{tq_transform_xy} is designed to enable working with (1) transformation
#' functions that require two primary inputs (e.g. EVWMA, VWAP, etc) and (2) data
#' that is not in OHLC format. Example 2 shows the first benefit in action:
#' using the EVWMA function that uses volume to defind the moving average period.
#' The two variables do not fall into a single OHLC code (i.e. CV does not exist).
#' The xy form gets us out of this problem. Example 3 shows the second benefit
#' in action: Some functions are useful to non-OHLC data, and defining x = price
#' allows us to transform WTI crude prices from daily to monthly periodicity.
#'
#' \code{tq_tranform_} and \code{tq_transform_xy_} are setup for Non-Standard
#' Evaluation (NSE). This enables programatically changing column names by modifying
#' the text representations. Example 4 shows the difference in implementation.
#' Note that character strings are being passed to the variables instead of
#' unquoted variable names. See \code{vignette("nse")} for more information.
#'
#' @seealso \code{\link{tq_mutate}}
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
#' # Example 1: Return logarithmic daily returns using periodReturn()
#' fb_stock_prices %>%
#'     tq_transform(ohlc_fun = Cl, transform_fun = periodReturn,
#'                  period = "daily", type = "log")
#'
#' # Example 2: Use tq_transform_xy to use functions with two columns required
#' fb_stock_prices %>%
#'     tq_transform_xy(x = close, y = volume, transform_fun = EVWMA)
#'
#' # Example 3: Using tq_transform_xy to work with non-OHLC data
#' tq_get("DCOILWTICO", get = "economic.data") %>%
#'     tq_transform_xy(x = price, transform_fun = to.period, period = "months")
#'
#' # Example 4: Non-standard evaluation:
#' # Programming with tq_tranform_() and tq_transform_xy_()
#' col_name <- "adjusted"
#' transform <- "periodReturn"
#' period <- c("daily", "weekly", "monthly")
#' tq_transform_xy_(fb_stock_prices, x = col_name, transform_fun = transform,
#'                  period = period[[1]])

# PRIMARY FUNCTIONS ----

#' @rdname tq_transform
#' @export
tq_transform <- function(data, ohlc_fun = OHLCV, transform_fun, ...) {

    # Convert to NSE
    ohlc_fun <- deparse(substitute(ohlc_fun))
    transform_fun <- deparse(substitute(transform_fun))

    # Patch for grouped data frames
    if (dplyr::is.grouped_df(data)) {

        tq_transform_grouped_df_(data, ohlc_fun, transform_fun, ...)

    } else {

        tq_transform_base_(data, ohlc_fun, transform_fun, ...)

    }
}

#' @rdname tq_transform
#' @export
tq_transform_ <- function(data, ohlc_fun = "OHLCV", transform_fun, ...) {

    # Patch for grouped data frames
    if (dplyr::is.grouped_df(data)) {

        tq_transform_grouped_df_(data, ohlc_fun, transform_fun, ...)

    } else {

        tq_transform_base_(data, ohlc_fun, transform_fun, ...)

    }

}

#' @rdname tq_transform
#' @export
tq_transform_xy <- function(data, x, y = NULL, transform_fun, ...) {

    # Convert to NSE
    x <- deparse(substitute(x))
    y <- deparse(substitute(y))
    transform_fun <- deparse(substitute(transform_fun))

    # Patch for grouped data frames
    if (dplyr::is.grouped_df(data)) {

        tq_transform_xy_grouped_df_(data, x, y, transform_fun, ...)

    } else {

        tq_transform_xy_base_(data, x, y, transform_fun, ...)

    }
}

#' @rdname tq_transform
#' @export
tq_transform_xy_ <- function(data, x, y = NULL, transform_fun, ...) {

    # Patch for grouped data frames
    if (dplyr::is.grouped_df(data)) {

        tq_transform_xy_grouped_df_(data, x, y, transform_fun, ...)

    } else {

        tq_transform_xy_base_(data, x, y, transform_fun, ...)

    }
}

#' @rdname tq_transform
#' @export
tq_transform_fun_options <- function() {

    # zoo rollapply functions
    pkg_regex_zoo <- "roll"
    funs_zoo <- ls("package:zoo")[stringr::str_detect(ls("package:zoo"), pkg_regex_zoo)]

    # xts apply.period, to.period, lag and diff functions
    pkg_regex_xts <- "apply|to\\.|period|lag|diff"
    funs_xts <- ls("package:xts")[stringr::str_detect(ls("package:xts"), pkg_regex_xts)]

    # quantmod periodReturns, Delt, series functions
    pkg_regex_quantmod <- "Return|Delt|Lag|Next|^Op..|^Cl..|^Hi..|^Lo..|^series"
    funs_quantmod <- ls("package:quantmod")[stringr::str_detect(ls("package:quantmod"), pkg_regex_quantmod)]

    # TTR functions
    pkg_regex_ttr <- "^get*|^stock|^naCh" # NOT these
    funs_ttr <- ls("package:TTR")[!stringr::str_detect(ls("package:TTR"), pkg_regex_ttr)]

    fun_options <- list(zoo = funs_zoo,
                        xts = funs_xts,
                        quantmod = funs_quantmod,
                        TTR = funs_ttr)

    fun_options

}


# UTILITY FUNCTIONS ----

# See utils-date.R for date and datetime functions

# Base functions ----

tq_transform_base_ <- function(data, ohlc_fun = "OHLCV", transform_fun, ...) {

    # Check transform_fun in xts, quantmod or TTR
    check_transform_fun_options(transform_fun)

    # Check for x: either x, HLC, or price arguments
    check_ohlc_fun_options(ohlc_fun)

    # Check data
    check_data_is_data_frame(data)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Convert inputs to functions
    ohlc_fun <- paste0("quantmod::", ohlc_fun)
    fun_x <- eval(parse(text = ohlc_fun))
    fun_transform <- eval(parse(text = transform_fun))

    # Patch for to.period functions
    is_period_fun <- detect_period_fun(transform_fun)

    # Apply functions
    if (is_period_fun) {
        # Add arg: OHLC = FALSE
        ret <- data %>%
            as_xts_(date_col = date_col_name) %>%
            fun_x() %>%
            fun_transform(OHLC = FALSE, ...)

    } else {
        ret <- data %>%
            as_xts_(date_col = date_col_name) %>%
            fun_x() %>%
            fun_transform(...)
    }

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name, time_zone)

    ret

}

tq_transform_xy_base_ <- function(data, x, y = NULL, transform_fun, ...) {

    # Check transform_fun in xts, quantmod or TTR
    check_transform_fun_options(transform_fun)

    # Check data
    check_data_is_data_frame(data)

    # Check x and y
    check_x_y_valid(data, x, y)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Convert inputs to functions
    fun_transform <- eval(parse(text = transform_fun))

    # Patch for to.period functions
    is_period_fun <- detect_period_fun(transform_fun)

    # Apply functions
    if (is_period_fun) {
        # Add arg: OHLC = FALSE
        if (y == "NULL" || is.null(y)) {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transform(eval(parse(text = x)), OHLC = FALSE, ...)
        } else {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transform(eval(parse(text = x)),
                              eval(parse(text = y)),
                              OHLC = FALSE,
                              ...)
        }
    } else {
        if (y == "NULL" || is.null(y)) {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transform(eval(parse(text = x)), ...)
        } else {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transform(eval(parse(text = x)),
                              eval(parse(text = y)),
                              ...)
        }
    }

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name, time_zone)

    ret

}

# Patches for grouped data frames -----

tq_transform_grouped_df_ <- function(data, ohlc_fun, transform_fun, ...) {

    group_names <- dplyr::groups(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = data %>%
                          purrr::map(~ tq_transform_base_(data = .x,
                                                          ohlc_fun = ohlc_fun,
                                                          transform_fun = transform_fun,
                                                          ...))
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names)
}

tq_transform_xy_grouped_df_ <- function(data, x, y, transform_fun, ...) {

    group_names <- dplyr::groups(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = data %>%
                          purrr::map(~ tq_transform_xy_base_(data = .x,
                                                             x = x,
                                                             y = y,
                                                             transform_fun = transform_fun,
                                                             ...))
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names)
}

# Checks -----

check_transform_fun_options <- function(fun) {
    fun_options <- tq_transform_fun_options() %>%
        unlist()
    if (!(fun %in% fun_options)) {
        stop(paste0("fun = ", fun, " not a valid option."))
    }
}

check_ohlc_fun_options <- function(fun) {
    x_options <- c("Op", "Hi", "Lo", "Cl", "Vo", "Ad",
                   "HLC", "OHLC", "OHLCV")
    if (!(fun %in% x_options)) {
        stop(paste0("ohlc_fun = ", fun, " not a valid name."))
    }
}

check_data_is_data_frame <- function(data) {
    if (!inherits(data, "data.frame")) {
        stop("`data` must be a tibble or data.frame.")
    }
}

check_x_y_valid <- function(data, x, y) {
    if (!(x %in% names(data))) stop(paste0("x = ", x, " not a valid name."))
    if (y != "NULL" && !is.null(y)) {
        if (!(y %in% names(data))) stop(paste0("y = ", y, " not a valid name."))
    }
}

# Other -----

coerce_to_tibble <- function(data, date_col_name, time_zone, transform_fun) {

    # Coerce to tibble
    ret <- data %>%
        as_tibble(preserve_row_names = TRUE) %>%
        dplyr::rename(date = row.names)

    # Convert to date
    ret <- convert_date_cols(ret, time_zone)

    # Rename row.names
    names(ret)[[1]] <- date_col_name

    ret
}

detect_period_fun <- function(fun) {
    is_period_fun <- FALSE
    to_period_funs <- tq_transform_fun_options() %>%
        unlist() %>%
        stringr::str_subset("^to")
    if (fun %in% to_period_funs) is_period_fun <- TRUE
    is_period_fun
}

