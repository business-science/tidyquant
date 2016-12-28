#' Transforms quantitative data (returns new variables in new tibble)
#'
#' @param data A \code{tibble} (tidy data frame) of data from \code{\link{tq_get}}.
#' @param x_fun The \code{quantmod} function that identifes columns to pass to
#' the transformation function. OHLCV is \code{quantmod} terminology for
#' open, high, low, close, and volume. Options include c(Op, Hi, Lo, Cl, Vo, Ad,
#' HLC, OHLC, OHLCV).
#' @param .x,.y Column names of variables to be passed to the transformation
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
#' \code{x_fun} is one of the various \code{quantmod} Open, High, Low, Close (OHLC) functions.
#' The function returns a column or set of columns from \code{data} that are passed to the
#' \code{transform_fun}. In Example 1 below, \code{Cl} returns the "close" price and sends
#' this to the transform function, \code{periodReturn}.
#'
#' \code{transform_fun} is the function that performs the work. In Example 1, this
#' is \code{periodReturn}, which calculates the period returns. The \code{...}
#' functions are additional arguments passed to the \code{transform_fun}. Think of
#' the whole operation in Example 1 as the close price, obtained by \code{x_fun = Cl},
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
#' in action: Some functions are useful to non-OHLC data, and defining .x = price
#' allows us to transform WTI crude prices from daily to monthly periodicity.
#'
#' \code{tq_tranform_} and \code{tq_transform_xy_} are setup for Non-Standard
#' Evaluation (NSE). This enables programatically changing column names by modifying
#' the text representations. Example 4 shows the difference in implemenation.
#' Note that character strings are being passed to the variables instead of
#' unquoted variable names. See \code{vignette("nse")} for more information.
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
#'     tq_transform(x_fun = Cl, transform_fun = periodReturn,
#'                  period = "daily", type = "log")
#'
#' # Example 2: Use tq_transform_xy to use functions with two columns required
#' fb_stock_prices %>%
#'     tq_transform_xy(.x = close, .y = volume, transform_fun = EVWMA)
#'
#' # Example 3: Using tq_transform_xy to work with non-OHLC data
#' tq_get("DCOILWTICO", get = "economic.data") %>%
#'     tq_transform_xy(.x = price, transform_fun = to.period,
#'                     period = "months", OHLC = FALSE)
#'
#' # Example 4: Non-standard evaluation:
#' # Programming with tq_tranform_() and tq_transform_xy_()
#' col_name <- "adjusted"
#' transform <- "periodReturn"
#' period <- c("daily", "weekly", "monthly")
#' tq_transform_xy_(fb_stock_prices, .x = col_name, transform_fun = transform,
#'                  period = period[[1]])

# PRIMARY FUNCTIONS ----

tq_transform <- function(data, x_fun = OHLCV, transform_fun, ...) {

    # Convert to NSE
    x_fun <- deparse(substitute(x_fun))
    transform_fun <- deparse(substitute(transform_fun))

    tq_transform_(data, x_fun, transform_fun, ...)

}

#' @rdname tq_transform
#' @export
tq_transform_ <- function(data, x_fun = "OHLCV", transform_fun, ...) {

    # Check transform_fun in xts, quantmod or TTR
    fun_options <- tq_transform_fun_options() %>%
        unlist()

    if (!(transform_fun %in% fun_options)) {
        stop(paste0("transform_fun = '",
                    transform_fun,
                    "' not a valid option."))
    }

    # Check for x: either x, HLC, or price arguments
    x_options <- c("Op", "Hi", "Lo", "Cl", "Vo", "Ad",
                   "HLC", "OHLC", "OHLCV")
    if (!(x_fun %in% x_options)) {
        stop(paste0("x = '",
                    x_fun,
                    "' not a valid option."))
    }

    # Check data
    if (!inherits(data, "data.frame")) {
        stop("`data` must be a tibble or data.frame.")
    }

    # Find date col
    date_cols <- find_date_cols(data)
    date_cols <- date_cols[date_cols == TRUE]
    if (length(date_cols) == 0) {
        stop("No date column found in x")
    }
    date_col_name <- names(date_cols)[[1]]

    # Convert inputs to functions
    fun_x <- eval(parse(text = x_fun))
    fun_transform <- eval(parse(text = transform_fun))

    # Apply fun

    ret <- tryCatch({

        data %>%
            as_xts_(date_col = date_col_name) %>%
            fun_x() %>%
            fun_transform(...)


    }, error = function(e) {

        warning("Error in tranform_fun")
        NA

    })

    if (xts::is.xts(ret)) {

        # Coerce to tibble
        ret <- ret %>%
            as_tibble(preserve_row_names = T) %>%
            dplyr::rename(date = row.names)

        # Fix names
        names(ret) <- names(ret) %>%
            stringr::str_to_lower() %>%
            stringr::str_replace_all("[[:punct:]]", "")

        # Detect date and convert
        date_cols <- suppressWarnings(find_date_cols(ret))
        date_cols <- date_cols[date_cols == TRUE]
        if (length(date_cols) > 0) {
            ret <- dplyr::mutate(ret, date = lubridate::ymd(date))
        }

    }

    ret

}

#' @rdname tq_transform
#' @export
tq_transform_xy <- function(data, .x, .y = NULL, transform_fun, ...) {

    # Convert to NSE
    # .x <- deparse(substitute(.x))
    # if (!is.null(.y)) .y <- deparse(substitute(.y))
    .x <- deparse(substitute(.x))
    .y <- deparse(substitute(.y))
    transform_fun <- deparse(substitute(transform_fun))

    tq_transform_xy_(data, .x, .y, transform_fun, ...)

}

#' @rdname tq_transform
#' @export
tq_transform_xy_ <- function(data, .x, .y = NULL, transform_fun, ...) {

    # Check transform_fun in xts, quantmod or TTR
    fun_options <- tq_transform_fun_options() %>%
        unlist()

    if (!(transform_fun %in% fun_options)) {
        stop(paste0("transform_fun = '",
                    transform_fun,
                    "' not a valid option."))
    }

    # Check data
    if (!inherits(data, "data.frame")) {
        stop("`data` must be a tibble or data.frame.")
    }

    # Find date col
    date_cols <- find_date_cols(data)
    date_cols <- date_cols[date_cols == TRUE]
    if (length(date_cols) == 0) {
        stop("No date column found in x")
    }
    date_col_name <- names(date_cols)[[1]]

    # Convert inputs to functions
    fun_transform <- eval(parse(text = transform_fun))

    # Apply fun

    ret <- tryCatch({

        if (.y == "NULL" || is.null(.y)) {
            data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transform(eval(parse(text = .x)), ...)
        } else {
            data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transform(eval(parse(text = .x)),
                              eval(parse(text = .y)), ...)
        }



    }, error = function(e) {

        warning("Error in tranform_fun")
        NA

    })

    if (xts::is.xts(ret)) {

        # Coerce to tibble
        ret <- ret %>%
            as_tibble(preserve_row_names = T) %>%
            dplyr::rename(date = row.names)

        # Fix names
        names(ret) <- names(ret) %>%
            stringr::str_to_lower() %>%
            stringr::str_replace_all("[[:punct:]]", "")

        # Detect date and convert
        date_cols <- suppressWarnings(find_date_cols(ret))
        date_cols <- date_cols[date_cols == TRUE]
        if (length(date_cols) > 0) {
            ret <- dplyr::mutate(ret, date = lubridate::ymd(date))
        }

    }

    ret

}

#' @rdname tq_transform
#' @export
tq_transform_fun_options <- function() {

    pkg_regex_xts <- "apply|to\\.|period|lag|diff"
    funs_xts <- ls("package:xts")[stringr::str_detect(ls("package:xts"), pkg_regex_xts)]

    pkg_regex_quantmod <- "Return|Delt|Lag|Next|^Op..|^Cl..|^Hi..|^Lo..|^series"
    funs_quantmod <- ls("package:quantmod")[stringr::str_detect(ls("package:quantmod"), pkg_regex_quantmod)]

    pkg_regex_ttr <- "^get*|^stock|^naCh" # NOT these
    funs_ttr <- ls("package:TTR")[!stringr::str_detect(ls("package:TTR"), pkg_regex_ttr)]

    fun_options <- list(xts = funs_xts,
                        quantmod = funs_quantmod,
                        TTR = funs_ttr)

    fun_options

}


# UTILITY FUNCTIONS ----

# See utils.R for find_date_cols
