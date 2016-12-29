#' Mutates quantitative data (adds new variables to existing tibble)
#'
#' @param data A \code{tibble} (tidy data frame) of data from \code{\link{tq_get}}.
#' @param x_fun The \code{quantmod} function that identifes columns to pass to
#' the mutatation function. OHLCV is \code{quantmod} terminology for
#' open, high, low, close, and volume. Options include c(Op, Hi, Lo, Cl, Vo, Ad,
#' HLC, OHLC, OHLCV).
#' @param .x,.y Column names of variables to be passed to the mutatation
#' function (instead of OHLC functions).
#' @param mutate_fun The mutation function from either the \code{xts},
#' \code{quantmod}, or \code{TTR} package. Execute \code{tq_mutate_fun_options()}
#' to see the full list of options by package.
#' @param ... Additional parameters passed to the appropriate mutatation
#' function.
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details \code{tq_mutate} is a very flexible wrapper for various \code{xts},
#' \code{quantmod} and \code{TTR} functions. The main advantage is the
#' results are returned as a \code{tibble} and the
#' function can be used with the \code{tidyverse}.
#'
#' \code{x_fun} is one of the various \code{quantmod} Open, High, Low, Close (OHLC) functions.
#' The function returns a column or set of columns from \code{data} that are passed to the
#' \code{mutate_fun}. In Example 1 below, \code{Cl} returns the "close" price and sends
#' this to the mutate function, \code{periodReturn}.
#'
#' \code{mutate_fun} is the function that performs the work. In Example 1, this
#' is \code{periodReturn}, which calculates the period returns. The \code{...}
#' functions are additional arguments passed to the \code{mutate_fun}. Think of
#' the whole operation in Example 1 as the close price, obtained by \code{x_fun = Cl},
#' is being sent to the \code{periodReturn} function along
#' with additional arguments defining how to perform the period return, which
#' includes \code{period = "daily"} and \code{type = "log"}.
#'
#' \code{tq_mutate_xy} is designed to enable working with (1) mutatation
#' functions that require two primary inputs (e.g. EVWMA, VWAP, etc) and (2) data
#' that is not in OHLC format. Example 2 shows the first benefit in action:
#' using the EVWMA function that uses volume to defind the moving average period.
#' The two variables do not fall into a single OHLC code (i.e. CV does not exist).
#' The xy form gets us out of this problem. Example 3 shows the second benefit
#' in action: Some functions are useful to non-OHLC data, and defining .x = price
#' allows us to mutate WTI crude prices from daily to monthly periodicity.
#'
#' \code{tq_mutate_} and \code{tq_mutate_xy_} are setup for Non-Standard
#' Evaluation (NSE). This enables programatically changing column names by modifying
#' the text representations. Example 4 shows the difference in implemenation.
#' Note that character strings are being passed to the variables instead of
#' unquoted variable names. See \code{vignette("nse")} for more information.
#'
#' @seealso \code{\link{tq_transform}}
#'
#' @name tq_mutate
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
#'     tq_mutate(x_fun = Cl, mutate_fun = periodReturn,
#'                  period = "daily", type = "log")
#'
#' # Example 2: Use tq_mutate_xy to use functions with two columns required
#' fb_stock_prices %>%
#'     tq_mutate_xy(.x = close, .y = volume, mutate_fun = EVWMA)
#'
#' # Example 3: Using tq_mutate_xy to work with non-OHLC data
#' tq_get("DCOILWTICO", get = "economic.data") %>%
#'     tq_mutate_xy(.x = price, mutate_fun = lag.xts, k = 1, na.pad = TRUE)
#'
#' # Example 4: Non-standard evaluation:
#' # Programming with tq_mutate_() and tq_mutate_xy_()
#' col_name <- "adjusted"
#' mutate <- c("MACD", "SMA")
#' tq_mutate_xy_(fb_stock_prices, .x = col_name, mutate_fun = mutate[[1]])

# PRIMARY FUNCTIONS ----

tq_mutate <- function(data, x_fun = OHLCV, mutate_fun, ...) {

    # Convert to NSE
    x_fun <- deparse(substitute(x_fun))
    mutate_fun <- deparse(substitute(mutate_fun))

    tq_mutate_(data = data, x_fun = x_fun, mutate_fun = mutate_fun, ...)

}

#' @rdname tq_mutate
#' @export
tq_mutate_ <- function(data, x_fun = "OHLCV", mutate_fun, ...) {

    # Get transformation
    ret <- tq_transform_(data = data, x_fun = x_fun, transform_fun = mutate_fun, ...)

    ret <- merge_two_tibbles(tib1 = data, tib2 = ret)

    ret

}

#' @rdname tq_mutate
#' @export
tq_mutate_xy <- function(data, .x, .y = NULL, mutate_fun, ...) {

    # Convert to NSE
    .x <- deparse(substitute(.x))
    .y <- deparse(substitute(.y))
    mutate_fun <- deparse(substitute(mutate_fun))

    tq_mutate_xy_(data = data, .x = .x, .y = .y, mutate_fun = mutate_fun, ...)

}

#' @rdname tq_mutate
#' @export
tq_mutate_xy_ <- function(data, .x, .y = NULL, mutate_fun, ...) {


    # Get transformation
    ret <- tq_transform_xy_(data = data, .x = .x, .y = .y, transform_fun = mutate_fun, ...)

    ret <- merge_two_tibbles(tib1 = data, tib2 = ret)

    ret

}

#' @rdname tq_mutate
#' @export
tq_mutate_fun_options <- function() tq_transform_fun_options()


# UTILITY FUNCTIONS ----

# See utils.R for get_col_name_date_or_date_time()

merge_two_tibbles <- function(tib1, tib2) {

    # Gracefully handle errors
    ret <- tryCatch({

        # Find date or date-time col
        col_name_date_tib1 <- get_col_name_date_or_date_time(tib1)
        tib1_date <- eval(parse(text = paste0("tib1$", col_name_date_tib1)))

        col_name_date_tib2 <- get_col_name_date_or_date_time(tib2)
        tib2_date <- eval(parse(text = paste0("tib2$", col_name_date_tib2)))

        # Merge results
        if (identical(nrow(tib1), nrow(tib1))) {
            if (identical(tib1_date, tib2_date)) {
                merge(tib1, tib2, by.x = col_name_date_tib1, by.y = col_name_date_tib2) %>%
                    as_tibble()
            }
        } else {

            warning("Could not join. Incompatible structures.")
            NA
        }

    }, error = function(e) {

        warning(paste0(e))
        NA

    })
}
