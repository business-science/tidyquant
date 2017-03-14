#' Mutates quantitative data
#'
#' `tq_mutate()` adds new variables to an existing tibble;
#' `tq_transmute()` returns only newly created columns and is typically
#' used when periodicity changes
#'
#' @name tq_mutate
#'
#' @param data A `tibble` (tidy data frame) of data typically from [tq_get()].
#' @param ohlc_fun The `quantmod` function that identifies columns to pass to
#' the mutatation function. OHLCV is `quantmod` terminology for
#' open, high, low, close, and volume. Options include c(Op, Hi, Lo, Cl, Vo, Ad,
#' HLC, OHLC, OHLCV).
#' @param x,y Column names of variables to be passed to the mutatation
#' function (instead of OHLC functions).
#' @param mutate_fun The mutation function from either the `xts`,
#' `quantmod`, or `TTR` package. Execute `tq_mutate_fun_options()`
#' to see the full list of options by package.
#' @param col_rename A string or character vector containing names that can be used
#' to quickly rename columns.
#' @param transform_fun Deprecated. Use `mutate_fun`.
#' @param ... Additional parameters passed to the appropriate mutatation
#' function.
#'
#' @return Returns mutated data in the form of a `tibble` object.
#'
#' @details
#' `tq_mutate` and `tq_transmute` are very flexible wrappers for various `xts`,
#' `quantmod` and `TTR` functions. The main advantage is the
#' results are returned as a `tibble` and the
#' function can be used with the `tidyverse`. `tq_mutate` is used when additional
#' columns are added to the return data frame. `tq_transmute` works exactly like `tq_mutate`
#' except it only returns the newly created columns. This is helpful when
#' changing periodicity where the new columns would not have the same number of rows
#' as the original tibble.
#'
#' `ohlc_fun` is one of the various `quantmod` Open, High, Low, Close (OHLC) functions.
#' The function returns a column or set of columns from `data` that are passed to the
#' `mutate_fun`. In Example 1 below, `Cl` returns the "close" price and sends
#' this to the mutate function, `periodReturn`.
#'
#' `mutate_fun` is the function that performs the work. In Example 1, this
#' is `periodReturn`, which calculates the period returns. The `...`
#' functions are additional arguments passed to the `mutate_fun`. Think of
#' the whole operation in Example 1 as the close price, obtained by `ohlc_fun = Cl`,
#' is being sent to the `periodReturn` function along
#' with additional arguments defining how to perform the period return, which
#' includes `period = "daily"` and `type = "log"`.
#'
#' `tq_mutate_xy` and `tq_transmute_xy` are designed to enable working with (1) mutatation
#' functions that require two primary inputs (e.g. EVWMA, VWAP, etc) and (2) data
#' that is not in OHLC format. Example 2 shows the first benefit in action:
#' using the EVWMA function that uses volume to defind the moving average period.
#' The two variables do not fall into a single OHLC code (i.e. CV does not exist).
#' The xy form gets us out of this problem. Example 3 shows the second benefit
#' in action: Some functions are useful to non-OHLC data, and defining x = price
#' allows us to mutate WTI crude prices from daily to monthly periodicity.
#'
#' `tq_mutate_data` and `tq_tranmute_data` are designed to enable working with
#' functions that require "data" as the input (i.e. `rollapply`). This allows
#' working with the `FUN` argument and apply `by.column = FALSE`, which is
#' specifically designed for handling complex functions that require multiple
#' column inputs into the `FUN` function (e.g. rolling regressions using `lm`).
#' Example 4 shows how to apply a rolling regression.
#'
#' `tq_mutate_`, `tq_mutate_xy_`, `tq_transmute_`, and `tq_transmute_xy_`
#' are setup for Non-Standard
#' Evaluation (NSE). This enables programatically changing column names by modifying
#' the text representations. Example 5 shows the difference in implementation.
#' Note that character strings are being passed to the variables instead of
#' unquoted variable names. See `vignette("nse")` for more information.
#'
#' `tq_mutate_fun_options` and `tq_transmute_fun_options` return a list of various
#' financial functions that are compatible with `tq_mutate` and `tq_transmute`,
#' respectively.
#'
#' @seealso [tq_get()]
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' ##### Basic Functionality
#'
#' fb_stock_prices  <- tq_get("FB",
#'                            get  = "stock.prices",
#'                            from = "2016-01-01",
#'                            to   = "2016-12-31")
#'
#' # Example 1: Return logarithmic daily returns using periodReturn()
#' fb_stock_prices %>%
#'     tq_mutate(ohlc_fun = Cl, mutate_fun = periodReturn,
#'               period = "daily", type = "log")
#'
#' # Example 2: Use tq_mutate_xy to use functions with two columns required
#' fb_stock_prices %>%
#'     tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA,
#'                  col_rename = "EVWMA")
#'
#' # Example 3: Using tq_mutate_xy to work with non-OHLC data
#' tq_get("DCOILWTICO", get = "economic.data") %>%
#'     tq_mutate_xy(x = price, mutate_fun = lag.xts, k = 1, na.pad = TRUE)
#'
#' # Example 4: Using tq_mutate_data to apply a rolling regression
#' fb_returns <- fb_stock_prices %>%
#'     tq_transmute(Ad, periodReturn, period = "monthly", col_rename = "fb.returns")
#' xlk_returns <- tq_get("XLK", from = "2016-01-01", to = "2016-12-31") %>%
#'     tq_transmute(Ad, periodReturn, period = "monthly", col_rename = "xlk.returns")
#' returns_combined <- left_join(fb_returns, xlk_returns, by = "date")
#' regr_fun <- function(data) {
#'     coef(lm(fb.returns ~ xlk.returns, data = as_data_frame(data)))
#' }
#' returns_combined %>%
#'     tq_mutate_data(mutate_fun = rollapply,
#'                    width      = 6,
#'                    FUN        = regr_fun,
#'                    by.column  = FALSE,
#'                    col_rename = c("coef.0", "coef.1"))
#'
#' # Example 5: Non-standard evaluation:
#' # Programming with tq_mutate_() and tq_mutate_xy_()
#' col_name <- "adjusted"
#' mutate <- c("MACD", "SMA")
#' tq_mutate_xy_(fb_stock_prices, x = col_name, mutate_fun = mutate[[1]])
NULL

# tq_mutate ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_mutate <- function(data, ohlc_fun = OHLCV, mutate_fun, col_rename = NULL, ...) {

    # NSE
    tq_mutate_(data       = data,
               ohlc_fun   = lazyeval::expr_text(ohlc_fun),
               mutate_fun = lazyeval::expr_text(mutate_fun),
               col_rename = col_rename,
               ...        = ...)
}

#' @rdname tq_mutate
#' @export
tq_mutate_ <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_mutate_", data)
}

# tq_mutate method dispatch --------------------------------------------------------------------------------

#' @export
tq_mutate_.default <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_mutate_.tbl_df <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    # Get transformation
    ret <- tq_transmute_(data          = data,
                         ohlc_fun      = ohlc_fun,
                         mutate_fun    = mutate_fun,
                         col_rename    = col_rename,
                         ...           = ...)

    merge_two_tibbles(tib1 = data, tib2 = ret, mutate_fun)
}

#' @export
tq_mutate_.data.frame <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- as_tibble(data)

    # Get transformation
    ret <- tq_transmute_(data          = data,
                         ohlc_fun      = ohlc_fun,
                         mutate_fun    = mutate_fun,
                         col_rename    = col_rename,
                         ...           = ...)

    merge_two_tibbles(tib1 = data, tib2 = ret, mutate_fun)
}

# tq_mutate_xy ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_mutate_xy <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # NSE
    tq_mutate_xy_(data       = data,
                  x          = lazyeval::expr_text(x),
                  y          = lazyeval::expr_text(y),
                  mutate_fun = lazyeval::expr_text(mutate_fun),
                  col_rename = col_rename,
                  ...        = ...)
}

#' @rdname tq_mutate
#' @export
tq_mutate_xy_ <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_mutate_xy_", data)
}

# tq_mutate_xy method dispatch --------------------------------------------------------------------------------

#' @export
tq_mutate_xy_.default <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_mutate_xy_.tbl_df <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Get transformation
    ret <- tq_transmute_xy_(data          = data,
                            x             = x,
                            y             = y,
                            mutate_fun    = mutate_fun,
                            col_rename    = col_rename,
                            ...           = ...)

    merge_two_tibbles(tib1 = data, tib2 = ret, mutate_fun)
}

#' @export
tq_mutate_xy_.data.frame <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- as_tibble(data)

    # Get transformation
    ret <- tq_transmute_xy_(data          = data,
                            x             = x,
                            y             = y,
                            mutate_fun    = mutate_fun,
                            col_rename    = col_rename,
                            ...           = ...)

    merge_two_tibbles(tib1 = data, tib2 = ret, mutate_fun)
}

# tq_mutate_data ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_mutate_data <- function(data, mutate_fun, col_rename = NULL, ...) {

    # NSE
    tq_mutate_data_(data       = data,
                    mutate_fun = lazyeval::expr_text(mutate_fun),
                    col_rename = col_rename,
                    ...        = ...)
}

#' @rdname tq_mutate
#' @export
tq_mutate_data_ <- function(data, mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_mutate_data_", data)
}

# tq_mutate_data method dispatch --------------------------------------------------------------------------------

#' @export
tq_mutate_data_.default <- function(data, mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_mutate_data_.tbl_df <- function(data, mutate_fun, col_rename = NULL, ...) {

    # Get transformation
    ret <- tq_transmute_data_(data          = data,
                              mutate_fun    = mutate_fun,
                              col_rename    = col_rename,
                              ...           = ...)

    merge_two_tibbles(tib1 = data, tib2 = ret, mutate_fun)
}

#' @export
tq_mutate_data_.data.frame <- function(data, mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- as_tibble(data)

    # Get transformation
    ret <- tq_transmute_data_(data          = data,
                              mutate_fun    = mutate_fun,
                              col_rename    = col_rename,
                              ...           = ...)

    merge_two_tibbles(tib1 = data, tib2 = ret, mutate_fun)
}

# Function options -------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_mutate_fun_options <- function() {
    tq_transmute_fun_options()
}

# Utility ----------------------------------------------------------------------------------------------------

merge_two_tibbles <- function(tib1, tib2, mutate_fun) {

    # Merge results
    if (identical(nrow(tib1), nrow(tib2))) {

        # Arrange dates - Possibility of issue if dates not decending in tib1
        tib1 <- arrange_by_date(tib1)

        # Drop date column and groups
        tib2 <- drop_date_and_group_cols(tib2)

        # Replace bad names
        tib2 <- replace_bad_names(tib2, mutate_fun)

        # Replace duplicate names
        tib2 <- replace_duplicate_colnames(tib1, tib2)

        ret <- dplyr::bind_cols(tib1, tib2)

    } else {

        stop("Could not join. Incompatible structures.")
    }

    ret
}

replace_duplicate_colnames <- function(tib1, tib2) {

    # Collect column names
    name_list_tib1 <- colnames(tib1)
    name_list_tib2 <- colnames(tib2)
    name_list <- c(name_list_tib1, name_list_tib2)

    duplicates_exist <- detect_duplicates(name_list)

    # Iteratively add .1, .2, .3 ... onto end of column names
    if (duplicates_exist) {

        i <- 1

        while (duplicates_exist) {

            dup_names_stripped <-
                stringr::str_split(name_list[duplicated(name_list)],
                                   pattern = "\\.",
                                   simplify = FALSE) %>%
                sapply(function(x) x[[1]])

            name_list[duplicated(name_list)] <-
                stringr::str_c(dup_names_stripped, ".", i)

            i <- i + 1

            duplicates_exist <- detect_duplicates(name_list)

        }

        name_list_tib2 <- name_list[(ncol(tib1) + 1):length(name_list)]

        colnames(tib2) <- name_list_tib2
    }

    tib2
}

detect_duplicates <- function(name_list) {

    name_list %>%
        duplicated() %>%
        any()
}

# bad / restricted names are names that get selected unintetionally by OHLC functions
replace_bad_names <- function(tib, fun_name) {

    bad_names_regex <- "open|high|low|close|volume|adjusted|price"

    name_list_tib <- colnames(tib)
    name_list_tib_lower <- stringr::str_to_lower(name_list_tib)

    detect_bad_names <- stringr::str_detect(name_list_tib_lower, bad_names_regex)

    if (any(detect_bad_names)) {

        len <- length(name_list_tib_lower[detect_bad_names])
        name_list_tib[detect_bad_names] <- rep(fun_name, length.out = len)

    }

    colnames(tib) <- name_list_tib

    tib
}

arrange_by_date <- function(tib) {

    if (dplyr::is.grouped_df(tib)) {

        group_names <- dplyr::groups(tib)

        arrange_date <- function(tib) {
            date_col <- get_col_name_date_or_date_time(tib)
            tib %>%
                dplyr::arrange_(date_col)
        }

        tib %>%
            tidyr::nest() %>%
            dplyr::mutate(nested.col =
                              purrr::map(data, arrange_date)
            ) %>%
            dplyr::select(-data) %>%
            tidyr::unnest() %>%
            dplyr::group_by_(.dots = group_names)


    } else {

        tib <- tib %>%
            dplyr::arrange_(get_col_name_date_or_date_time(tib))

    }

    tib
}

drop_date_and_group_cols <- function(tib) {

    date_col <- get_col_name_date_or_date_time(tib)
    group_cols <- dplyr::groups(tib) %>%
        as.character()
    cols_to_remove <- c(date_col, group_cols)
    tib_names <- colnames(tib)
    cols_to_remove_logical <- tib_names %in% cols_to_remove
    tib_names_without_date_or_group <- tib_names[!cols_to_remove_logical]

    tib <- tib %>%
        dplyr::ungroup() %>%
        dplyr::select_(.dots = as.list(tib_names_without_date_or_group))
}
