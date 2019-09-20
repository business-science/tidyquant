
# tq_transmute ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_transmute <- function(data, select = NULL, mutate_fun, col_rename = NULL, ohlc_fun = NULL, ...) {

    # Deprecate ohlc_fun in favor of select
    if (!missing(ohlc_fun)) {
        warning("Argument `ohlc_fun` is deprecated; please use `select` instead.",
                call. = FALSE)

        # As text
        ohlc_string <- lazyeval::expr_text(ohlc_fun)

        # Find select equivalent or die trying
        select <- map_ohlc_to_select(ohlc_string)

        # NSE and return
        return(
            tq_transmute_(data      = data,
                      select        = select,
                      mutate_fun    = lazyeval::expr_text(mutate_fun),
                      col_rename    = col_rename,
                      ...           = ...)
            )
    }

    # NSE
    tq_transmute_(data          = data,
                  select        = lazyeval::expr_text(select),
                  mutate_fun    = lazyeval::expr_text(mutate_fun),
                  col_rename    = col_rename,
                  ...           = ...)
}

#' @rdname tq_mutate
#' @export
tq_transmute_ <- function(data, select = NULL, mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_transmute_", data)
}

# tq_transmute method dispatch --------------------------------------------------------------------------------

#' @export
tq_transmute_.default <- function(data, select = NULL, mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_transmute_.tbl_df <- function(data, select = NULL, mutate_fun, col_rename = NULL, ...) {

    # Check mutate_fun in xts, quantmod or TTR
    check_transmute_fun_options(mutate_fun)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Get date column
    date_col <- dplyr::select(data, !!rlang::sym(date_col_name))

    # Implement select
    if (!(select == "NULL" || is.null(select))) data <- dplyr::select(data, !!rlang::parse_expr(select))

    # Only grab numeric columns
    numeric_cols <- data %>%
        dplyr::select_if(is.numeric)

    # Bind date with numeric columns that are within select
    data <- dplyr::bind_cols(date_col, numeric_cols)

    # Convert inputs to functions
    fun_transmute <- eval(parse(text = mutate_fun))

    # Patch for to.period functions
    is_period_fun <- detect_period_fun(mutate_fun)

    # Apply functions
    if (is_period_fun) {
        # Add arg: OHLC = FALSE
        ret <- data %>%
            timetk::tk_xts_(silent = TRUE) %>%
            fun_transmute(OHLC = FALSE, ...)

    } else {
        ret <- data %>%
            timetk::tk_xts_(silent = TRUE) %>%
            fun_transmute(...)
    }

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
                                                  time_zone, col_rename)

    ret
}

#' @export
tq_transmute_.data.frame <- function(data, select = NULL, mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- tibble::as_tibble(data)

    # Call tq_transmute_ for a tibble
    tq_transmute_(data          = data,
                  select        = select,
                  mutate_fun    = mutate_fun,
                  col_rename    = col_rename,
                  ...           = ...)
}

#' @export
tq_transmute_.grouped_df <- function(data, select = NULL, mutate_fun, col_rename = NULL, ...) {

    group_names <- dplyr::group_vars(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x         = data,
            .f         = tq_transmute_,
            select     = select,
            mutate_fun = mutate_fun,
            col_rename = col_rename,
            ...)
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}

#' @export
tq_transmute_.tbl_time <- function(data, select = NULL, mutate_fun, col_rename = NULL, ...) {
    if(!requireNamespace("tibbletime", quietly = TRUE)) {
        stop("tibbletime must be installed to use a tidyquant function on a tbl_time object.", call. = FALSE)
    }
    tibbletime::reconstruct(NextMethod(), data)
}

# tq_transmute_xy ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_transmute_xy <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # NSE
    tq_transmute_xy_(data          = data,
                     x             = lazyeval::expr_text(x),
                     y             = lazyeval::expr_text(y),
                     mutate_fun    = lazyeval::expr_text(mutate_fun),
                     col_rename    = col_rename,
                     ...           = ...)
}

#' @rdname tq_mutate
#' @export
tq_transmute_xy_ <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_transmute_xy_", data)
}

# tq_transmute_xy method dispatch --------------------------------------------------------------------------------

#' @export
tq_transmute_xy_.default <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_transmute_xy_.tbl_df <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Check mutate_fun in xts, quantmod or TTR
    check_transmute_fun_options(mutate_fun)

    # Check x and y
    check_x_y_valid(data, x, y)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Drop any non-numeric columns except for date
    date_col <- dplyr::select(data, !!rlang::sym(date_col_name))
    numeric_cols <- data %>%
        dplyr::select_if(is.numeric)
    data <- dplyr::bind_cols(date_col, numeric_cols)

    # Convert inputs to functions
    fun_transmute <- eval(parse(text = mutate_fun))

    # Patch for to.period functions
    is_period_fun <- detect_period_fun(mutate_fun)

    # Apply functions
    if (is_period_fun) {
        # Add arg: OHLC = FALSE
        if (y == "NULL" || is.null(y)) {
            ret <- data %>%
                timetk::tk_xts_(silent = TRUE) %$%
                fun_transmute(eval(parse(text = x)), OHLC = FALSE, ...)
        } else {
            ret <- data %>%
                timetk::tk_xts_(silent = TRUE) %$%
                fun_transmute(eval(parse(text = x)),
                              eval(parse(text = y)),
                              OHLC = FALSE,
                              ...)
        }
    } else {
        if (y == "NULL" || is.null(y)) {
            ret <- data %>%
                timetk::tk_xts_(silent = TRUE) %$%
                # OHLCV() %$%
                fun_transmute(eval(parse(text = x)), ...)
        } else {
            ret <- data %>%
                timetk::tk_xts_(silent = TRUE) %$%
                # OHLCV() %$%
                fun_transmute(eval(parse(text = x)),
                              eval(parse(text = y)),
                              ...)
        }
    }

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
                                                  time_zone, col_rename)

    ret
}

#' @export
tq_transmute_xy_.data.frame <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- tibble::as_tibble(data)

    # Call tq_transmute_xy_ for a tibble
    tq_transmute_xy_(data          = data,
                     x             = x,
                     y             = y,
                     mutate_fun    = mutate_fun,
                     col_rename    = col_rename,
                     ...           = ...)
}

#' @export
tq_transmute_xy_.grouped_df <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, ...) {

    group_names <- dplyr::group_vars(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = purrr::map(
            .x            = data,
            .f            = tq_transmute_xy_,
            x             = x,
            y             = y,
            mutate_fun    = mutate_fun,
            col_rename    = col_rename,
            ...)
            ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(cols = nested.col) %>%
        dplyr::group_by_at(.vars = group_names)
}

# Function options -------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_transmute_fun_options <- function() {

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

    # PerformanceAnalytics apply.rolling, Return...
    pkg_PA <- "package:PerformanceAnalytics"
    pkg_regex_PA <- "Return.annualized|Return.excess|Return.Geltner|Return.cumulative|Return.clean|zerofill"
    funs_PA <- ls(pkg_PA)[stringr::str_detect(ls(pkg_PA), pkg_regex_PA)]



    fun_options <- list(zoo                  = funs_zoo,
                        xts                  = funs_xts,
                        quantmod             = funs_quantmod,
                        TTR                  = funs_ttr,
                        PerformanceAnalytics = funs_PA)

    fun_options
}

# Checks ----------------------------------------------------------------------------------------------------

check_transmute_fun_options <- function(fun) {
    fun_options <- tq_transmute_fun_options() %>%
        unlist()
    if (!(fun %in% fun_options)) {
        stop(paste0("fun = ", fun, " not a valid option."))
    }
}

# Can be removed in version 0.6 with removal of ohlc_fun argument
check_ohlc_fun_options <- function(fun) {
    x_options <- c("Op", "Hi", "Lo", "Cl", "Vo", "Ad",
                   "HLC", "OHLC", "OHLCV")
    if (!(fun %in% x_options)) {
        stop(paste0("ohlc_fun = ", fun, " not a valid name."))
    }
}

check_x_y_valid <- function(data, x, y) {
    if (!(x %in% names(data))) stop(paste0("x = ", x, " not a valid name."))
    if (y != "NULL" && !is.null(y)) {
        if (!(y %in% names(data))) stop(paste0("y = ", y, " not a valid name."))
    }
}

# Utility ---------------------------------------------------------------------------------------------------

coerce_to_tibble <- function(data, date_col_name, time_zone, col_rename) {

    # Coerce to tibble
    ret <- data %>%
        timetk::tk_tbl(preserve_index = TRUE, rename_index = date_col_name, silent = TRUE)

    # # Convert to date
    # ret <- convert_date_cols(ret, time_zone)

    # # Rename row.names
    # names(ret)[[1]] <- date_col_name

    # Rename columns
    if (!is.null(col_rename)) {
        if (length(col_rename) == length(names(ret)) - 1) {
            # Are any col_rename names repeated? Can't have duplicates!
            if(any(purrr::map_lgl(seq_along(col_rename), ~any(col_rename[-.x] == col_rename[.x])))) {
                stop("Could not rename columns. Do you have duplicate names in `col_rename`?", call. = FALSE)
            } else {
                names(ret)[2:length(names(ret))] <- col_rename
            }
        } else {
            warning("Could not rename columns. The function name will be used. \n  Is the length of `col_rename` the same as the number of columns returned from the `mutate_fun`?")
        }
    }

    ret
}

detect_period_fun <- function(fun) {
    is_period_fun <- FALSE
    to_period_funs <- tq_transmute_fun_options() %>%
        unlist() %>%
        stringr::str_subset("^to")
    if (fun %in% to_period_funs) is_period_fun <- TRUE
    is_period_fun
}

# For use with ohlc_fun deprecation. Can remove in version 0.6.
map_ohlc_to_select <- function(x) {

    # quantmod extractors
    ohlc_fun_options <- list(Op    = "open",          OpLo  = "c(open, low)",
                             OpHi  = "c(open, high)", OpCl  = "c(open, close)",
                             Hi    = "high",          HiCl  = "c(high, close)",
                             Lo    = "low",           LoCl  = "c(low, close)",
                             LoHi  = "c(low, high)",  Cl    = "close",
                             Vo    = "volume",        Ad    = "adjusted",
                             OHLC  = "open:close",    OHLCV = "open:volume")

    # Find position
    location <- which(names(ohlc_fun_options) == x)

    # Stop if invalid quantmod extractor
    if(length(location) == 0) stop("OHLCV extractor is not valid. Cannot coerce to `select` equivalent.", call. = FALSE)

    # Select equivalent
    select_string <- ohlc_fun_options[location][[1]]

    select_string
}
