
# tq_transmute ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_transmute <- function(data, ohlc_fun = OHLCV, mutate_fun, col_rename = NULL, transform_fun, ...) {

    # Deprecate transform_fun in favor of mutate_fun
    if (!missing(transform_fun)) {
        warning("argument transform_fun is deprecated; please use mutate_fun instead.",
                call. = FALSE)
        delayedAssign(x = "mutate_fun", value = transform_fun)
    }

    # NSE
    tq_transmute_(data          = data,
                  ohlc_fun      = lazyeval::expr_text(ohlc_fun),
                  mutate_fun    = lazyeval::expr_text(mutate_fun),
                  col_rename    = col_rename,
                  ...           = ...)
}

#' @rdname tq_mutate
#' @export
tq_transmute_ <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_transmute_", data)
}

# tq_transmute method dispatch --------------------------------------------------------------------------------

#' @export
tq_transmute_.default <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_transmute_.tbl_df <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    # Check mutate_fun in xts, quantmod or TTR
    check_transmute_fun_options(mutate_fun)

    # Check for x: either x, HLC, or price arguments
    check_ohlc_fun_options(ohlc_fun)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Drop any non-numeric columns except for date
    date_col <- dplyr::select_(data, date_col_name)
    numeric_cols <- data %>%
        dplyr::select_if(is.numeric)
    data <- dplyr::bind_cols(date_col, numeric_cols)

    # Convert inputs to functions
    ohlc_fun <- paste0("quantmod::", ohlc_fun)
    fun_x <- eval(parse(text = ohlc_fun))
    fun_transmute <- eval(parse(text = mutate_fun))

    # Patch for to.period functions
    is_period_fun <- detect_period_fun(mutate_fun)

    # Apply functions
    if (is_period_fun) {
        # Add arg: OHLC = FALSE
        ret <- data %>%
            as_xts_(date_col = date_col_name) %>%
            fun_x() %>%
            fun_transmute(OHLC = FALSE, ...)

    } else {
        ret <- data %>%
            as_xts_(date_col = date_col_name) %>%
            fun_x() %>%
            fun_transmute(...)
    }

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
                                                  time_zone, col_rename)

    ret
}

#' @export
tq_transmute_.data.frame <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- as_tibble(data)

    # Call tq_transmute_ for a tibble
    tq_transmute_(data          = data,
                  ohlc_fun      = ohlc_fun,
                  mutate_fun    = mutate_fun,
                  col_rename    = col_rename,
                  ...           = ...)
}

#' @export
tq_transmute_.grouped_df <- function(data, ohlc_fun = "OHLCV", mutate_fun, col_rename = NULL, ...) {

    group_names <- dplyr::groups(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = data %>%
                          purrr::map(~ tq_transmute_(data          = .x,
                                                     ohlc_fun      = ohlc_fun,
                                                     mutate_fun    = mutate_fun,
                                                     col_rename    = col_rename,
                                                     ...           = ...))
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names)
}

# tq_transmute_xy ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_transmute_xy <- function(data, x, y = NULL, mutate_fun, col_rename = NULL, transform_fun, ...) {

    # Deprecate transform_fun in favor of mutate_fun
    if (!missing(transform_fun)) {
        warning("argument transform_fun is deprecated; please use mutate_fun instead.",
                call. = FALSE)
        delayedAssign(x = "mutate_fun", value = transform_fun)
    }

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
    date_col <- dplyr::select_(data, date_col_name)
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
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transmute(eval(parse(text = x)), OHLC = FALSE, ...)
        } else {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transmute(eval(parse(text = x)),
                              eval(parse(text = y)),
                              OHLC = FALSE,
                              ...)
        }
    } else {
        if (y == "NULL" || is.null(y)) {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
                # OHLCV() %$%
                fun_transmute(eval(parse(text = x)), ...)
        } else {
            ret <- data %>%
                as_xts_(date_col = date_col_name) %$%
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
    data <- as_tibble(data)

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

    group_names <- dplyr::groups(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = data %>%
                          purrr::map(~ tq_transmute_xy_(data          = .x,
                                                        x             = x,
                                                        y             = y,
                                                        mutate_fun    = mutate_fun,
                                                        col_rename    = col_rename,
                                                        ...           = ...))
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names)
}


# tq_transmute_data ------------------------------------------------------------------------------------------------

#' @rdname tq_mutate
#' @export
tq_transmute_data <- function(data, mutate_fun, col_rename = NULL, ...) {

    # NSE
    tq_transmute_data_(data          = data,
                       mutate_fun    = lazyeval::expr_text(mutate_fun),
                       col_rename    = col_rename,
                       ...           = ...)
}

#' @rdname tq_mutate
#' @export
tq_transmute_data_ <- function(data, mutate_fun, col_rename = NULL, ...) {
    UseMethod("tq_transmute_data_", data)
}

# tq_transmute_data method dispatch --------------------------------------------------------------------------------

#' @export
tq_transmute_data_.default <- function(data, mutate_fun, col_rename = NULL, ...) {

    # Error message
    stop("data must be a tibble or data.frame object")
}

#' @export
tq_transmute_data_.tbl_df <- function(data, mutate_fun, col_rename = NULL, ...) {

    # Check mutate_fun in xts, quantmod or TTR
    check_transmute_fun_options(mutate_fun)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Drop any non-numeric columns except for date
    date_col <- dplyr::select_(data, date_col_name)
    numeric_cols <- data %>%
        dplyr::select_if(is.numeric)
    data <- dplyr::bind_cols(date_col, numeric_cols)

    # Convert inputs to functions
    fun_transmute <- eval(parse(text = mutate_fun))

    # Patch for to.period functions
    is_period_fun <- detect_period_fun(mutate_fun)

    # Apply function
    ret <- data %>%
        as_xts(date_col = date) %>%
        fun_transmute(...)

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
                                                  time_zone, col_rename)

    ret
}

#' @export
tq_transmute_data_.data.frame <- function(data, mutate_fun, col_rename = NULL, ...) {

    # Convert data.frame to tibble
    data <- as_tibble(data)

    # Call tq_transmute_xy_ for a tibble
    tq_transmute_data_(data          = data,
                       mutate_fun    = mutate_fun,
                       col_rename    = col_rename,
                       ...           = ...)
}

#' @export
tq_transmute_data_.grouped_df <- function(data, mutate_fun, col_rename = NULL, ...) {

    group_names <- dplyr::groups(data)

    data %>%
        tidyr::nest() %>%
        dplyr::mutate(nested.col = data %>%
                          purrr::map(~ tq_transmute_data_(data          = .,
                                                          mutate_fun    = mutate_fun,
                                                          col_rename    = col_rename,
                                                          ...           = ...))
        ) %>%
        dplyr::select(-data) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names)
}

# tq_transform and tq_transform_xy for backwards compatability -----------------------------------------------

#' @rdname deprecated
#' @export
tq_transform <- function(data, ohlc_fun = OHLCV, transform_fun, col_rename = NULL, ...) {

    # Pass to tq_transmute_ but warn the user
    .Deprecated("tq_transmute",
                msg = "`tq_transform` is deprecated and will be removed in 0.5.0 \nPlease use `tq_transmute` instead.")

    # NSE
    tq_transmute_(data          = data,
                  ohlc_fun      = lazyeval::expr_text(ohlc_fun),
                  mutate_fun    = lazyeval::expr_text(transform_fun),
                  col_rename    = col_rename,
                  ...           = ...)
}

#' @rdname deprecated
#' @export
tq_transform_xy <- function(data, x, y = NULL, transform_fun, col_rename = NULL, ...) {

    # Pass to tq_transmute_xy but warn the user
    .Deprecated("tq_transmute_xy",
                msg = "`tq_transform_xy` is deprecated and will be removed in 0.5.0 \nPlease use `tq_transmute_xy` instead.")

    # NSE
    tq_transmute_xy_(data          = data,
                     x             = lazyeval::expr_text(x),
                     y             = lazyeval::expr_text(y),
                     mutate_fun    = lazyeval::expr_text(transform_fun),
                     col_rename    = col_rename,
                     ...           = ...)
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

    fun_options <- list(zoo = funs_zoo,
                        xts = funs_xts,
                        quantmod = funs_quantmod,
                        TTR = funs_ttr)

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
        as_tibble(preserve_row_names = TRUE) %>%
        dplyr::rename(date = row.names)

    # Convert to date
    ret <- convert_date_cols(ret, time_zone)

    # Rename row.names
    names(ret)[[1]] <- date_col_name

    # Rename columns
    if (!is.null(col_rename)) {
        if (length(col_rename) == length(names(ret)) - 1) {
            names(ret)[2:length(names(ret))] <- col_rename
        } else {
            warning("Could not rename columns")
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

