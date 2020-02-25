#' Excel Statistical Functions
#'
#' @description
#' 30+ common statistical functions familiar to users of Excel (e.g. [SUM()], [AVERAGE()]).
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names in most cases match Excel function names
#' - Functionality replicates Excel
#' - By default, missing values are ignored (same as in Excel)
#'
#' @section Useful functions:
#'
#' __Summary Functions__ - Return a single value from a vector
#' * Sum: [SUM()]
#' * Center: [AVERAGE()], [MEDIAN()]
#' * Spread: [STDEV()], [VAR()]
#' * Range: [MIN()], [MAX()]
#' * Count: [COUNT()], [COUNT_UNIQUE()]
#' * Position: [FIRST()], [LAST()], [NTH()]
#' * Change (Summary): [CHANGE_FIRSTLAST()], [PCT_CHANGE_FIRSTLAST()]
#' * Correlation: [COR()], [COV()]
#'
#' __Mutation Functions__ - Transforms a vector
#' * Transformation: [ABS()], [SQRT()], [LOG()], [EXP()]
#' * Lags & Change (Offsetting Functions): [CHANGE()], [PCT_CHANGE()], [LAG()], [LEAD()]
#' * Cumulative Totals: [CUMULATIVE_SUM()], [CUMULATIVE_PRODUCT()]
#'
#' @param x A vector. Most functions are designed for numeric data.
#' Some functions like [COUNT()] handle multiple data types.
#' @param y A vector. Used in functions requiring 2 inputs.
#' @param n Values to offset. Used in functions like [LAG()], [LEAD()], and [PCT_CHANGE()]
#' @param fill_na Fill missing (`NA`) values with a different value. Used in offsetting functions.
#'
#'
#' @return
#' - __Summary functions__ return a single value
#' - __Mutation functions__ return a mutated version of the vector
#' - __Logical evaluations functions__ returns a boolean version of the vector
#'
#' @details
#' __Summary Functions__
#' - All functions remove missing values (`NA`). This is the same behavior as in Excel and most commonly what is desired.
#'
#' @examples
#' # Libraries
#' library(tidyquant)
#' library(tidyverse)
#' library(forcats)
#'
#' # --- Basic Usage ----
#'
#' SUM(1:10)
#'
#' CUMULATIVE_SUM(1:10)
#'
#' PCT_CHANGE(c(21, 24, 22, 25), fill_na = 0)
#'
#' PCT_CHANGE_FIRSTLAST(c(21, 24, 22, 25))
#'
#' # --- Usage with tidyverse ---
#'
#' # Go from daily to monthly periodicity,
#' # then calculate returns and growth of $1 USD
#' FANG %>%
#'     mutate(symbol = as_factor(symbol)) %>%
#'     group_by(symbol) %>%
#'
#'     # Collapse from daily to FIRST value by month
#'     summarise_by_time(
#'         .date_var  = date,
#'         .time_unit = "month",
#'         adjusted   = FIRST(adjusted)
#'     ) %>%
#'
#'     # Calculate monthly returns and cumulative growth of $1 USD
#'     group_by(symbol) %>%
#'     mutate(
#'         returns = PCT_CHANGE(adjusted, fill_na = 0),
#'         growth  = CUMULATIVE_SUM(returns) + 1
#'     )
#'
#' @name excel_stat_functions

# SUMMARY FUNCTIONS ----

#' @rdname excel_stat_functions
#' @export
SUM <- function(x) {
    validate_numericish(x, "SUM")
    sum(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
AVERAGE <- function(x) {
    validate_numericish(x, "AVERAGE")
    mean(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
MEDIAN <- function(x) {
    validate_numericish(x, "MEDIAN")
    stats::median(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
MIN <- function(x) {
    validate_numericish(x, "MIN")
    min(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
MAX <- function(x) {
    validate_numericish(x, "MAX")
    max(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
COUNT <- function(x) {
    # NO VALIDATION - CAN USE MULTIPLE DATA TYPES
    sum(!is.na(x))
}

#' @rdname excel_stat_functions
#' @export
COUNT_UNIQUE <- function(x) {
    # NO VALIDATION - CAN USE MULTIPLE DATA TYPES
    sum(!is.na(unique(x)))
}

#' @rdname excel_stat_functions
#' @export
STDEV <- function(x) {
    validate_numericish(x, "STDEV")
    stats::sd(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
VAR <- function(x) {
    validate_numericish(x, "VAR")
    stats::var(x, na.rm = TRUE)
}

#' @rdname excel_stat_functions
#' @export
COR <- function(x, y) {
    validate_numericish(x, "COR")
    validate_numericish(y, "COR")
    stats::cor(x, y, use = "pairwise.complete.obs", method = "pearson")
}

#' @rdname excel_stat_functions
#' @export
COV <- function(x, y) {
    validate_numericish(x, "COV")
    validate_numericish(y, "COV")
    stats::cov(x, y, use = "pairwise.complete.obs", method = "pearson")
}

#' @rdname excel_stat_functions
#' @export
FIRST <- function(x) {
    dplyr::first(x)
}

#' @rdname excel_stat_functions
#' @export
LAST <- function(x) {
    dplyr::last(x)
}

#' @rdname excel_stat_functions
#' @export
NTH <- function(x, n = 1) {
    dplyr::nth(x, n)
}

#' @rdname excel_stat_functions
#' @export
CHANGE_FIRSTLAST <- function(x) {
    LAST(x) - FIRST(x)
}

#' @rdname excel_stat_functions
#' @export
PCT_CHANGE_FIRSTLAST <- function(x) {
    (LAST(x) - FIRST(x)) / FIRST(x)
}

# MUTATION FUNCTIONS ----


#' @rdname excel_stat_functions
#' @export
ABS <- function(x) {
    validate_numericish(x, "ABS")
    abs(x)
}

#' @rdname excel_stat_functions
#' @export
SQRT <- function(x) {
    validate_numericish(x, "SQRT")
    sqrt(x)
}

#' @rdname excel_stat_functions
#' @export
LOG <- function(x) {
    validate_numericish(x, "LOG")
    log(x)
}

#' @rdname excel_stat_functions
#' @export
EXP <- function(x) {
    validate_numericish(x, "EXP")
    exp(x)
}
#' @rdname excel_stat_functions
#' @export
RETURN <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "RETURN")
    l <- dplyr::lag(x, n = n)
    r <- (x - l) / l
    if (!is.na(fill_na)) r[is.na(r)] <- fill_na

    return(r)
}

#' @rdname excel_stat_functions
#' @export
PCT_CHANGE <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "PCT_CHANGE")
    return(RETURN(x, n, fill_na))
}

#' @rdname excel_stat_functions
#' @export
CHANGE <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "CHANGE")
    l <- dplyr::lag(x, n = n)
    r <- (x - l)
    if (!is.na(fill_na)) r[is.na(r)] <- fill_na

    return(r)
}

#' @rdname excel_stat_functions
#' @export
LAG <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "LAG")
    l <- dplyr::lag(x, n = n)
    if (!is.na(fill_na)) l[is.na(l)] <- fill_na

    return(l)
}

#' @rdname excel_stat_functions
#' @export
LEAD <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "LAG")
    l <- dplyr::lead(x, n = n)
    if (!is.na(fill_na)) l[is.na(l)] <- fill_na

    return(l)
}

#' @rdname excel_stat_functions
#' @export
CUMULATIVE_SUM <- function(x) {
    validate_numericish(x, "CUMULATIVE_SUM")
    cumsum(x)
}

#' @rdname excel_stat_functions
#' @export
CUMULATIVE_PRODUCT <- function(x) {
    validate_numericish(x, "CUMULATIVE_PRODUCT")
    cumprod(x)
}

#' @rdname excel_stat_functions
#' @export
CUMULATIVE_MAX <- function(x) {
    validate_numericish(x, "CUMULATIVE_MAX")
    cummax(x)
}

#' @rdname excel_stat_functions
#' @export
CUMULATIVE_MIN <- function(x) {
    validate_numericish(x, "CUMULATIVE_MIN")
    cummin(x)
}

#' @rdname excel_stat_functions
#' @export
CUMULATIVE_MEAN <- function(x) {
    validate_numericish(x, "CUMULATIVE_MEAN")
    dplyr::cummean(x)
}

#' @rdname excel_stat_functions
#' @export
CUMULATIVE_MEDIAN <- function(x) {
    validate_numericish(x, "CUMULATIVE_MEDIAN")
    purrr::accumulate(x, .f = stats::median, na.rm = TRUE)
}




# # ROWWISE FUNCTIONS ----
# # Should not need row-wise functions: https://github.com/tidyverse/dplyr/blob/master/R/context.R
#
# #' @rdname excel_stat_functions
# #' @export
# ROWWISE_SUM <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_SUM")
#     rowSums(as.matrix(...), na.rm = TRUE)
# }
#
# #' @rdname excel_stat_functions
# #' @export
# ROWWISE_AVERAGE <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_AVERAGE")
#     apply(as.data.frame(...), MARGIN = 1, FUN = mean, na.rm = TRUE)
# }
#
# #' @rdname excel_stat_functions
# #' @export
# ROWWISE_MEDIAN <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_MEDIAN")
#     apply(as.data.frame(...), MARGIN = 1, FUN = stats::median, na.rm = TRUE)
# }
#
#
# #' @rdname excel_stat_functions
# #' @export
# ROWWISE_PRODUCT <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_PRODUCT")
#     apply(as.data.frame(...), MARGIN = 1, FUN = prod, na.rm = TRUE)
# }
#
# #' @rdname excel_stat_functions
# #' @export
# ROWWISE_MIN <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_MIN")
#     pmin(..., na.rm = TRUE)
# }
#
# #' @rdname excel_stat_functions
# #' @export
# ROWWISE_MAX <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_MAX")
#     pmax(..., na.rm = TRUE)
# }


# HELPER FUNCTIONS -----

validate_numericish <- function(x, function_label) {

    if ( !is_numericish(x) ) {
        stop(paste0(function_label, "(): input data type must be numeric or logical. Type supplied is: ", class(x)[[1]]), call. = FALSE)
    }
}

is_numericish <- function(x) {
    ret <- (is.numeric(x)) | (is.logical(x))
    return(ret)
}
