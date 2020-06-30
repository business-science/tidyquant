#' Excel Statistical Summary Functions
#'
#' @description
#' 15+ common statistical functions familiar to users of Excel (e.g. [SUM()], [AVERAGE()]).
#' These functions return a __single value__ (i.e. a vector of length 1).
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
#'
#' @param x A vector. Most functions are designed for numeric data.
#' Some functions like [COUNT()] handle multiple data types.
#' @param y A vector. Used in functions requiring 2 inputs.
#' @param n A single value used in [NTH()] to select a specific element location to return.
#'
#'
#' @return
#' - __Summary functions__ return a single value
#'
#' @details
#' __Summary Functions__
#' - All functions remove missing values (`NA`). This is the same behavior as in Excel and most commonly what is desired.
#'
#' @examples
#' # Libraries
#' library(tidyquant)
#' library(timetk)
#' library(tidyverse)
#' library(forcats)
#'
#' # --- Basic Usage ----
#'
#' SUM(1:10)
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
#'     # Summarization - Collapse from daily to FIRST value by month
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by        = "month",
#'         adjusted   = FIRST(adjusted)
#'     )
#'
#' @name excel_stat_summary_functions

# SUMMARY FUNCTIONS ----

#' @rdname excel_stat_summary_functions
#' @export
SUM <- function(x) {
    validate_numericish(x, "SUM")
    sum(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
AVERAGE <- function(x) {
    validate_numericish(x, "AVERAGE")
    mean(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
MEDIAN <- function(x) {
    validate_numericish(x, "MEDIAN")
    stats::median(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
MIN <- function(x) {
    validate_numericish(x, "MIN")
    min(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
MAX <- function(x) {
    validate_numericish(x, "MAX")
    max(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
COUNT <- function(x) {
    # NO VALIDATION - CAN USE MULTIPLE DATA TYPES
    sum(!is.na(x))
}

#' @rdname excel_stat_summary_functions
#' @export
COUNT_UNIQUE <- function(x) {
    # NO VALIDATION - CAN USE MULTIPLE DATA TYPES
    sum(!is.na(unique(x)))
}

#' @rdname excel_stat_summary_functions
#' @export
STDEV <- function(x) {
    validate_numericish(x, "STDEV")
    stats::sd(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
VAR <- function(x) {
    validate_numericish(x, "VAR")
    stats::var(x, na.rm = TRUE)
}

#' @rdname excel_stat_summary_functions
#' @export
COR <- function(x, y) {
    validate_numericish(x, "COR")
    validate_numericish(y, "COR")
    stats::cor(x, y, use = "pairwise.complete.obs", method = "pearson")
}

#' @rdname excel_stat_summary_functions
#' @export
COV <- function(x, y) {
    validate_numericish(x, "COV")
    validate_numericish(y, "COV")
    stats::cov(x, y, use = "pairwise.complete.obs", method = "pearson")
}

#' @rdname excel_stat_summary_functions
#' @export
FIRST <- function(x) {
    dplyr::first(x)
}

#' @rdname excel_stat_summary_functions
#' @export
LAST <- function(x) {
    dplyr::last(x)
}

#' @rdname excel_stat_summary_functions
#' @export
NTH <- function(x, n = 1) {
    dplyr::nth(x, n)
}

#' @rdname excel_stat_summary_functions
#' @export
CHANGE_FIRSTLAST <- function(x) {
    LAST(x) - FIRST(x)
}

#' @rdname excel_stat_summary_functions
#' @export
PCT_CHANGE_FIRSTLAST <- function(x) {
    (LAST(x) - FIRST(x)) / FIRST(x)
}




# # ROWWISE FUNCTIONS ----
# # Should not need row-wise functions: https://github.com/tidyverse/dplyr/blob/master/R/context.R
#
# #' @rdname excel_stat_summary_functions
# #' @export
# ROWWISE_SUM <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_SUM")
#     rowSums(as.matrix(...), na.rm = TRUE)
# }
#
# #' @rdname excel_stat_summary_functions
# #' @export
# ROWWISE_AVERAGE <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_AVERAGE")
#     apply(as.data.frame(...), MARGIN = 1, FUN = mean, na.rm = TRUE)
# }
#
# #' @rdname excel_stat_summary_functions
# #' @export
# ROWWISE_MEDIAN <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_MEDIAN")
#     apply(as.data.frame(...), MARGIN = 1, FUN = stats::median, na.rm = TRUE)
# }
#
#
# #' @rdname excel_stat_summary_functions
# #' @export
# ROWWISE_PRODUCT <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_PRODUCT")
#     apply(as.data.frame(...), MARGIN = 1, FUN = prod, na.rm = TRUE)
# }
#
# #' @rdname excel_stat_summary_functions
# #' @export
# ROWWISE_MIN <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_MIN")
#     pmin(..., na.rm = TRUE)
# }
#
# #' @rdname excel_stat_summary_functions
# #' @export
# ROWWISE_MAX <- function(...) {
#     list(...) %>%
#         purrr::map(validate_numericish, function_label = "ROWWISE_MAX")
#     pmax(..., na.rm = TRUE)
# }



