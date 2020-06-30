#' Excel Statistical Mutation Functions
#'
#' @description
#' 15+ common statistical functions familiar to users of Excel (e.g. [ABS()], [SQRT()])
#' that __modify / transform__ a series of values
#' (i.e. a vector of the same length of the input is returned).
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names in most cases match Excel function names
#' - Functionality replicates Excel
#' - By default, missing values are ignored (same as in Excel)
#'
#' @section Useful functions:
#'
#' __Mutation Functions__ - Transforms a vector
#' * Transformation: [ABS()], [SQRT()], [LOG()], [EXP()]
#' * Lags & Change (Offsetting Functions): [CHANGE()], [PCT_CHANGE()], [LAG()], [LEAD()]
#' * Cumulative Totals: [CUMULATIVE_SUM()], [CUMULATIVE_PRODUCT()]
#'
#' @param x A vector. Most functions are designed for numeric data.
#' @param n Values to offset. Used in functions like [LAG()], [LEAD()], and [PCT_CHANGE()]
#' @param fill_na Fill missing (`NA`) values with a different value. Used in offsetting functions.
#'
#'
#' @return
#' - __Mutation functions__ return a mutated / transformed version of the vector
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
#' CUMULATIVE_SUM(1:10)
#'
#' PCT_CHANGE(c(21, 24, 22, 25), fill_na = 0)
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
#'     ) %>%
#'
#'     # Mutation - Calculate monthly returns and cumulative growth of $1 USD
#'     group_by(symbol) %>%
#'     mutate(
#'         returns = PCT_CHANGE(adjusted, fill_na = 0),
#'         growth  = CUMULATIVE_SUM(returns) + 1
#'     )
#'
#' @name excel_stat_mutation_functions


# MUTATION FUNCTIONS ----


#' @rdname excel_stat_mutation_functions
#' @export
ABS <- function(x) {
    validate_numericish(x, "ABS")
    abs(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
SQRT <- function(x) {
    validate_numericish(x, "SQRT")
    sqrt(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
LOG <- function(x) {
    validate_numericish(x, "LOG")
    log(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
EXP <- function(x) {
    validate_numericish(x, "EXP")
    exp(x)
}
#' @rdname excel_stat_mutation_functions
#' @export
RETURN <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "RETURN")
    l <- dplyr::lag(x, n = n)
    r <- (x - l) / l
    if (!is.na(fill_na)) r[is.na(r)] <- fill_na

    return(r)
}

#' @rdname excel_stat_mutation_functions
#' @export
PCT_CHANGE <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "PCT_CHANGE")
    return(RETURN(x, n, fill_na))
}

#' @rdname excel_stat_mutation_functions
#' @export
CHANGE <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "CHANGE")
    l <- dplyr::lag(x, n = n)
    r <- (x - l)
    if (!is.na(fill_na)) r[is.na(r)] <- fill_na

    return(r)
}

#' @rdname excel_stat_mutation_functions
#' @export
LAG <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "LAG")
    l <- dplyr::lag(x, n = n)
    if (!is.na(fill_na)) l[is.na(l)] <- fill_na

    return(l)
}

#' @rdname excel_stat_mutation_functions
#' @export
LEAD <- function(x, n = 1, fill_na = NA) {
    validate_numericish(x, "LAG")
    l <- dplyr::lead(x, n = n)
    if (!is.na(fill_na)) l[is.na(l)] <- fill_na

    return(l)
}

#' @rdname excel_stat_mutation_functions
#' @export
CUMULATIVE_SUM <- function(x) {
    validate_numericish(x, "CUMULATIVE_SUM")
    cumsum(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
CUMULATIVE_PRODUCT <- function(x) {
    validate_numericish(x, "CUMULATIVE_PRODUCT")
    cumprod(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
CUMULATIVE_MAX <- function(x) {
    validate_numericish(x, "CUMULATIVE_MAX")
    cummax(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
CUMULATIVE_MIN <- function(x) {
    validate_numericish(x, "CUMULATIVE_MIN")
    cummin(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
CUMULATIVE_MEAN <- function(x) {
    validate_numericish(x, "CUMULATIVE_MEAN")
    dplyr::cummean(x)
}

#' @rdname excel_stat_mutation_functions
#' @export
CUMULATIVE_MEDIAN <- function(x) {
    validate_numericish(x, "CUMULATIVE_MEDIAN")
    purrr::accumulate(x, .f = stats::median, na.rm = TRUE)
}





