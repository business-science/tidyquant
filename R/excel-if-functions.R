#' Excel Summarising "If" Functions
#'
#' @description
#' __"IFS" functions__ are filtering versions of their summarization counterparts.
#' Simply add "cases" that filter if a condition is true.
#' Multiple cases are evaluated as "AND" filtering operations.
#' A single case with `|` ("OR") bars can be created to accomplish an "OR".
#' See details below.
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names are similar to Excel function names
#' - By default, missing values are ignored (same as in Excel)
#'
#' @section Useful functions:
#'
#' __Summary Functions__ - Return a single value from a vector
#' * Sum: [SUM_IFS()]
#' * Center: [AVERAGE_IFS()], [MEDIAN_IFS()]
#' * Count: [COUNT_IFS()]
#' * Range: [MIN_IFS()], [MAX_IFS()]
#'
#' __Create your own summary "_IF" function__
#' This is a function factory that generates summary "_IFS" functions.
#'
#' @param x A vector. Most functions are designed for numeric data.
#' Some functions like [COUNT_IFS()] handle multiple data types.
#' @param ... Add cases to evaluate. See Details.
#' @param .f A function to convert to an "IFS" function.
#' Use `...` in this case to provide parameters to the `.f` like `na.rm = TRUE`.
#'
#' @return
#' - __Summary functions__ return a single value
#'
#' @details
#' __"AND" Filtering:__
#' Multiple cases are evaluated as "AND" filtering operations.
#'
#' __"OR" Filtering:__
#' Compound single cases with `|` ("OR") bars can be created to accomplish an "OR".
#' Simply use a statement like `x > 10 | x < -10` to perform an "OR" if-statement.
#'
#' __Creating New "Summarizing IFS" Functions:__
#' Users can create new "IFS" functions using the [CREATE_IFS()] function factory.
#' The only requirement is that the output of your function (`.f`) must be a single
#' value (scalar). See examples below.
#'
#'
#' @examples
#' library(tidyverse)
#' library(tidyquant)
#' library(timetk)
#' library(stringr)
#' library(lubridate)
#'
#' # --- Basic Usage ---
#'
#' SUM_IFS(x = 1:10, x > 5)
#'
#' COUNT_IFS(x = letters, str_detect(x, "a|b|c"))
#'
#' SUM_IFS(-10:10, x > 8 | x < -5)
#'
#' # Create your own IFS function (Mind blowingly simple)!
#' Q75_IFS <- CREATE_IFS(.f = quantile, probs = 0.75, na.rm = TRUE)
#' Q75_IFS(1:10, x > 5)
#'
#' # --- Usage with tidyverse ---
#'
#' # Using multiple cases IFS cases to count the frequency of days with
#' # high trade volume in a given year
#' FANG %>%
#'     group_by(symbol) %>%
#'     summarise(
#'         high_volume_in_2015 = COUNT_IFS(volume,
#'                                         year(date) == 2015,
#'                                         volume > quantile(volume, 0.75))
#'     )
#'
#' # Count negative returns by month
#' FANG %>%
#'     mutate(symbol = as_factor(symbol)) %>%
#'     group_by(symbol) %>%
#'
#'     # Collapse from daily to FIRST value by month
#'     summarise_by_time(
#'         .date_var  = date,
#'         .by        = "month",
#'         adjusted   = FIRST(adjusted)
#'     ) %>%
#'
#'     # Calculate monthly returns
#'     group_by(symbol) %>%
#'     mutate(
#'         returns = PCT_CHANGE(adjusted, fill_na = 0)
#'     ) %>%
#'
#'     # Find returns less than zero and count the frequency
#'     summarise(
#'         negative_monthly_returns = COUNT_IFS(returns, returns < 0)
#'     )
#'
#' @name excel_if_functions

# MUTATING IFS  (NOT IMPLEMENTED) ----
#
# #' @rdname excel_if_functions
# #' @export
# IF <- function(logical_test, value_if_true, value_if_false) {
#     ifelse(logical_test, value_if_true, value_if_false)
# }
#
# #' @rdname excel_if_functions
# #' @export
# IFS <- function(..., .not_meets_criteria = FALSE) {
#     ifelse(logical_test, value_if_true, value_if_false)
# }
# IF <- function(x, ...) {
#     meets_criteria <- eval_cases(x, ...)
#     meets_criteria[is.na(meets_criteria)] <- FALSE
#     meets_criteria
# }
#
# IF <- IFS

# SUMMARIZATION IFS ----

#' @rdname excel_if_functions
#' @export
SUM_IFS <- function(x, ...) {
    validate_numericish(x, "SUM_IFS")
    meets_criteria <- eval_cases(x, ...)
    SUM(x[meets_criteria])
}

#' @rdname excel_if_functions
#' @export
COUNT_IFS <- function(x, ...) {
    meets_criteria <- eval_cases(x, ...)
    COUNT(x[meets_criteria])
}

#' @rdname excel_if_functions
#' @export
AVERAGE_IFS <- function(x, ...) {
    validate_numericish(x, "AVERAGE_IFS")
    meets_criteria <- eval_cases(x, ...)
    AVERAGE(x[meets_criteria])
}

#' @rdname excel_if_functions
#' @export
MEDIAN_IFS <- function(x, ...) {
    validate_numericish(x, "MEDIAN_IFS")
    meets_criteria <- eval_cases(x, ...)
    MEDIAN(x[meets_criteria])
}

#' @rdname excel_if_functions
#' @export
MIN_IFS <- function(x, ...) {
    validate_numericish(x, "MIN_IFS")
    meets_criteria <- eval_cases(x, ...)
    MIN(x[meets_criteria])
}

#' @rdname excel_if_functions
#' @export
MAX_IFS <- function(x, ...) {
    validate_numericish(x, "MAX_IFS")
    meets_criteria <- eval_cases(x, ...)
    MAX(x[meets_criteria])
}


# IFS GENERATOR (Function Factory to create IFS functions) ----

#' @rdname excel_if_functions
#' @export
CREATE_IFS <- function(.f, ...) {

    .dots <- list(...)

    function(x, ...) {
        # if (.validate_numericish) validate_numericish(x, .NAME)
        meets_criteria <- eval_cases(x, ...)
        do.call(.f, append(list(x[meets_criteria]), .dots))
    }
}


# UTILITIES ----
bind_expr <- function(expr1, expr2) {
    rlang::expr(!!expr1 & !!expr2)
}

eval_cases <- function(x, ...) {
    dots_expr <- rlang::enquos(...)

    flatten_exprs <- dots_expr %>%
        purrr::reduce(bind_expr)

    cases_expr <- rlang::expr(!! flatten_exprs ~ TRUE)

    meets_criteria <- tibble::tibble(x = x) %>%
        dplyr::mutate(meets_criteria = dplyr::case_when(!! cases_expr)) %>%
        dplyr::pull(meets_criteria)

    return(meets_criteria)
}

