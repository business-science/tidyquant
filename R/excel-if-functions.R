#' Excel "If" Functions
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
#' * Count: [COUNT_IFS()], [COUNT_UNIQUE()]
#' * Range: [MIN_IFS()], [MAX_IFS()]
#'
#' __Create your own "_IF" function__
#' This is a function factory that generates "_IFS"
#'
#' @param x A vector. Most functions are designed for numeric data.
#' Some functions like [COUNT_IFS()] handle multiple data types.
#' @param ... Add cases to evaluate. See Details.
#' @param .f A function to convert to an "IFS" function
#' @param .NAME The name for the new "IFS" function you create
#' @param .validate_numericish Validate whether the input, `x`, should be numeric or logical.
#' Some summary functions like [COUNT_IFS()] can more than just numeric and logical data types.
#'
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
#' __Creating New "IFS" Functions:__
#' Users can create new "IFS" functions using the [CREATE_IFS()] function factory.
#'
#'
#' @examples
#' # Libraries
#' library(tidyquant)
#' library(tidyverse)
#'
#' # TODO
#'
#' @name excel_if_functions

# PRIMARY IFS ----

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
CREATE_IFS <- function(x, .f, .NAME = "_IFS", .validate_numericish = FALSE) {
    function(x, ...) {
        if (.validate_numericish) validate_numericish(x, .NAME)
        meets_criteria <- eval_cases(x, ...)
        .f(x[meets_criteria])
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

