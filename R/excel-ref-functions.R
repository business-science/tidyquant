#' Excel Reference Functions
#'
#' @description
#' Excel reference functions are used to efficiently lookup values from a data source.
#' The most popular lookup function is "VLOOKUP", which has been implemented in R.
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names are similar to Excel function names
#' - Functionality replicates Excel
#'
#'
#' @param .lookup_values One or more lookup values.
#' @param .data A `data.frame` or `tibble` that contains values to evaluate and return
#' @param .lookup_column The column in `.data` containing exact matching values of the `.lookup_values`
#' @param .return_column The column in `.data` containing the values to return if a match is found
#'
#'
#' @return Returns a vector the length of the input lookup values
#'
#' @details
#' __[VLOOKUP()] Details__
#' - Performs exact matching only. Fuzzy matching is not implemented.
#' - Can only return values from one column only. Use `dplyr::left_join()` to perform table joining.
#'
#' @examples
#' library(tidyquant)
#' library(tidyverse)
#'
#' lookup_table <- tibble(
#'     stock   = c("FB", "AMZN", "NFLX", "GOOG"),
#'     company = c("Facebook", "Amazon", "Netflix", "Google")
#' )
#'
#' # --- Basic Usage ---
#'
#' VLOOKUP("NFLX",
#'         .data = lookup_table,
#'         .lookup_column = stock,
#'         .return_column = company)
#'
#' # --- Usage with tidyverse ---
#'
#' # Add company names to the stock data
#' FANG %>%
#'     mutate(company = VLOOKUP(symbol, lookup_table, stock, company))
#'
#' @name excel_ref_functions


#' @rdname excel_ref_functions
#' @export
VLOOKUP <- function(.lookup_values, .data, .lookup_column, .return_column) {

    key_expr <- rlang::enquo(.lookup_column)
    var_expr <- rlang::enquo(.return_column)

    tibble(x = .lookup_values) %>%
        dplyr::left_join(
            y  = .data,
            by = c(x = rlang::quo_name(key_expr)),
        ) %>%
        dplyr::pull(!! var_expr)
}

