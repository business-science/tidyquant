#' Excel Pivot Table
#'
#' @description
#' __The Pivot Table__ is one of Excel's most powerful features, and now it's available in `R`!
#' A pivot table is a table of statistics that summarizes the data of a more extensive table
#' (such as from a database, spreadsheet, or business intelligence program).
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names are similar to Excel function names
#' - Functionality replicates Excel
#'
#'
#' @param .data A `data.frame` or `tibble` that contains data to summarize with a pivot table
#' @param .rows Enter one or more groups to assess as expressions (e.g. `~ MONTH(date_column)`)
#' @param .columns Enter one or more groups to assess expressions (e.g. `~ YEAR(date_column)`)
#' @param .values Numeric only. Enter one or more summarization expression(s) (e.g. `~ SUM(value_column)`)
#' @param .filters This argument is not yet in use
#' @param .sort This argument is not yet in use
#' @param fill_na A value to replace missing values with. Default is `NA`
#'
#'
#' @return Returns a tibble that has been pivoted to summarize information by column and row groupings
#'
#' @details
#'
#' This summary might include sums, averages, or other statistics, which the pivot table groups together in a meaningful way.
#'
#' The key parameters are:
#' - `.rows` - These are groups that will appear as row-wise headings for the summarization, You can modify these groups by applying collapsing functions (e.g. (`YEAR()`).
#' - `.columns` - These are groups that will appear as column headings for the summarization. You can modify these groups by applying collapsing functions (e.g. (`YEAR()`).
#' - `.values` - These are numeric data that are summarized using a summary function
#' (e.g. `SUM()`, `AVERAGE()`, `COUNT()`, `FIRST()`, `LAST()`, `SUM_IFS()`, `AVERAGE_IFS()`, `COUNT_IFS()`)
#'
#' __R implementation details.__
#' - The `pivot_table()` function is powered by the `tidyverse`, an ecosystem of packages designed to manipulate data.
#' - All of the key parameters can be expressed using a functional form:
#'     - Rows and Column Groupings can be collapsed. Example: `.columns = ~ YEAR(order_date)`
#'     - Values can be summarized provided a single value is returned. Example: `.values = ~ SUM_IFS(order_volume >= quantile(order_volume, probs = 0.75))`
#'     - Summarizations and Row/Column Groupings can be stacked (combined) with `c()`. Example: `.rows = c(~ YEAR(order_date), company)`
#'     - Bare columns (e.g. `company`) don not need to be prefixed with the `~`.
#'     - __All grouping and summarizing functions MUST BE prefixed with `~`__. Example: `.rows = ~ YEAR(order_date)`
#'
#' @examples
#' library(tidyquant)
#' library(tidyverse)
#'
#' # PIVOT TABLE ----
#' # Calculate returns by year/quarter
#' FANG %>%
#'     pivot_table(
#'         .rows       = c(symbol, ~ QUARTER(date)),
#'         .columns    = ~ YEAR(date),
#'         .values     = ~ PCT_CHANGE_FIRSTLAST(adjusted)
#'     )
#'
#' @name excel_pivot_table



#' @rdname excel_pivot_table
#' @export
pivot_table <- function(.data, .rows, .columns, .values,
                        .filters = NULL, .sort = NULL, fill_na = NA) {

    temp_val_quo <- rlang::enquo(.values)
    temp_row_quo <- rlang::enquo(.rows)
    temp_col_quo <- rlang::enquo(.columns)

    # --- COLLAPSE ROW FUNCTIONS ---
    parsed_rows_text_tbl <- temp_row_quo %>%
        deparse() %>%
        stringr::str_trim() %>%
        paste0(collapse = " ") %>%
        stringr::str_remove("^~") %>%
        stringr::str_remove_all("\"") %>%
        stringr::str_remove_all("\\\\n") %>%
        parse_multi_input()
    mutated_rows_tbl     <- apply_collapsing_mutation(.data, parsed_rows_text_tbl)

    # mutated_rows_tbl

    # --- COLLAPSE COL FUNCTIONS ---
    parsed_cols_text_tbl <- temp_col_quo %>%
        deparse() %>%
        stringr::str_trim() %>%
        paste0(collapse = " ") %>%
        stringr::str_remove("^~") %>%
        stringr::str_remove_all("\"") %>%
        stringr::str_remove_all("\\\\n") %>%
        parse_multi_input()
    mutated_cols_tbl     <- apply_collapsing_mutation(mutated_rows_tbl, parsed_cols_text_tbl)

    # mutated_cols_tbl

    # --- APPLY SUMMARIZATION ---

    row_exprs <- parsed_rows_text_tbl %>% dplyr::pull(var_text) %>% stringr::str_remove("~") %>% rlang::syms()
    col_exprs <- parsed_cols_text_tbl %>% dplyr::pull(var_text) %>% stringr::str_remove("~") %>% rlang::syms()

    parsed_vals_text_tbl <- temp_val_quo %>%
        deparse() %>%
        stringr::str_trim() %>%
        paste0(collapse = " ") %>%
        stringr::str_remove("^~") %>%
        stringr::str_remove_all("\"") %>%
        stringr::str_remove_all("\\\\n") %>%
        parse_multi_input()

    summarized_tbl <- mutated_cols_tbl %>%
        dplyr::group_by_at(.vars = dplyr::vars(!!! row_exprs, !!! col_exprs)) %>%
        apply_collapsing_summarise(parsed_vals_text_tbl) %>%
        dplyr::ungroup()

    # summarized_tbl

    # --- APPLY PIVOT ---

    .col_names <- summarized_tbl %>% dplyr::select(!!! col_exprs) %>% names()
    .row_names <- summarized_tbl %>% dplyr::select(!!! row_exprs) %>% names()
    .val_names <- setdiff(names(summarized_tbl), unique(c(.col_names, .row_names)))

    # Check if pivot is required
    if (length(.col_names) == 0) return(summarized_tbl)

    pivoted_tbl <- summarized_tbl %>%
        tidyr::pivot_wider(names_from = tidyselect::all_of(.col_names), values_from = tidyselect::all_of(.val_names))

    pivoted_tbl[is.na(pivoted_tbl)] <- fill_na

    pivoted_tbl <- pivoted_tbl %>%
        purrr::set_names(nm = names(pivoted_tbl) %>% stringr::str_trim())

    return(pivoted_tbl)

}


# Utilities ----

apply_collapsing_summarise <- function(data, parsed_text_tbl) {

    # Extract functions & convert to symbols
    function_text  <- extract_function_text(parsed_text_tbl)
    function_exprs <- rlang::syms(function_text)

    # Apply collapsing summarizations programmatically
    summarized_tibble_list <- list()
    for (i in seq_along(function_exprs)) {
        summarized_tbl <- data %>%
            dplyr::summarize(!!function_exprs[[i]] := !! rlang::parse_quo(rlang::quo_name(function_exprs[[i]]),
                                                                   env = rlang::caller_env()))

        summarized_tibble_list <- append(summarized_tibble_list, list(summarized_tbl))
    }

    suppressMessages({
        ret_tbl <- purrr::reduce(summarized_tibble_list, dplyr::left_join)
    })

    return(ret_tbl)
}

apply_collapsing_mutation <- function(data, parsed_text_tbl) {

    # Extract functions & convert to symbols
    function_text  <- extract_function_text(parsed_text_tbl)
    function_exprs <- rlang::syms(function_text)

    # Apply collapsing mutations programmatically
    mutated_tbl <- data
    for (i in seq_along(function_exprs)) {
        mutated_tbl <- mutated_tbl %>%
            dplyr::mutate(!!function_exprs[[i]] := !! rlang::parse_quo(rlang::quo_name(function_exprs[[i]]),
                                                                       env = rlang::caller_env()))
    }

    return(mutated_tbl)
}

parse_multi_input <- function(text) {

    # Remove c()
    parse_input_tbl <- text %>%
        tibble::tibble(var_text = .) %>%
        dplyr::mutate(var_text = ifelse(
            stringr::str_detect(var_text, "^c\\("),
            stringr::str_remove(var_text, pattern = "^c\\(") %>% stringr::str_remove("\\)$"),
            var_text))

    # Locate Parenthesis and Commas
    paren_assessment_list <- parse_input_tbl %>%
        dplyr::pull(var_text) %>%
        stringr::str_locate_all("\\(") %>%
        purrr::set_names("paren_begin_loc") %>%
        append(
            parse_input_tbl$var_text %>%
                stringr::str_locate_all("\\)") %>%
                purrr::set_names("paren_end_loc")
        ) %>%
        append(
            parse_input_tbl$var_text %>%
                stringr::str_locate_all(",") %>%
                purrr::set_names("comma_loc")
        ) %>%
        purrr::map(~ .[,1])

    # Return if no commas to deal with
    if (length(paren_assessment_list$comma_loc) == 0) {
        ret <- parse_input_tbl %>%
            dplyr::mutate(var_text    = stringr::str_trim(var_text)) %>%
            dplyr::mutate(is_function = stringr::str_detect(var_text, pattern = "~"))

        return(ret)
    }

    # If commas detected, get locations and check versus parenthesis
    paren_loc_tbl <-  tibble::tibble(
        paren_begin_loc = paren_assessment_list$paren_begin_loc,
        paren_end_loc   = paren_assessment_list$paren_end_loc
    )

    comma_loc_tbl <- tibble::tibble(comma_loc = paren_assessment_list$comma_loc)

    comma_test <- function(.x) {
        p_count <- paren_loc_tbl %>%
            tidyr::gather() %>%
            dplyr::group_by(key) %>%
            dplyr::summarise(count_parenthesis = COUNT_IFS(value, value > .x)) %>%
            dplyr::pull(count_parenthesis)


        ifelse(p_count[1] == p_count[2], "PASS", "FAIL")
    }

    passing_commas_tbl <- comma_loc_tbl %>%
        dplyr::mutate(comma_test = purrr::map_chr(comma_loc, comma_test)) %>%
        dplyr::filter(comma_test == "PASS")

    # Return if no commas pass, requiring separation
    if (nrow(passing_commas_tbl) == 0) {
        ret <- parse_input_tbl %>%
            dplyr::mutate(var_text    = stringr::str_trim(var_text)) %>%
            dplyr::mutate(is_function = stringr::str_detect(var_text, pattern = "~"))

        return(ret)
    }

    # Apply string replacement, used for splitting
    string_replacement <- parse_input_tbl$var_text
    for (r in 1:nrow(passing_commas_tbl)) {
        pos <- passing_commas_tbl$comma_loc[r]
        stringr::str_sub(string_replacement, start = pos, end = pos) <- "@"
    }

    ret <- tibble::tibble(var_text = string_replacement) %>%
        tidyr::separate_rows(var_text, sep = "@") %>%
        dplyr::mutate(var_text    = stringr::str_trim(var_text)) %>%
        dplyr::mutate(is_function = stringr::str_detect(var_text, pattern = "~"))

    return(ret)
}


extract_function_text <- function(data) {
    data %>%
        dplyr::filter(is_function) %>%
        dplyr::pull(var_text) %>%
        stringr::str_replace("~", "")
}
