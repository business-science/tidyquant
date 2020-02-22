
VLOOKUP <- function(.lookup, .data, .data_match, .data_return) {

    key_expr <- rlang::enquo(.data_match)
    var_expr <- rlang::enquo(.data_return)

    tibble(x = .lookup) %>%
        dplyr::left_join(
            y  = .data,
            by = c(x = rlang::quo_name(key_expr)),
        ) %>%
        dplyr::pull(!! var_expr)
}

