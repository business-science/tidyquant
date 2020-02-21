


SUMIFS <- function(x, ...) {

    dots_expr <- enquos(...)

    flatten_exprs <- dots_expr %>%
        reduce(bind_expr)

    cases_expr <- expr(!! flatten_exprs ~ TRUE)



    meets_criteria <- tibble(x = x) %>%
        mutate(meets_criteria = case_when(!! cases_expr)) %>%
        pull(meets_criteria)

    sum(x[meets_criteria], na.rm = TRUE)

    # cases_expr <- lapply(seq_along(dots_expr), function(i) {
    #     expr(!! (dots_expr[[i]]))
    # })

    # tibble(x = x) %>%
    #     mutate(meets_criteria = case_when(!! cases_expr))

    # cases_expr

}

SUMIFS(1:10, x > 5, x < 8)

bind_expr <- function(expr1, expr2) {
    expr(!! expr1 & !!expr2)
}
bind_expr(quo(a), quo(b))
