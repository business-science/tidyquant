library(tidyquant)
context("Testing as_xts")

AAPL_tbl <- tq_get("AAPL")

test_that("as_xts yields a warning.", {
    expect_warning(
        AAPL_tbl %>%
            as_xts()
    )
})

test_that("as_xts produces tibble in correct format.", {
    test <- suppressWarnings(
        AAPL_tbl %>%
            as_xts(date_col = date)
    )
    expect_is(test, "xts")
})

test_that("as_xts_ produces tibble in correct format.", {
    test <- suppressWarnings(
        AAPL_tbl %>%
            as_xts_(date_col = "date")
    )
    expect_is(test, "xts")
})
