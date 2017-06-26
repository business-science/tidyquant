library(tidyquant)
library(timekit)
context("Testing as_tibble")

AAPL_xts <- suppressWarnings({
    quantmod::getSymbols("AAPL", auto.assign = FALSE)
})

test_that("as_tibble yields a warning.", {
    # Deprecation warning
    expect_warning(
        AAPL_xts %>%
            as_tibble(preserve_row_names = TRUE)
    )
})

test_that("as_tibble produces tibble in correct format.", {

    test <- suppressWarnings(
        AAPL_xts %>%
            as_tibble(preserve_row_names = TRUE)
    )

    # Check tbl
    expect_is(test, "tbl")

    # Check date column is date
    expect_equal(
        test %>%
            tk_get_timeseries_variables() %>%
            length(),
        1)
})

test_that("preserve_row_names = FALSE.", {

    test <- suppressWarnings(
        AAPL_xts %>%
            as_tibble(preserve_row_names = FALSE)
    )

    # Check tbl
    expect_is(test, "tbl")

    # Check no date column
    expect_equal(
        test %>%
            tk_get_timeseries_variables() %>%
            length(),
        0)
})
