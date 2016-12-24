library(tidyquant)
context("Testing as_tibble")

# Setup
test_xts <- suppressWarnings({
    quantmod::getSymbols("AAPL", auto.assign = FALSE)
})

test_tib <- test_xts %>%
    as_tibble(preserve_row_names = TRUE)

test_err <- 5

test_err2 <- matrix(rep(1, 25), nrow = 5)

# Tests
test_that("Test returns tibble with correct rows and columns.", {
    # Test class
    expect_is(test_xts, "xts")
    expect_is(test_tib, "tbl")
    # Rows
    expect_equal(nrow(test_xts), nrow(test_tib))
    # Columns
    expect_equal(ncol(test_xts) + 1, ncol(test_tib))
})

test_that("Test handle errors gracefully", {
    # Returns warning
    expect_warning(as_tibble(test_err, preserve_row_names = TRUE))
    expect_warning(as_tibble(test_err2, preserve_row_names = TRUE))
})
