library(tidyquant)
context("Testing as_xts")

# Setup
test_tib <- tq_get("AAPL", get = "stock.prices")

test_xts <- as_xts(test_tib, date_col = date)

x <- "date"
test_xts_ <- as_xts_(test_tib, date_col = x)

test_err <- tibble::tibble(
    date.char = c("2016-06-01", "2016-07-01", "2016-08-01"),
    val       = c(1000, 2000, 3000),
    cat       = c("land", "ocean", "land")
)


# Tests
test_that("Test identical test_xts and test_xts_.", {
    expect_identical(test_xts, test_xts_)
})

test_that("Test test_xts class.", {
    expect_is(test_xts, "xts")
})

test_that("Test returns tibble with correct rows and columns.", {
    # Rows
    expect_equal(nrow(test_tib), nrow(test_xts))
    # Columns
    expect_equal(ncol(test_xts), ncol(test_tib) - 1)
})

# Graceful error handling was removed from as_xts
# test_that("Test handle errors gracefully", {
#     # Returns warning
#     expect_warning(as_xts(test_err, date_col = date.char))
#     # Returns NA
#     expect_equal(suppressWarnings({
#         as_xts(test_err, date_col = date.char)
#     }), NA)
# })
