library(tidyquant)

#### Setup
get <- "stock.prices"
context(paste0("Testing tq_get(get = '", get, "')"))

test1 <- tq_get("AAPL", get = get,
                from = "2016-01-01", to = "2016-06-01")

test2 <- c("AAPL", "FB") %>%
    tq_get(get = get, from = "2016-01-01", to = "2016-06-01")

#### Tests

test_that("Test 1 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 103)
    # Columns
    expect_equal(ncol(test1), 7)
})

test_that("Test 2 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test2, "tbl")
    # Rows
    expect_equal(nrow(test2), 206)
    # Columns
    expect_equal(ncol(test2), 8)
})

test_that("Google finance is now defunct.", {
    expect_warning(tq_get("AAPL", get = "stock.prices.google"))
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get)), NA)
})
