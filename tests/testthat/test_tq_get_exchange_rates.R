library(tidyquant)

#### Setup
get <- "exchange.rates"
context(paste0("Testing tq_get(get = '", get, "')"))

test1 <- tq_get("EUR/USD", get = get,
                from = "2017-01-01", to = "2017-04-01")

#### Tests

test_that("Test returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 91)
    # Columns
    expect_equal(ncol(test1), 2)
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get)), NA)
})
