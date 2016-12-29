library(tidyquant)

#### Setup
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")
FB <-  tq_get("FB", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")

AAPL_chain <- AAPL %>%
    tq_mutate(Cl, MACD) %>%
    tq_mutate(HLC, BBands) %>%
    tq_mutate(OHLC, OpCl)

#### Tests

test_that("Test returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 104)
    # Columns
    expect_equal(ncol(test1), 7)
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get)), NA)
})
