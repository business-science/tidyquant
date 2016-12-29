library(tidyquant)

#### Setup
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")
FB <-  tq_get("FB", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")

AAPL_transform <- AAPL %>%
    tq_transform(x_fun = Cl, transform_fun = to.period, period = "months", OHLC = FALSE)

AAPL_transform_xy <- AAPL %>%
    tq_transform_xy(.x = close, .y = volume, transform_fun = EVWMA)

AAPL_transform_xy_ <- AAPL %>%
    tq_transform_xy_(.x = "close", .y = "volume", transform_fun = "EVWMA")

FB_transform_no_date_col <- FB %>%
    tq_transform(x_fun = OHLCV, transform_fun = to.monthly)

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
