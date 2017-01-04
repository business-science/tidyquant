library(tidyquant)
context("Testing tq_transform")

#### Setup ----
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")

# Test 1: tq_transform to.period
test1 <- AAPL %>%
    tq_transform(ohlc_fun = Cl, transform_fun = to.period, period = "months")

# Test 2: tq_transform_xy test
test2 <- AAPL %>%
    tq_transform_xy(x = close, transform_fun = to.period, period = "months")


# Test 3: Test transform hourly data / Test transform timezone data
time_index <- seq(from = as.POSIXct("2012-05-15 07:00"),
                  to = as.POSIXct("2012-05-17 18:00"),
                  by = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))
tz <- "Zulu"
test3 <- tibble(time_index, value) %>%
    dplyr::mutate(time_index = lubridate::ymd_hms(time_index, tz = tz)) %>%
    tq_transform_xy(x = value, transform_fun = MACD)

# Test 4: transform to.monthly which returns character dates
test4 <- AAPL %>%
    tq_transform(ohlc_fun = OHLCV, transform_fun = to.monthly)


#### Tests ----

test_that("Test 1 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 60)
    # Columns
    expect_equal(ncol(test1), 2)
})

test_that("Test 2 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test2, "tbl")
    # Rows
    expect_equal(nrow(test2), 60)
    # Columns
    expect_equal(ncol(test2), 2)
})

test_that("Test 3 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test3, "tbl")
    # Rows
    expect_equal(nrow(test3), 60)
    # Columns
    expect_equal(ncol(test3), 3)
})

test_that("Test 3 returns correct timezone.", {
    expect_equal({test3$time_index %>% lubridate::tz()}, tz)
})


# Invalid data inputs
test_that("Test error on invalid data inputs.", {

    # Non-data.frame objects
    expect_error(
        a = seq(1:100) %>%
            tq_transform(ohlc_fun = OHLCV, transform_fun = to.monthly)
    )
    expect_error(
        a = seq(1:100) %>%
            tq_mutate_xy(x = a, mutate_fun = to.monthly)
    )

    # No date columns
    expect_error(
        tibble(a = seq(1:100)) %>%
            tq_mutate(ohlc_fun = OHLCV, mutate_fun = to.monthly),
        "No date or POSIXct column found in `data`."
    )
    expect_error(
        tibble(a = seq(1:100)) %>%
            tq_mutate_xy(x = a, mutate_fun = to.monthly),
        "No date or POSIXct column found in `data`."
    )
})

# Invalid ohlc_fun, x and y inputs
test_that("Test error on invalid ohlc_fun, x and y inputs.", {

    expect_error(
        {ohlc_fun <- "err"
        AAPL %>%
            tq_mutate_(ohlc_fun = ohlc_fun, mutate_fun = "to.monthly")}
    )
    expect_error(
        {x <-  "err"
        AAPL %>%
            tq_mutate_xy_(x = x, y = "close", mutate_fun = "Delt", k = 1)},
        paste0("x = ", x, " not a valid name.")
    )
    expect_error(
        {y <-  "err"
        AAPL %>%
            tq_mutate_xy_(x = "open", y = y, mutate_fun = "Delt", k = 1)},
        paste0("y = ", y, " not a valid name.")
    )

})

# Invalid mutate_fun, x and y inputs
test_that("Test error on invalid ohlc_fun, x and y inputs.", {

    expect_error(
        {mutate_fun <- "err"
        AAPL %>%
            tq_mutate_(ohlc_fun = "close", mutate_fun = mutate_fun)},
        paste0("fun = ", mutate_fun, " not a valid option.")
    )

})
