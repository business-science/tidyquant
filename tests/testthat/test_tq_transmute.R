library(tidyquant)
context("Testing tq_transmute")

#### Setup ----
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")

# Test 1: tq_transmute to.period
test1 <- AAPL %>%
    tq_transmute(ohlc_fun = Cl, transform_fun = to.period, period = "months")

# Test 1.2: Grouped_df test
grouped_df <- tibble(symbol = c("FB", "AMZN")) %>%
    dplyr::mutate(stock.prices = purrr::map(.x = symbol,
                                            .f = ~ tq_get(.x,
                                                          get  = "stock.prices",
                                                          from = "2015-01-01",
                                                          to   = "2016-01-01"))) %>%
    tidyr::unnest() %>%
    dplyr::group_by(symbol)

test1.2a  <- mutate(grouped_df, V1 = runSD(adjusted)) %>%
    select(-(open:adjusted))

test1.2b <- tq_transmute(grouped_df, Ad, runSD)


# Test 2: tq_transmute_xy test
test2 <- AAPL %>%
    tq_transmute_xy(x = close, transform_fun = to.period, period = "months")


# Test 3: Test transform hourly data / Test transform timezone data
time_index <- seq(from = as.POSIXct("2012-05-15 07:00"),
                  to = as.POSIXct("2012-05-17 18:00"),
                  by = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))
tz <- "Zulu"
test3 <- tibble(time_index, value) %>%
    dplyr::mutate(time_index = lubridate::ymd_hms(time_index, tz = tz)) %>%
    tq_transmute_xy(x = value, transform_fun = MACD)

# Test 4: transform to.monthly which returns character dates
test4 <- AAPL %>%
    tq_transmute(ohlc_fun = OHLCV, transform_fun = to.monthly)


#### Tests ----

test_that("Test 1 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 60)
    # Columns
    expect_equal(ncol(test1), 2)
})

test_that("Test 1.2 grouped data frames are same with mutate and tq_transmute", {
    # Return column
    expect_identical(test1.2a$V1, test1.2b$V1)
    # Groups
    expect_identical(dplyr::groups(test1.2a), dplyr::groups(test1.2b))
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
            tq_transmute(ohlc_fun = OHLCV, transform_fun = to.monthly)
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
        paste0("x = err not a valid name.")
    )
    expect_error(
        {y <-  "err"
        AAPL %>%
            tq_mutate_xy_(x = "open", y = y, mutate_fun = "Delt", k = 1)},
        paste0("y = err not a valid name.")
    )

})

# Invalid mutate_fun, x and y inputs
test_that("Test error on invalid ohlc_fun, x and y inputs.", {

    expect_error(
        {mutate_fun <- "err"
        AAPL %>%
            tq_mutate_(ohlc_fun = "close", mutate_fun = mutate_fun)},
        paste0("fun = err not a valid option.")
    )

})
