library(tidyquant)
context("Testing tq_mutate")

#### Setup ----
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")

# Test 1: tq_mutate piping test
test1 <- AAPL %>%
    tq_mutate(Cl, MACD) %>%
    tq_mutate(HLC, BBands) %>%
    tq_mutate(OHLC, OpCl)

# Test 1.1: tq_mutate piping test; test renaming of column names on mutate
test1.1 <- AAPL %>%
    tq_mutate(Cl, rollapply, width = 7, FUN = mean) %>%
    tq_mutate(Op, SMA, n = 3) %>%
    tq_mutate(Cl, rollapply, width = 9, FUN = mean) %>%
    tq_mutate(Op, SMA, n = 5) %>%
    tq_mutate(HLC, BBands, n = 20) %>%
    tq_mutate(HLC, BBands, n = 50)

test1.1_names <- c("date", "open", "high", "low", "close", "volume", "adjusted",
                   "rollapply", "sma", "rollapply.1", "sma.1", "dn", "mavg",
                   "up", "pctb", "dn.1", "mavg.1", "up.1", "pctb.1")

# Test 2: tq_mutate_xy piping test
test2 <- AAPL %>%
    tq_mutate_xy(x = close, mutate_fun = MACD) %>%
    tq_mutate_xy(x = open, y = close, mutate_fun = Delt, k = 1:4)


# Test 3: Bind hourly xts data
time_index <- seq(from = as.POSIXct("2012-05-15 07:00"),
                  to = as.POSIXct("2012-05-17 18:00"),
                  by = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))
hourly_data <- xts(value, order.by = time_index)
test3 <- hourly_data %>%
    as_tibble(preserve_row_names = T) %>%
    dplyr::mutate(row.names = lubridate::ymd_hms(row.names, tz = "US/Mountain")) %>%
    tq_mutate_xy(x = V1, mutate_fun = MACD)

# Test 4: Bind hourly data with tq_mutate_xy
test4 <- tibble(time_index, value) %>%
    tq_mutate_xy(x = value, mutate_fun = MACD)



#### Tests ----

test_that("Test 1 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 1258)
    # Columns
    expect_equal(ncol(test1), 14)
})

test_that("Test 1.1 returns correct column names", {
    # Column names
    expect_equal(colnames(test1.1), test1.1_names)
})

test_that("Test 2 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test2, "tbl")
    # Rows
    expect_equal(nrow(test2), 1258)
    # Columns
    expect_equal(ncol(test2), 13)
})

test_that("Test 3 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test3, "tbl")
    # Rows
    expect_equal(nrow(test3), 60)
    # Columns
    expect_equal(ncol(test3), 4)
})

# Incompatible structure: trying to mutate different periodicities.
test_that("Test error on incompatible structures.", {
    expect_error(
        AAPL %>%
            tq_mutate(ohlc_fun = OHLCV, mutate_fun = to.period, period = "months"),
        "Could not join. Incompatible structures."
    )
    expect_error(
        AAPL %>%
            tq_mutate_xy(x = close, mutate_fun = to.period, period = "months"),
        "Could not join. Incompatible structures."
    )
})


# Invalid data inputs
test_that("Test error on invalid data inputs.", {

    # Non-data.frame objects
    expect_error(
        a = seq(1:100) %>%
            tq_mutate(ohlc_fun = OHLCV, mutate_fun = to.monthly)
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
