library(tidyquant)
context("Testing tq_mutate")

#### Setup ----
AAPL <- tq_get("AAPL", get = "stock.prices", from = "2010-01-01", to = "2015-01-01")

# Test 1: tq_mutate piping test
test1 <- AAPL %>%
    tq_mutate(close, MACD) %>%
    tq_mutate(high:close, BBands) %>%
    tq_mutate(open:close, OpCl)

# Test 1.1: tq_mutate piping test; test renaming of column names on mutate
test1.1 <- AAPL %>%
    tq_mutate(close, rollapply, width = 7, FUN = mean) %>%
    tq_mutate(open, SMA, n = 3) %>%
    tq_mutate(close, rollapply, width = 9, FUN = mean) %>%
    tq_mutate(open, SMA, n = 5) %>%
    tq_mutate(high:close, BBands, n = 20) %>%
    tq_mutate(high:close, BBands, n = 50)

test1.1_names <- c("date", "open", "high", "low", "close", "volume", "adjusted",
                   "rollapply", "SMA", "rollapply..1", "SMA..1", "dn", "mavg",
                   "up", "pctB", "dn..1", "mavg..1", "up..1", "pctB..1")

# Test 1.2: Grouped_df test
grouped_df <- tibble(symbol = c("FB", "AMZN")) %>%
    dplyr::mutate(stock.prices = purrr::map(.x = symbol,
                                            .f = ~ tq_get(.x,
                                                          get  = "stock.prices",
                                                          from = "2015-01-01",
                                                          to   = "2016-01-01"))) %>%
    tidyr::unnest() %>%
    dplyr::group_by(symbol)

test1.2a  <- mutate(grouped_df, V1 = runSD(adjusted))

test1.2b <- tq_mutate(grouped_df, adjusted, runSD, col_rename = "V1")

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
    timetk::tk_tbl(preserve_index = T, silent = T) %>%
    tq_mutate_xy(x = value, mutate_fun = MACD)

# Test 4: Bind hourly data with tq_mutate_xy
test4 <- tibble(time_index, value) %>%
    tq_mutate_xy(x = value, mutate_fun = MACD)

# Test 5:
test5 <- c("AAPL", "FB") %>%
    tq_get(from = "2016-01-01",
           to   = "2017-01-01") %>%
    group_by(symbol)
my_lm_fun <- function(data) {
    coef(lm(close ~ open, data = as_data_frame(data)))
}
test5 <- test5 %>%
    tq_mutate(mutate_fun = rollapply,
              width      = 90,
              FUN        = my_lm_fun,
              by.column  = FALSE,
              col_rename = c("coef.0", "coef.1"))

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

test_that("Test 1.2 grouped data frames are same with mutate and tq_mutate", {
    expect_identical(test1.2a, test1.2b)
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

test_that("Test 5 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test5, "tbl")
    # Rows
    expect_equal(nrow(test5), 504)
    # Columns
    expect_equal(ncol(test5), 10)
})

# Incompatible structure: trying to mutate different periodicities.
test_that("Test error on incompatible structures.", {
    expect_error(
        AAPL %>%
            tq_mutate(select = NULL, mutate_fun = to.period, period = "months"),
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
    a <- seq(1:100)
    expect_error(
        seq(1:100) %>%
            tq_mutate(select = NULL, mutate_fun = to.monthly)
    )
    expect_error(
        seq(1:100) %>%
            tq_mutate_xy(x = a, mutate_fun = to.monthly)
    )

    # No date columns
    expect_error(
        tibble(a = seq(1:100)) %>%
            tq_mutate(select = NULL, mutate_fun = to.monthly),
        "No date or POSIXct column found in `data`."
    )
    expect_error(
        tibble(a = seq(1:100)) %>%
            tq_mutate_xy(x = a, mutate_fun = to.monthly),
        "No date or POSIXct column found in `data`."
    )
})

# Invalid select, x and y inputs
test_that("Test error on invalid select, x and y inputs.", {

    expect_error(
        {select <- "err"
        AAPL %>%
            tq_mutate_(select = select, mutate_fun = "to.monthly")}
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
test_that("Test error on invalid mutate_fun, x and y inputs.", {

    expect_error(
        {mutate_fun <- "err"
        AAPL %>%
            tq_mutate_(select = "close", mutate_fun = mutate_fun)},
        paste0("fun = err not a valid option.")
    )

})
