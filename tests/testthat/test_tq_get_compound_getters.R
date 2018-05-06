library(tidyquant)

#### Setup
get <- c("stock.prices", "dividends")
context(paste0("Testing tq_get(get = c('stock.prices', 'dividends'))"))

test1 <- tq_get("AAPL", get = get,
                from = "2016-01-01", to = "2016-06-01")

test2 <- c("AAPL", "FB") %>%
    tq_get(get = get, from = "2016-01-01", to = "2016-06-01")

test3 <- tibble(symbol = c("GOOG", "FB", "SBUX"), industry = c("Tech", "Tech", "Food & Bev")) %>%
    tq_get(get = get, from = "2016-01-01", to = "2016-06-01")



#### Test Successes -----

test_that("Test 1 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    # Rows
    expect_equal(nrow(test1), 1)
    # Columns
    expect_equal(ncol(test1), 3)
})

test_that("Test 2 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test2, "tbl")
    # Rows
    expect_equal(nrow(test2), 2)
    # Columns
    expect_equal(ncol(test2), 3)
})

test_that("Test 3 returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test3, "tbl")
    # Rows
    expect_equal(nrow(test3), 3)
    # Columns
    expect_equal(ncol(test3), 4)
})

#### Test Errors ----

test_that("Error on invalid compound gets", {
    expect_error(
        c("AAPL", "GOOG") %>%
            tq_get(c("stock.prices", "dividends", "economic.data"))
    )
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(
        c("AAPL", "XYZ") %>%
            tq_get(get = get)
    )
    # # Rows
    # expect_equal(nrow(test4), 1)
    # # Columns
    # expect_equal(ncol(test4), 3)
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(
        c("AAPL", "XYZ") %>%
            tq_get(get = get, complete_cases = FALSE)
    )
    # # Rows
    # expect_equal(nrow(test5), 2)
    # # Columns
    # expect_equal(ncol(test5), 3)
})
