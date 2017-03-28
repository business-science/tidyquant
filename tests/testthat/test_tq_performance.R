library(tidyquant)

#### Setup

context(paste0("Testing tq_performance"))

# Get returns for individual stock components grouped by symbol
Ra <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Ra")

# Get returns for SP500 as baseline
Rb <- "^GSPC" %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")

# Merge stock returns with baseline
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))

# Get performance metrics
test1 <- RaRb %>%
    tq_performance(Ra = Ra, performance_fun = SharpeRatio, p = 0.95)

test2 <- RaRb %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)


#### Test Successes -----

test_that("Test1 returns grouped tibble of appropriate size", {
    # Tibble
    expect_is(test1, "tbl")
    # Number of groups
    expect_equal(dplyr::group_size(test1) %>% length(), 3)
    # Rows
    expect_equal(nrow(test1), 3)
    # Cols
    expect_equal(ncol(test1), 4)
})

test_that("Test2 returns grouped tibble of appropriate size", {
    # Tibble
    expect_is(test2, "tbl")
    # Number of groups
    expect_equal(dplyr::group_size(test2) %>% length(), 3)
    # Rows
    expect_equal(nrow(test2), 3)
    # Cols
    expect_equal(ncol(test2), 13)
})


