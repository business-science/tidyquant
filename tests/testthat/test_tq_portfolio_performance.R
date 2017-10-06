library(tidyquant)

#### Setup

context(paste0("Testing tq_portfolio_performance"))

# Get stock prices
stock_prices <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get = "stock.prices",
           from = "2010-01-01",
           to = "2015-12-31")


# Get returns for individual stock components
portfolio_monthly_returns <- stock_prices %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly")

# Test that numeric vector and tibble works
weights = c(0.5, 0, 0.5)
test1 <- tq_portfolio_performance(
                      data = portfolio_monthly_returns,
                      assets_col = symbol,
                      returns_col = monthly.returns,
                      weights = weights,
                      performance_fun = StdDev)

weights_df <- tibble(symbol = c("AAPL", "NFLX"),
                     weights = c(0.5, 0.5))
test2 <- tq_portfolio_performance(
                      data = portfolio_monthly_returns,
                      assets_col = symbol,
                      returns_col = monthly.returns,
                      weights = weights_df,
                      performance_fun = StdDev)


# Test default equal weighting
test3 <- suppressMessages(
    tq_portfolio_performance(data = portfolio_monthly_returns,
                 assets_col = symbol,
                 returns_col = monthly.returns,
                 weights = NULL,
                 performance_fun = VaR,
                 portfolio_method = "component"))

equal_weights <- c(1/3, 1/3, 1/3)
test3_equivalence <- tq_portfolio_performance(data = portfolio_monthly_returns,
                                              assets_col = symbol,
                                              returns_col = monthly.returns,
                                              weights = equal_weights,
                                              performance_fun = VaR,
                                              portfolio_method = "component")

# Multiple portfolio support



#### Test Successes -----

test_that("Tests return equivalent tibbles", {
    # Tibble
    expect_is(test1, "tbl")
    # Equivalence
    expect_equal(test1, test2)
    # Rows
    expect_equal(nrow(test1), 1)
})

test_that("Equal weighting works with NULL weights", {
    # Tibble
    expect_is(test3, "tbl")
    # Equivalence
    expect_equal(test3, test3_equivalence)
    # Rows
    expect_equal(nrow(test3), 3)
})


