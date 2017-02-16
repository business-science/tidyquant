library(tidyquant)

#### Setup

context(paste0("Testing tq_portfolio"))

# Get stock prices
stock_prices <- c("AAPL", "GOOG", "NFLX") %>%
    tq_get(get = "stock.prices",
           from = "2010-01-01",
           to = "2015-12-31")


# Get returns for individual stock components
portfolio_monthly_returns <- stock_prices %>%
    group_by(symbol) %>%
    tq_transform(Ad, periodReturn, period = "monthly")

# Method 1: Use tq_portfolio with numeric vector of weights
weights = c(0.5, 0, 0.5)
test1 <- tq_portfolio(data = portfolio_monthly_returns,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights,
             col_rename = NULL,
             wealth.index = FALSE)


weights_df <- tibble(symbol = c("AAPL", "NFLX"),
                     weights = c(0.5, 0.5))
test2 <- tq_portfolio(data = portfolio_monthly_returns,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_df,
             col_rename = NULL,
             wealth.index = FALSE)



#### Test Successes -----

test_that("Tests return equivalent tibbles", {
    # Tibble
    expect_is(test1, "tbl")
    # Equivalence
    expect_equal(test1, test2)
    # Rows
    expect_equal(nrow(test1), 72)
})




