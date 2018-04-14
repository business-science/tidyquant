library(tidyquant)

# Get some data
FANG <- c("FB", "AMZN", "NFLX", "GOOG") %>%
    tq_get(get = "stock.prices", from = "2013-01-01", to = "2017-01-01")

# Transform to monthly returns using split, apply, combine framework
# tq_transform can be passed the quantmod::periodReturn function to get returns
FANG_returns <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "monthly",
                 col_rename = "returns")

# Create and aggregate portfolios in three steps
# A: Make portfolio by mapping weights
FANG_returns_mult <- FANG_returns %>%
    tq_repeat_df(n = 4)

# B: Make weights table
weights_table <- tribble(
    ~portfolio, ~stocks, ~weights,
    1,          "FB",    0.50,
    1,          "AMZN",  0.25,
    1,          "NFLX",  0.25,
    1,          "GOOG",  0.00,

    2,          "FB",    0.00,
    2,          "AMZN",  0.50,
    2,          "NFLX",  0.25,
    2,          "GOOG",  0.25,

    3,          "FB",    0.25,
    3,          "AMZN",  0.00,
    3,          "NFLX",  0.50,
    3,          "GOOG",  0.25,

    4,          "FB",    0.25,
    4,          "AMZN",  0.25,
    4,          "NFLX",  0.00,
    4,          "GOOG",  0.50) %>%
    group_by(portfolio)

# C: Aggregate portfolio with tq_portfolio. Pass wealth.index = TRUE
FANG_portfolio_wealth <- FANG_returns_mult %>%
    tq_portfolio(assets_col = symbol, returns_col = returns,
                 weights = weights_table, wealth.index = TRUE,
                 col_rename = "wealth.index") %>%
    mutate(wealth.index = wealth.index * 10000)

# Visualize!!!
FANG_portfolio_wealth  %>%
    ggplot(aes(x = date, y = wealth.index, color = factor(portfolio))) +
    geom_line(size = 2) +
    geom_smooth(method = "loess") +
    labs(title = "Portfolios: Comparing the Growth of $10K",
         subtitle = "Quickly visualize blended portfolio performance",
         x = "", y = "Investment Value",
         color = "Portfolio Number: ") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar)

ggsave("img/sample_img_3_portfolio_returns.png", width = 7.64, height = 4.5)


# Website

# FANG_portfolio_wealth  %>%
#     ggplot(aes(x = date, y = wealth.index, color = factor(portfolio))) +
#     geom_line(size = 2) +
#     geom_smooth(method = "loess") +
#     labs(x = "", y = "Investment Value",
#          color = "Portfolio Number: ") +
#     theme_tq() +
#     scale_color_tq() +
#     scale_y_continuous(labels = scales::dollar)
#
# ggsave("docs/README_3_port_returns.png", height = 4.5, width = 8)
