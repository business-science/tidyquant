library(tidyquant)

# Get some data
FANG <- c("META", "AMZN", "NFLX", "GOOG") %>%
    tq_get(get = "stock.prices", from = "2013-01-01", to = "2017-01-01")

# Transform to monthly returns using split, apply, combine framework
# tq_transform can be passed the quantmod::periodReturn function to get returns
FANG_returns <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "monthly",
                 col_rename = "returns")

# Get wealth index with a quick mutation cumprod(1 + return)
init_investment <- 10000
FANG_wealth <- FANG_returns %>%
    mutate(wealth.index = init_investment * cumprod(1 + returns))

# Visualize!!!
FANG_wealth %>%
    ggplot(aes(x = date, y = wealth.index, color = symbol)) +
    geom_line(linewidth = 2) +
    geom_smooth(method = "loess") +
    labs(title = "Individual Stocks: Comparing the Growth of $10K",
         subtitle = "Quickly visualize stock performance",
         x = "", y = "Investment Value") +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar)

ggsave("man/figures/sample_img_2_stock_returns.png")
