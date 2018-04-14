library(tidyquant)

# Get some data
FANG <- c("FB", "AMZN", "NFLX", "GOOG") %>%
    tq_get(get = "stock.prices", from = "2007-01-01", to = "2017-01-01")

# Setup dates for zoom window
end <- ymd("2017-01-01")
start <- end - weeks(20)

# Visualize!!!
n_mavg <- 20 # Number of periods (days) for moving average
FANG %>%
    filter(date >= start - days(2 * n_mavg)) %>%
    ggplot(aes(x = date, y = close, group = symbol)) +
    geom_candlestick(aes(open = open, close = close, high = high, low = low)) +
    geom_bbands(aes(high = high, low = low, close = close),
                ma_fun = SMA, n = n_mavg, sd = 2, size = 0.5) +
    labs(title = "Multiple Stocks at Once!",
         subtitle = "Quickly visualize the volatility of four stocks at once",
         x = "", y = "Closing Price") +
    coord_x_date(xlim = c(start, end)) +
    facet_wrap(~ symbol, scales = "free_y") +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar)

ggsave("img/sample_img_1_volatility.png")

# Website

# FANG %>%
#     filter(date >= start - days(2 * n_mavg)) %>%
#     ggplot(aes(x = date, y = close, group = symbol)) +
#     geom_candlestick(aes(open = open, close = close, high = high, low = low)) +
#     geom_bbands(aes(high = high, low = low, close = close),
#                 ma_fun = SMA, n = n_mavg, sd = 2, size = 0.5) +
#     coord_x_date(xlim = c(start, end)) +
#     facet_wrap(~ symbol, scales = "free_y") +
#     theme_tq() +
#     scale_y_continuous(labels = scales::dollar)
#
# ggsave("docs/README_1_volatility.png", width = 8, height = 4.5)
