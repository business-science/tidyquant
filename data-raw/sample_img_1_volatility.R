library(tidyquant)
library(ggplot2)
library(lubridate)
library(dplyr)
# Get some data
FANG <- c("META", "AMZN", "NFLX", "GOOG") %>%
    tq_get(get = "stock.prices", from = "2007-01-01", to = "2017-01-01")

# Setup dates for zoom window
end <- ymd("2017-01-01")
start <- end - weeks(20)

# Visualize!!!
n_mavg <- 20 # Number of periods (days) for moving average
d <- FANG %>%
    dplyr::filter(date >= start - lubridate::days(2 * n_mavg)) |>
    ggplot(aes(x = date, y = close, group = factor(symbol))) +
    geom_candlestick(aes(open = open, close = close, high = high, low = low)) +
    geom_bbands(aes(high = high, low = low, close = close),
                ma_fun = SMA, n = n_mavg, sd = 2, size = 0.5) +
    labs(title = "Multiple Stocks at Once!",
         subtitle = "Quickly visualize the volatility of four stocks at once",
         x = "", y = "Closing Price") +
    coord_x_date(xlim = c(start, end)) +
    facet_wrap(~ symbol, scales = "free_y") +
    theme_tq() +
    scale_y_continuous(labels = scales::label_dollar())
# Add dropped_aes
d
ggsave("figures/man/sample_img_1_volatility.png")
