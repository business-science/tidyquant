#' Plot Bollinger Bands using Moving Averages
#'
#' Bollinger Bands plot a range around a moving average typically two standard deviations up and down.
#' The `geom_bbands()` function enables plotting Bollinger Bands quickly using various moving average functions.
#' The moving average functions used are specified in `TTR::SMA()`
#' from the TTR package. Use [coord_x_date()] to zoom into specific plot regions.
#' The following moving averages are available:
#' \itemize{
#'    \item \strong{\href{https://www.investopedia.com/terms/s/sma.asp}{Simple moving averages (SMA)}}:
#'    Rolling mean over a period defined by `n`.
#'    \item \strong{\href{https://www.investopedia.com/terms/e/ema.asp}{Exponential moving averages (EMA)}}: Includes
#'    exponentially-weighted mean that gives more weight to recent observations.
#'    Uses `wilder` and `ratio` args.
#'    \item \strong{\href{https://www.investopedia.com/ask/answers/071414/whats-difference-between-moving-average-and-weighted-moving-average.asp}{Weighted moving averages (WMA)}}:
#'    Uses a set of weights, `wts`, to weight observations in the moving average.
#'    \item \strong{\href{https://www.investopedia.com/articles/trading/10/double-exponential-moving-average.asp}{Double exponential moving averages (DEMA)}}:
#'    Uses `v` volume factor, `wilder` and `ratio` args.
#'    \item \strong{\href{https://en.wikipedia.org/wiki/Zero_lag_exponential_moving_average}{Zero-lag exponential moving averages (ZLEMA)}}:
#'    Uses `wilder` and `ratio` args.
#'    \item \strong{\href{https://www.investopedia.com/articles/trading/11/trading-with-vwap-mvwap.asp}{Volume-weighted moving averages (VWMA)}}:
#'    Requires `volume` aesthetic.
#'    \item \strong{\href{https://www.motivewave.com/studies/elastic_volume_weighted_moving_average.htm}{Elastic, volume-weighted moving averages (EVWMA)}}:
#'    Requires `volume` aesthetic.
#' }
#'
#' @inheritParams geom_ma
#' @inheritParams ggplot2::geom_ribbon
#' @inheritParams TTR::SMA
#' @inheritParams TTR::BBands
#' @param color_ma,color_bands Select the line color to be applied for the moving
#' average line and the Bollinger band line.
#' @param alpha Used to adjust the alpha transparency for the BBand ribbon.
#' @param fill Used to adjust the fill color for the BBand ribbon.
#'
#' @section Aesthetics:
#' The following aesthetics are understood (required are in bold):
#' \itemize{
#'    \item \strong{`x`}, Typically a date
#'    \item \strong{`high`}, Required to be the high price
#'    \item \strong{`low`}, Required to be the low price
#'    \item \strong{`close`}, Required to be the close price
#'    \item `volume`, Required for VWMA and EVWMA
#'    \item `colour`, Affects line colors
#'    \item `fill`, Affects ribbon fill color
#'    \item `alpha`, Affects ribbon alpha value
#'    \item `group`
#'    \item `linetype`
#'    \item `size`
#' }
#'
#' @seealso See individual modeling functions for underlying parameters:
#' \itemize{
#'    \item `TTR::SMA()` for simple moving averages
#'    \item `TTR::EMA()` for exponential moving averages
#'    \item `TTR::WMA()` for weighted moving averages
#'    \item `TTR::DEMA()` for double exponential moving averages
#'    \item `TTR::ZLEMA()` for zero-lag exponential moving averages
#'    \item `TTR::VWMA()` for volume-weighted moving averages
#'    \item `TTR::EVWMA()` for elastic, volume-weighted moving averages
#'    \item [coord_x_date()] for zooming into specific regions of a plot
#' }
#'
#' @name geom_bbands
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#' library(dplyr)
#' library(ggplot2)
#'
#'
#' AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")
#'
#' # SMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_line() +           # Plot stock price
#'     geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 50) +
#'     coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
#'                  ylim = c(75, 125))
#'
#'
#' # EMA
#' AAPL %>%
#'    ggplot(aes(x = date, y = close)) +
#'    geom_line() +           # Plot stock price
#'    geom_bbands(aes(high = high, low = low, close = close),
#'                   ma_fun = EMA, wilder = TRUE, ratio = NULL, n = 50) +
#'    coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
#'                 ylim = c(75, 125))
#'
#'
#' # VWMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_line() +           # Plot stock price
#'     geom_bbands(aes(high = high, low = low, close = close, volume = volume),
#'                    ma_fun = VWMA, n = 50) +
#'     coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
#'                 ylim = c(75, 125))


#' @rdname geom_bbands
#' @export
geom_bbands <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = TRUE, show.legend = NA,
                       inherit.aes = TRUE,
                       ma_fun = SMA, n = 20, sd = 2,
                       wilder = FALSE, ratio = NULL, v = 1, wts = 1:n,
                       color_ma = "darkblue", color_bands = "red",
                       alpha = 0.15, fill = "grey20", ...) {

    ma_fun <- deparse(substitute(ma_fun))

    geom_bbands_(mapping = mapping, data = data,
                position = position, na.rm = na.rm, show.legend = show.legend,
                inherit.aes = inherit.aes,
                ma_fun = ma_fun, n = n, sd = sd,
                wilder = wilder, ratio = ratio, v = v, wts = wts,
                color_ma = color_ma, color_bands = color_bands,
                alpha = alpha, fill = fill, ...)
}


#' @rdname geom_bbands
#' @export
geom_bbands_ <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE,
                           show.legend = NA, inherit.aes = TRUE,
                           ma_fun = "SMA", n = 10, sd = 2,
                           wilder = FALSE, ratio = NULL, v = 1, wts = 1:n,
                           color_ma = "darkblue", color_bands = "red",
                           alpha = 0.15, fill = "grey20", ...) {

    # Check ma_fun is valid

    # Toggle if volume based
    if (ma_fun == "VWMA" || ma_fun == "EVWMA") {
        stat_ribbon <- StatBBandsRibbon_vol
        stat_ma <- StatBBandsMA_vol
    } else {
        stat_ribbon <- StatBBandsRibbon
        stat_ma <- StatBBandsMA
    }

    ribbon <- ggplot2::layer(
        stat = stat_ribbon, geom = GeomBBandsRibbon, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, ma_fun = ma_fun, sd = sd, na.rm = na.rm,
                      wilder = wilder, ratio = ratio, v = v, wts = wts,
                      color = color_bands, alpha = alpha, fill = fill, ...)
    )

    ma <- ggplot2::layer(
        stat = stat_ma, geom = GeomBBandsMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, ma_fun = ma_fun, sd = sd, na.rm = na.rm,
                      wilder = wilder, ratio = ratio, v = v, wts = wts,
                      color = color_ma, alpha = NA, fill = fill, ...)
    )

    list(ribbon, ma)

}


StatBBandsRibbon <- ggplot2::ggproto("StatBBandsRibbon", ggplot2::Stat,
                            required_aes = c("x", "high", "low", "close"),

                            compute_group = function(data, scales, params,
                                                     n = 10, ma_fun = SMA, sd = 2,
                                                     wilder = FALSE, ratio = NULL,
                                                     v = 1, wts = 1:n) {

                                grid   <- tibble::tibble(x = data$x)

                                HLC <- tibble::tibble(high  = data$high,
                                                      low   = data$low,
                                                      close = data$close)

                                bbands <- get_bbands(HLC, n, ma_fun,
                                                     sd, wilder, ratio,
                                                     v, wts)

                                grid$ymin <- bbands[,"dn"]
                                grid$ymax <- bbands[,"up"]

                                grid
                            }
)


StatBBandsMA <- ggplot2::ggproto("StatBBandsMA", ggplot2::Stat,
                                 required_aes = c("x", "high", "low", "close"),

                                 compute_group = function(data, scales, params,
                                                          ma_fun = "SMA", n = 20, sd = 2,
                                                          wilder = FALSE, ratio = NULL,
                                                          v = 1, wts = 1:n,
                                                          fill = "grey20") {

                                     grid   <- tibble::tibble(x = data$x)

                                     HLC <- tibble::tibble(high  = data$high,
                                                           low   = data$low,
                                                           close = data$close)

                                     bbands <- get_bbands(HLC, n, ma_fun,
                                                          sd, wilder, ratio,
                                                          v, wts)

                                     grid$y <- bbands[,"mavg"]

                                     grid
                                 }
)

StatBBandsRibbon_vol <- ggplot2::ggproto("StatBBandsRibbon", ggplot2::Stat,
                                         required_aes = c("x", "high", "low", "close", "volume"),

                                         compute_group = function(data, scales, params,
                                                                  ma_fun = "SMA", n = 10, sd = 2,
                                                                  wilder = FALSE, ratio = NULL,
                                                                  v = 1, wts = 1:n) {

                                             grid   <- tibble::tibble(x = data$x)

                                             HLC <- tibble::tibble(high  = data$high,
                                                                   low   = data$low,
                                                                   close = data$close)

                                             bbands <- TTR::BBands(HLC = HLC,
                                                                   n = n,
                                                                   ma_fun = eval(parse(text = ma_fun)),
                                                                   volume = data$volume,
                                                                   sd = sd)

                                             grid$ymin <- bbands[,"dn"]
                                             grid$ymax <- bbands[,"up"]

                                             grid
                                         }
)

StatBBandsMA_vol <- ggplot2::ggproto("StatBBandsMA", ggplot2::Stat,
                                 required_aes = c("x", "high", "low", "close", "volume"),

                                 compute_group = function(data, scales, params,
                                                          n = 20, ma_fun = SMA, sd = 2,
                                                          wilder = FALSE, ratio = NULL,
                                                          v = 1, wts = 1:n,
                                                          fill = "grey20") {

                                     grid   <- tibble::tibble(x = data$x)

                                     HLC <- tibble::tibble(high  = data$high,
                                                           low   = data$low,
                                                           close = data$close)

                                     bbands <- TTR::BBands(HLC = HLC,
                                                           n = n,
                                                           ma_fun = eval(parse(text = ma_fun)),
                                                           volume = data$volume,
                                                           sd = sd)

                                     grid$y <- bbands[,"mavg"]

                                     grid
                                 }
)

# Geoms ----

GeomBBandsRibbon <- ggplot2::ggproto("GeomBBandsRibbon", ggplot2::GeomRibbon,
                            default_aes = ggplot2::aes(colour = "red",
                                              fill = "grey20",
                                              size = 0.5,
                                              linetype = 2,
                                              alpha = 0.15)
)

GeomBBandsMA <- ggplot2::ggproto("GeomBBandsMA", ggplot2::GeomLine,
                        default_aes = ggplot2::aes(colour = "darkblue",
                                          linetype = 2,
                                          size = 0.5,
                                          alpha = NA)
)

# Get BBands  -----

get_bbands <- function(HLC, n, ma_fun, sd, wilder, ratio, v, wts) {

    if (ma_fun == "SMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              ma_fun = eval(parse(text = ma_fun)),
                              sd = sd)
    } else if (ma_fun == "EMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              ma_fun = eval(parse(text = ma_fun)),
                              sd = sd,
                              wilder = wilder,
                              ratio = ratio)
    } else if (ma_fun == "DEMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              ma_fun = eval(parse(text = ma_fun)),
                              sd = sd,
                              wilder = wilder,
                              ratio = ratio,
                              v = v)
    } else if (ma_fun == "WMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              ma_fun = eval(parse(text = ma_fun)),
                              sd = sd,
                              wts = wts)
    } else if (ma_fun == "ZLEMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              ma_fun = eval(parse(text = ma_fun)),
                              sd = sd,
                              ratio = ratio)
    } else {

        stop(paste0("Unsupported ma_fun: ", ma_fun))

    }

    return(bbands)

}
