#' Plot Bollinger Bands using Moving Averages
#'
#' Bollinger Bands plot a range around a moving average typically two standard deviations up and down.
#' The \code{tq_geom_bbands} function enables plotting Bollinger Bands quickly using various moving average functions.
#' The moving average functions used are specified in \code{\link[TTR]{SMA}}
#' from the TTR package. Use \code{\link{coord_x_date}} to zoom into specific plot regions.
#' The following moving averages are available:
#' \itemize{
#'    \item \strong{\href{http://www.investopedia.com/terms/s/sma.asp}{Simple moving averages (SMA)}}:
#'    Rolling mean over a period defined by \code{n}.
#'    \item \strong{\href{http://www.investopedia.com/terms/e/ema.asp}{Exponential moving averages (EMA)}}: Includes
#'    exponentially-weighted mean that gives more weight to recent observations.
#'    Uses \code{wilder} and \code{ratio} args.
#'    \item \strong{\href{http://www.investopedia.com/ask/answers/071414/whats-difference-between-moving-average-and-weighted-moving-average.asp}{Weighted moving averages (WMA)}}:
#'    Uses a set of weights, \code{wts}, to weight observations in the moving average.
#'    \item \strong{\href{http://www.investopedia.com/articles/trading/10/double-exponential-moving-average.asp}{Double exponential moving averages (DEMA)}}:
#'    Uses \code{v} volume factor, \code{wilder} and \code{ratio} args.
#'    \item \strong{\href{https://en.wikipedia.org/wiki/Zero_lag_exponential_moving_average}{Zero-lag exponential moving averages (ZLEMA)}}:
#'    Uses \code{wilder} and \code{ratio} args.
#'    \item \strong{\href{http://www.investopedia.com/articles/trading/11/trading-with-vwap-mvwap.asp}{Volume-weighted moving averages (VWMA)}}:
#'    Requires \code{volume} aesthetic.
#'    \item \strong{\href{http://www.motivewave.com/studies/elastic_volume_weighted_moving_average.htm}{Elastic, volume-weighted moving averages (EVWMA)}}:
#'    Requires \code{volume} aesthetic.
#' }
#'
#' @inheritParams tq_geom_ma
#' @inheritParams ggplot2::geom_ribbon
#' @inheritParams TTR::SMA
#' @inheritParams TTR::BBands
#'
#' @section Aesthetics:
#' The following aesthetics are understood (required are in bold):
#' \itemize{
#'    \item \strong{\code{x}}, Typically a date
#'    \item \strong{\code{y}}, Required to be the close price
#'    \item \strong{\code{high}}, Required to be the high price
#'    \item \strong{\code{low}}, Required to be the low price
#'    \item \code{volume}, Required for VWMA and EVWMA
#'    \item \code{colour}, Affects line colors
#'    \item \code{fill}, Affects ribbon fill color
#'    \item \code{alpha}, Affects ribbon alpha value
#'    \item \code{group}
#'    \item \code{linetype}
#'    \item \code{size}
#' }
#'
#' @seealso See individual modeling functions for underlying parameters:
#' \itemize{
#'    \item \code{\link[TTR]{SMA}} for simple moving averages
#'    \item \code{\link[TTR]{EMA}} for exponential moving averages
#'    \item \code{\link[TTR]{WMA}} for weighted moving averages
#'    \item \code{\link[TTR]{DEMA}} for double exponential moving averages
#'    \item \code{\link[TTR]{ZLEMA}} for zero-lag exponential moving averages
#'    \item \code{\link[TTR]{VWMA}} for volume-weighted moving averages
#'    \item \code{\link[TTR]{EVWMA}} for elastic, volume-weighted moving averages
#'    \item \code{\link{coord_x_date}} for zooming into specific regions of a plot
#' }
#'
#' @name tq_geom_bbands
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' AAPL <- tq_get("AAPL")
#'
#' # SMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_line() +           # Plot stock price
#'     tq_geom_bbands(aes(high = high, low = low), maType = SMA, n = 50) +
#'     coord_x_date(xlim = c(today() - years(1), today()), ylim = c(80, 130))
#'
#'
#' # EMA
#' AAPL %>%
#'    ggplot(aes(x = date, y = close)) +
#'    geom_line() +           # Plot stock price
#'    tq_geom_bbands(aes(high = high, low = low),
#'                   maType = EMA, wilder = TRUE, ratio = NULL, n = 50) +
#'    coord_x_date(xlim = c(today() - years(1), today()), ylim = c(80, 130))
#'
#'
#' # VWMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_line() +           # Plot stock price
#'     tq_geom_bbands(aes(high = high, low = low, volume = volume),
#'                    maType = VWMA, n = 50) +
#'     coord_x_date(xlim = c(today() - years(1), today()), ylim = c(80, 130))



#' @rdname tq_geom_bbands
#' @export
tq_geom_bbands <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = TRUE,
                           show.legend = NA, inherit.aes = TRUE,
                           maType = SMA, n = 20, sd = 2,
                           wilder = FALSE, ratio = NULL, v = 1, wts = 1:n, ...) {

    maType <- deparse(substitute(maType))

    # Check maType is valid

    # Toggle if volume based
    if (maType == "VWMA" || maType == "EVWMA") {
        stat_ribbon <- StatBBandsRibbon_vol
        stat_ma <- StatBBandsMA_vol
    } else {
        stat_ribbon <- StatBBandsRibbon
        stat_ma <- StatBBandsMA
    }

    ribbon <- ggplot2::layer(
        stat = stat_ribbon, geom = GeomBBandsRibbon, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, maType = maType, sd = sd, na.rm = na.rm,
                      wilder = wilder, ratio = ratio, v = 1, wts = 1:n, ...)
    )

    ma <- ggplot2::layer(
        stat = stat_ma, geom = GeomBBandsMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, maType = maType, sd = sd, na.rm = na.rm, alpha = NA,
                      wilder = wilder, ratio = ratio, v = 1, wts = 1:n, ...)
    )

    list(ribbon, ma)

}


StatBBandsRibbon <- ggplot2::ggproto("StatBBandsRibbon", Stat,
                            required_aes = c("x", "y", "high", "low"),

                            compute_group = function(data, scales, params,
                                                     n = 20, maType = SMA, sd = 2,
                                                     wilder = FALSE, ratio = NULL,
                                                     v = 1, wts = 1:n) {

                                grid   <- tibble::tibble(x = data$x)

                                HLC <- tibble::tibble(high  = data$high,
                                                      low   = data$low,
                                                      close = data$y)

                                bbands <- get_bbands(HLC, n, maType,
                                                     sd, wilder, ratio,
                                                     v, wts)

                                grid$ymin <- bbands[,"dn"]
                                grid$ymax <- bbands[,"up"]

                                grid
                            }
)


StatBBandsMA <- ggplot2::ggproto("StatBBandsMA", Stat,
                                 required_aes = c("x", "y", "high", "low"),

                                 compute_group = function(data, scales, params,
                                                          n = 20, maType = SMA, sd = 2,
                                                          wilder = FALSE, ratio = NULL,
                                                          v = 1, wts = 1:n) {

                                     grid   <- tibble::tibble(x = data$x)

                                     HLC <- tibble::tibble(high  = data$high,
                                                           low   = data$low,
                                                           close = data$y)

                                     bbands <- get_bbands(HLC, n, maType,
                                                          sd, wilder, ratio,
                                                          v, wts)

                                     grid$y <- bbands[,"mavg"]

                                     grid
                                 }
)

StatBBandsRibbon_vol <- ggplot2::ggproto("StatBBandsRibbon", Stat,
                                         required_aes = c("x", "y", "high", "low", "volume"),

                                         compute_group = function(data, scales, params,
                                                                  n = 20, maType = SMA, sd = 2,
                                                                  wilder = FALSE, ratio = NULL,
                                                                  v = 1, wts = 1:n) {

                                             grid   <- tibble::tibble(x = data$x)

                                             HLC <- tibble::tibble(high  = data$high,
                                                                   low   = data$low,
                                                                   close = data$y)

                                             bbands <- TTR::BBands(HLC = HLC,
                                                                   n = n,
                                                                   maType = eval(parse(text = maType)),
                                                                   volume = data$volume,
                                                                   sd = sd)

                                             grid$ymin <- bbands[,"dn"]
                                             grid$ymax <- bbands[,"up"]

                                             grid
                                         }
)

StatBBandsMA_vol <- ggplot2::ggproto("StatBBandsMA", Stat,
                                 required_aes = c("x", "y", "high", "low", "volume"),

                                 compute_group = function(data, scales, params,
                                                          n = 20, maType = SMA, sd = 2,
                                                          wilder = FALSE, ratio = NULL,
                                                          v = 1, wts = 1:n) {

                                     grid   <- tibble::tibble(x = data$x)

                                     HLC <- tibble::tibble(high  = data$high,
                                                           low   = data$low,
                                                           close = data$y)

                                     bbands <- TTR::BBands(HLC = HLC,
                                                           n = n,
                                                           maType = eval(parse(text = maType)),
                                                           volume = data$volume,
                                                           sd = sd)

                                     grid$y <- bbands[,"mavg"]

                                     grid
                                 }
)

GeomBBandsRibbon <- ggproto("GeomBBandsRibbon", GeomRibbon,
                            default_aes = aes(colour = "red",
                                              fill = "grey20",
                                              size = 0.5,
                                              linetype = 2,
                                              alpha = 0.25)
)

GeomBBandsMA <- ggproto("GeomBBandsMA", GeomLine,
                        default_aes = aes(colour = "darkblue",
                                          linetype = 2,
                                          size = 0.5,
                                          alpha = NA)
)


get_bbands <- function(HLC, n, maType, sd, wilder, ratio, v, wts) {

    if (maType == "SMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              maType = eval(parse(text = maType)),
                              sd = sd)
    } else if (maType == "EMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              maType = eval(parse(text = maType)),
                              sd = sd,
                              wilder = wilder,
                              ratio = ratio)
    } else if (maType == "DEMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              maType = eval(parse(text = maType)),
                              sd = sd,
                              wilder = wilder,
                              ratio = ratio,
                              v = v)
    } else if (maType == "WMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              maType = eval(parse(text = maType)),
                              sd = sd,
                              wts = wts)
    } else if (maType == "ZLEMA") {
        bbands <- TTR::BBands(HLC = HLC,
                              n = n,
                              maType = eval(parse(text = maType)),
                              sd = sd,
                              ratio = ratio)
    } else {

        stop(paste0("Unsupported maType: ", maType))

    }

    return(bbands)

}
