#' Plot moving averages
#'
#' The underlying moving average functions used are specified in `TTR::SMA()`
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
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()] or
#' [ggplot2::aes_()]. If specified and `inherit.aes = TRUE` (the
#' default), it is combined with the default mapping at the top level of the
#' plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If `NULL`, the default, the data is inherited from the plot
#' data as specified in the call to [ggplot2::ggplot()].
#'
#' A `data.frame`, or other object, will override the plot
#' data. All objects will be fortified to produce a data frame. See
#' [ggplot2::fortify()] for which variables will be created.
#'
#' A `function` will be called with a single argument,
#' the plot data. The return value must be a `data.frame.`, and
#' will be used as the layer data.
#'
#' @param na.rm If `TRUE`, silently removes `NA` values, which
#' typically desired for moving averages.
#'
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#' often aesthetics, used to set an aesthetic to a fixed value, like
#' `color = "red"` or `size = 3`. They may also be parameters
#' to the paired geom/stat.
#'
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#' rather than combining with them. This is most useful for helper functions
#' that define both data and aesthetics and shouldn't inherit behaviour from
#' the default plot specification, e.g. [ggplot2::borders()].
#'
#' @param ma_fun The function used to calculate the moving average. Seven options are
#' available including: SMA, EMA, WMA, DEMA, ZLEMA, VWMA, and EVWMA. The default is
#' `SMA`. See `TTR::SMA()` for underlying functions.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @inheritParams TTR::SMA
#'
#'
#' @section Aesthetics:
#' The following aesthetics are understood (required are in bold):
#' \itemize{
#'    \item \strong{`x`}
#'    \item \strong{`y`}
#'    \item `volume`, Required for VWMA and EVWMA
#'    \item `alpha`
#'    \item `colour`
#'    \item `group`
#'    \item `linetype`
#'    \item `size`
#' }
#'
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
#'
#' @name geom_ma
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#' library(dplyr)
#' library(ggplot2)
#'
#' AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")
#'
#' # SMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = adjusted)) +
#'     geom_line() +                         # Plot stock price
#'     geom_ma(ma_fun = SMA, n = 50) +                 # Plot 50-day SMA
#'     geom_ma(ma_fun = SMA, n = 200, color = "red") + # Plot 200-day SMA
#'     coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
#'                  ylim = c(75, 125))                     # Zoom in
#'
#' # EVWMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = adjusted)) +
#'     geom_line() +                                                   # Plot stock price
#'     geom_ma(aes(volume = volume), ma_fun = EVWMA, n = 50) +   # Plot 50-day EVWMA
#'     coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
#'                  ylim = c(75, 125))                                  # Zoom in


#' @rdname geom_ma
#' @export
geom_ma <- function(mapping = NULL, data = NULL,
                       position = "identity", na.rm = TRUE, show.legend = NA,
                       inherit.aes = TRUE,
                       ma_fun = SMA, n = 20,
                       wilder = FALSE, ratio = NULL, v = 1, wts = 1:n, ...) {

    ma_fun <- deparse(substitute(ma_fun))

    geom_ma_(mapping = mapping, data = data,
                position = position, na.rm = na.rm, show.legend = show.legend,
                inherit.aes = inherit.aes,
                ma_fun = ma_fun, n = n,
                wilder = wilder, ratio = ratio, v = v, wts = wts, ...)
}


#' @rdname geom_ma
#' @export
geom_ma_ <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE,
                     ma_fun = "SMA", n = 20,
                     wilder = FALSE, ratio = NULL, v = 1, wts = 1:n, ...) {

    # Toggle if volume based
    if (ma_fun == "VWMA" || ma_fun == "EVWMA") {
        stat_ma <- StatMA_vol
    } else {
        stat_ma <- StatMA
    }

    ggplot2::layer(
        stat = stat_ma, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(ma_fun = ma_fun, n = n, na.rm = na.rm,
                      wilder = wilder, ratio = ratio, v = 1, wts = 1:n, ...)
    )
}


StatMA <- ggplot2::ggproto("StatSMA", ggplot2::Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, params,
                                            ma_fun = "SMA", n = 10, wilder = FALSE,
                                            ratio = ratio, v = 1, wts = 1:n) {

                       grid   <- tibble::tibble(x = data$x)
                       grid$y <- get_ma(x = data$y,
                                        ma_fun = ma_fun,
                                        n = n,
                                        wilder = wilder,
                                        ratio = ratio,
                                        v = v,
                                        wts = wts)

                       grid
                   }
)

StatMA_vol <- ggplot2::ggproto("StatSMA_vol", ggplot2::Stat,
                           required_aes = c("x", "y", "volume"),

                           compute_group = function(data, scales, params,
                                                    ma_fun, n = 10,
                                                    wilder = FALSE, ratio = NULL,
                                                    v = 1, wts = 1:n) {

                               grid   <- tibble::tibble(x = data$x)

                               ma_fun <- eval(parse(text = paste0("TTR::", ma_fun)))

                               grid$y <- ma_fun(price = data$y,
                                                volume = data$volume,
                                                n = n)

                               grid
                           }
)



# Utility Functions -----

GeomMA <- ggplot2::ggproto("GeomMA", ggplot2::GeomLine,
                  default_aes = ggplot2::aes(colour = "darkblue",
                                    linetype = 2,
                                    size = 0.5,
                                    alpha = NA)
)

get_ma <- function(ma_fun, x, n, wilder, ratio, v, wts) {

    if (ma_fun == "SMA") {
        ret <- TTR::SMA(x = x,
                        n = n)
    } else if (ma_fun == "EMA") {
        ret <- TTR::EMA(x = x,
                        n = n,
                        wilder = wilder,
                        ratio = ratio)
    } else if (ma_fun == "DEMA") {
        ret <- TTR::DEMA(x = x,
                         n = n,
                         wilder = wilder,
                         ratio = ratio,
                         v = v)
    } else if (ma_fun == "WMA") {
        ret <- TTR::WMA(x = x,
                        n = n,
                        wts = wts)
    } else if (ma_fun == "ZLEMA") {
        ret <- TTR::ZLEMA(x = x,
                          n = n,
                          ratio = ratio)
    } else {

        stop(paste0("Unsupported ma_fun: ", ma_fun))

    }

    return(ret)

}
