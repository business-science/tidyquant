#' Plot moving averages
#'
#' Plot moving averages quickly using various \code{geom_tq_} functions.
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
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
#' \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#' default), it is combined with the default mapping at the top level of the
#' plot. You must supply \code{mapping} if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'
#' If \code{NULL}, the default, the data is inherited from the plot
#' data as specified in the call to \code{\link[ggplot2]{ggplot}}.
#'
#' A \code{data.frame}, or other object, will override the plot
#' data. All objects will be fortified to produce a data frame. See
#' \code{\link[ggplot2]{fortify}} for which variables will be created.
#'
#' A \code{function} will be called with a single argument,
#' the plot data. The return value must be a \code{data.frame.}, and
#' will be used as the layer data.
#'
#' @param na.rm If \code{TRUE}, silently removes \code{NA} values, which
#' typically desired for moving averages.
#'
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}. These are
#' often aesthetics, used to set an aesthetic to a fixed value, like
#' \code{color = "red"} or \code{size = 3}. They may also be parameters
#' to the paired geom/stat.
#'
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#' rather than combining with them. This is most useful for helper functions
#' that define both data and aesthetics and shouldn't inherit behaviour from
#' the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @inheritParams TTR::SMA
#'
#'
#' @section Aesthetics:
#' The following aesthetics are understood (required are in bold):
#' \itemize{
#'    \item \strong{\code{x}}
#'    \item \strong{\code{y}}
#'    \item \code{volume}, Required for VWMA and EVWMA
#'    \item \code{alpha}
#'    \item \code{colour}
#'    \item \code{group}
#'    \item \code{linetype}
#'    \item \code{size}
#' }
#'
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
#'
#' @name tq_geom_ma
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
#'     ggplot(aes(x = date, y = adjusted)) +
#'     geom_line() +                         # Plot stock price
#'     geom_tq_sma(n = 50) +                 # Plot 50-day Moving Average
#'     geom_tq_sma(n = 200, color = "red") + # Plot 200-day Moving Average
#'     coord_x_date(xlim = c(today() - weeks(12), today()),
#'                ylim = c(100, 130))        # Zoom in
#'
#' # EVWMA
#' AAPL %>%
#'     ggplot(aes(x = date, y = adjusted)) +
#'     geom_line() +                                   # Plot stock price
#'     geom_tq_evwma(aes(volume = volume), n = 50) +   # Plot 50-day EVWMA
#'     coord_x_date(xlim = c(today() - weeks(12), today()),
#'                ylim = c(100, 130))                  # Zoom in










# Simple moving average (SMA) -----

#' @rdname geom_tq_ma
#' @export
geom_tq_ma <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE,
                     maType = SMA, n = 20, sd = 2,
                     wilder = FALSE, ratio = NULL, v = 1, wts = 1:n, ...) {

    maType <- deparse(substitute(maType))

    # Toggle if volume based
    if (maType == "VWMA" || maType == "EVWMA") {
        stat_ma <- StatMA_vol
    } else {
        stat_ma <- StatMA
    }

    ggplot2::layer(
        stat = stat_ma, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, maType = maType, sd = sd, na.rm = na.rm,
                      wilder = wilder, ratio = ratio, v = 1, wts = 1:n, ...)
    )
}


StatSMA <- ggplot2::ggproto("StatSMA", Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, params,
                                            n = 10) {

                       grid   <- tibble::tibble(x = data$x)
                       grid$y <- TTR::SMA(data$y, n = n)

                       grid
                   }
)

# Exponential moving average (EMA) -----

#' @rdname geom_tq_sma
#' @export
geom_tq_ema <- function(mapping = NULL, data = NULL,
                     position = "identity", na.rm = TRUE, show.legend = NA,
                     inherit.aes = TRUE, n = 10, wilder = FALSE, ratio = NULL, ...) {

    ggplot2::layer(
        stat = StatEMA, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, wilder = wilder, ratio = ratio, na.rm = na.rm, ...)
    )
}

StatEMA <- ggproto("StatEMA", Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, params,
                                            n = 10, wilder = FALSE, ratio = NULL) {

                       grid   <- tibble::tibble(x = data$x)
                       grid$y <- TTR::EMA(data$y,
                                          n = n,
                                          wilder = wilder,
                                          ratio = ratio)

                       grid
                   }
)

# Weighted Moving Average (WMA) -----

#' @rdname geom_tq_sma
#' @export
geom_tq_wma <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = TRUE, show.legend = NA,
                        inherit.aes = TRUE, n = 10, wts = 1:n, ...) {

    ggplot2::layer(
        stat = StatWMA, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, wts = wts, na.rm = na.rm, ...)
    )
}

StatWMA <- ggproto("StatWMA", Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, params, n = 10,
                                            wts = 1:n) {

                       grid   <- tibble::tibble(x = data$x)
                       grid$y <- TTR::WMA(data$y,
                                          n = n,
                                          wts = wts)

                       grid
                   }
)


# Double Exponential Moving Average (DEMA) -----

#' @rdname geom_tq_sma
#' @export
geom_tq_dema <- function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = TRUE, show.legend = NA,
                        inherit.aes = TRUE, n = 10, v = 1, wilder = FALSE, ratio = NULL, ...) {

    ggplot2::layer(
        stat = StatDEMA, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, v = v, wilder = wilder, ratio = ratio, na.rm = na.rm, ...)
    )
}

StatDEMA <- ggproto("StatDEMA", Stat,
                   required_aes = c("x", "y"),

                   compute_group = function(data, scales, params,
                                            n = 10, v = 1, wilder = FALSE,
                                            ratio = NULL) {

                       grid   <- tibble::tibble(x = data$x)
                       grid$y <- TTR::DEMA(data$y,
                                           n = n,
                                           v = v,
                                           wilder = wilder,
                                           ratio = ratio)

                       grid
                   }
)


# Zero lag exponential moving average (ZLEMA) -----

#' @rdname geom_tq_sma
#' @export
geom_tq_zlema <- function(mapping = NULL, data = NULL,
                          position = "identity", na.rm = TRUE, show.legend = NA,
                          inherit.aes = TRUE, n = 10, ratio = NULL, ...) {

    ggplot2::layer(
        stat = StatZLEMA, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, ratio = ratio, na.rm = na.rm, ...)
    )
}

StatZLEMA <- ggproto("StatZLEMA", Stat,
                     required_aes = c("x", "y"),

                     compute_group = function(data, scales, params,
                                              n = 10, ratio = NULL) {

                         grid   <- tibble::tibble(x = data$x)
                         grid$y <- TTR::ZLEMA(data$y,
                                              n = n,
                                              ratio = ratio)

                         grid
                     }
)

# Volume-Weighted Moving Average (VWMA) -----

#' @rdname geom_tq_sma
#' @export
geom_tq_vwma <- function(mapping = NULL, data = NULL,
                          position = "identity", na.rm = TRUE, show.legend = NA,
                          inherit.aes = TRUE, n = 10, ...) {

    ggplot2::layer(
        stat = StatVWMA, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, na.rm = na.rm, ...)
    )
}

StatVWMA <- ggproto("StatVWMA", Stat,
                     required_aes = c("x", "y", "volume"),

                     compute_group = function(data, scales, params,
                                              n = 10) {

                         grid   <- tibble::tibble(x = data$x)
                         grid$y <- TTR::VWMA(data$y,
                                             data$volume,
                                             n = n)

                         grid
                     }
)

# Elastic Volume-Weighted Moving Average (EVWMA) -----

#' @rdname geom_tq_sma
#' @export
geom_tq_evwma <- function(mapping = NULL, data = NULL,
                          position = "identity", na.rm = TRUE, show.legend = NA,
                          inherit.aes = TRUE, n = 10, ...) {

    ggplot2::layer(
        stat = StatEVWMA, geom = GeomMA, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, na.rm = na.rm, ...)
    )
}

StatEVWMA <- ggproto("StatEVWMA", Stat,
                     required_aes = c("x", "y", "volume"),

                     compute_group = function(data, scales, params,
                                              n = 10) {

                         grid   <- tibble::tibble(x = data$x)
                         grid$y <- TTR::EVWMA(data$y,
                                              data$volume,
                                              n = n)

                         grid
                     }
)


# Utility Functions -----

GeomMA <- ggproto("GeomMA", GeomLine,
                  default_aes = aes(colour = "darkblue",
                                    linetype = 2,
                                    size = 0.5,
                                    alpha = NA)
)
