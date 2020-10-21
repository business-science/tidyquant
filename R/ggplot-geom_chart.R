#' Plot Financial Charts in ggplot2
#'
#' Financial charts provide visual cues to open, high, low, and close prices.
#' Use [coord_x_date()] to zoom into specific plot regions.
#' The following financial chart geoms are available:
#' \itemize{
#'    \item \strong{\href{https://www.investopedia.com/terms/b/barchart.asp}{Bar Chart}}
#'    \item \strong{\href{https://www.investopedia.com/terms/c/candlestick.asp}{Candlestick Chart}}
#' }
#'
#' @inheritParams geom_ma
#' @inheritParams ggplot2::geom_linerange
#' @param colour_up,colour_down Select colors to be applied based on price movement
#' from open to close. If close >= open, `colour_up` is used. Otherwise,
#' `colour_down` is used. The default is "darkblue" and "red", respectively.
#' @param fill_up,fill_down Select fills to be applied based on price movement
#' from open to close. If close >= open, `fill_up` is used. Otherwise,
#' `fill_down` is used. The default is "darkblue" and "red", respectively.
#' Only affects `geom_candlestick`.
#'
#' @section Aesthetics:
#' The following aesthetics are understood (required are in bold):
#' \itemize{
#'    \item \strong{`x`}, Typically a date
#'    \item \strong{`open`}, Required to be the open price
#'    \item \strong{`high`}, Required to be the high price
#'    \item \strong{`low`}, Required to be the low price
#'    \item \strong{`close`}, Required to be the close price
#'    \item `alpha`
#'    \item `group`
#'    \item `linetype`
#'    \item `size`
#' }
#'
#' @seealso See individual modeling functions for underlying parameters:
#' \itemize{
#'    \item [geom_ma()] for adding moving averages to ggplots
#'    \item [geom_bbands()] for adding Bollinger Bands to ggplots
#'    \item [coord_x_date()] for zooming into specific regions of a plot
#' }
#'
#' @name geom_chart
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
#' # Bar Chart
#' AAPL %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_barchart(aes(open = open, high = high, low = low, close = close)) +
#'     geom_ma(color = "darkgreen") +
#'     coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
#'                  ylim = c(75, 125))
#'
#' # Candlestick Chart
#' AAPL %>%
#'     ggplot(aes(x = date, y = close)) +
#'     geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
#'     geom_ma(color = "darkgreen") +
#'     coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
#'                  ylim = c(75, 125))

# Bar Chart -----

#' @rdname geom_chart
#' @export
geom_barchart <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = TRUE, show.legend = NA,
                             inherit.aes = TRUE,
                             colour_up = "darkblue", colour_down = "red",
                             fill_up = "darkblue", fill_down = "red",
                             ...) {



    linerange <- ggplot2::layer(
        stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                      colour_up = colour_up, colour_down = colour_down, ...)
    )

    segment_left <- ggplot2::layer(
        stat = StatSegmentLeftBC, geom = GeomSegmentBC, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                      colour_up = colour_up, colour_down = colour_down, ...)
    )

    segment_right <- ggplot2::layer(
        stat = StatSegmentRightBC, geom = GeomSegmentBC, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                      colour_up = colour_up, colour_down = colour_down, ...)
    )

    list(linerange, segment_left, segment_right)
}

StatLinerangeBC <- ggplot2::ggproto("StatLinerangeBC", ggplot2::Stat,
                                    required_aes = c("x", "open", "high", "low", "close"),

                                    compute_group = function(data, scales, params,
                                                             fill_up, fill_down,
                                                             colour_up, colour_down) {

                                        data <-  data %>%
                                            dplyr::mutate(color = ifelse(open < close, colour_up, colour_down))

                                        tibble::tibble(x = data$x,
                                                       ymin = data$low,
                                                       ymax = data$high,
                                                       colour = data$color)
                                    }
)

StatSegmentLeftBC <- ggplot2::ggproto("StatSegmentLeftBC", ggplot2::Stat,
                                    required_aes = c("x", "open", "high", "low", "close"),

                                    compute_group = function(data, scales, params,
                                                             fill_up, fill_down,
                                                             colour_up, colour_down) {

                                        data <-  data %>%
                                            dplyr::mutate(color = ifelse(open < close, colour_up, colour_down))

                                        tibble::tibble(x    = data$x,
                                                       xend = data$x - 0.5,
                                                       y    = data$open,
                                                       yend = data$open,
                                                       colour = data$color)
                                    }
)


StatSegmentRightBC <- ggplot2::ggproto("StatSegmentRightBC", ggplot2::Stat,
                                      required_aes = c("x", "open", "high", "low", "close"),

                                      compute_group = function(data, scales, params,
                                                               fill_up, fill_down,
                                                               colour_up, colour_down) {

                                          data <-  data %>%
                                              dplyr::mutate(color = ifelse(open < close, colour_up, colour_down))

                                          tibble::tibble(x    = data$x,
                                                         xend = data$x + 0.5,
                                                         y    = data$close,
                                                         yend = data$close,
                                                         colour = data$color)
                                      }
)

GeomLinerangeBC <- ggplot2::ggproto("GeomLinerangeBC", ggplot2::GeomLinerange,
                           default_aes = ggplot2::aes(size = 0.5,
                                             linetype = 1,
                                             alpha = NA)
)

GeomSegmentBC <- ggplot2::ggproto("GeomSegmentBC", ggplot2::GeomSegment,
                       default_aes = ggplot2::aes(size = 0.5,
                                         linetype = 1,
                                         alpha = NA)
)


# Candlestick Chart -----

#' @rdname geom_chart
#' @export
geom_candlestick <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = TRUE, show.legend = NA,
                                inherit.aes = TRUE,
                                colour_up = "darkblue", colour_down = "red",
                                fill_up = "darkblue", fill_down = "red",
                                ...) {

    linerange <- ggplot2::layer(
        stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                      colour_up = colour_up, colour_down = colour_down, ...)
    )

    rect <- ggplot2::layer(
        stat = StatRectCS, geom = GeomRectCS, data = data, mapping = mapping,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                      colour_up = colour_up, colour_down = colour_down, ...)
    )



    list(linerange, rect)
}

StatRectCS <- ggplot2::ggproto("StatRectCS", ggplot2::Stat,
                                required_aes = c("x", "open", "high", "low", "close"),

                                compute_group = function(data, scales, params,
                                                         fill_up, fill_down,
                                                         colour_up, colour_down) {

                                    data <-  data %>%
                                        dplyr::mutate(fill = ifelse(open < close, fill_up, fill_down),
                                                      ymin = ifelse(open < close, open, close),
                                                      ymax = ifelse(open < close, close, open))

                                    tibble::tibble(xmin = data$x - 0.45,
                                                   xmax = data$x + 0.45,
                                                   ymin = data$ymin,
                                                   ymax = data$ymax,
                                                   fill = data$fill)
                                }
)





GeomRectCS <- ggplot2::ggproto("GeomRectCS", ggplot2::GeomRect,
                      default_aes = ggplot2::aes(colour = NA,
                                        size = 0.5,
                                        linetype = 1,
                                        alpha = NA)
)


