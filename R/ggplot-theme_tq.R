#' tidyquant themes for ggplot2.
#'
#' The `theme_tq()` function creates a custom theme using tidyquant colors.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @seealso [scale_manual()]
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Get stock prices
#' AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")
#'
#' # Plot using ggplot with theme_tq
#' AAPL %>% ggplot(aes(x = date, y = close)) +
#'        geom_line() +
#'        geom_bbands(aes(high = high, low = low, close = close),
#'                    ma_fun = EMA,
#'                    wilder = TRUE,
#'                    ratio = NULL,
#'                    n = 50) +
#'        coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
#'                  ylim = c(75, 125)) +
#'        labs(title = "Apple BBands",
#'             x = "Date",
#'             y = "Price") +
#'        theme_tq()
#'
#' @name theme_tq
NULL

#' @rdname theme_tq
#' @export
#' @importFrom ggplot2 `%+replace%`
theme_tq <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey80"

    # Starts with theme_grey and then modify some parts
    ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(

            # Base Inherited Elements
            line               =  ggplot2::element_line(colour = blue, size = 0.5, linetype = 1,
                                                        lineend = "butt"),
            rect               =  ggplot2::element_rect(fill = white, colour = blue,
                                                        size = 0.5, linetype = 1),
            text               =  ggplot2::element_text(family = base_family, face = "plain",
                                                        colour = blue, size = base_size,
                                                        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                                        margin = ggplot2::margin(), debug = FALSE),

            # Axes
            axis.line          = ggplot2::element_blank(),
            axis.text          = ggplot2::element_text(size = ggplot2::rel(0.8)),
            axis.ticks         = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)),
            axis.title         = ggplot2::element_text(size = ggplot2::rel(1.0)),

            # Panel
            panel.background   = ggplot2::element_rect(fill = white, color = NA),
            panel.border       = ggplot2::element_rect(fill = NA, size = ggplot2::rel(1/2), color = blue),
            panel.grid.major   = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)),
            panel.grid.minor   = ggplot2::element_line(color = grey, size = ggplot2::rel(1/3)),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.spacing      = ggplot2::unit(.75, "cm"),

            # Legend
            legend.key         = ggplot2::element_rect(fill = white, color = NA),
            legend.position    = "bottom",

            # Strip (Used with multiple panels)
            strip.background   = ggplot2::element_rect(fill = blue, color = blue),
            strip.text         = ggplot2::element_text(color = white, size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 5, b = 5)),

            # Plot
            plot.title         = ggplot2::element_text(size = ggplot2::rel(1.2), hjust = 0,
                                                       margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
            plot.subtitle      = ggplot2::element_text(size = ggplot2::rel(0.9), hjust = 0,
                                                       margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),

            # Complete theme
            complete = TRUE
        )
}

#' @rdname theme_tq
#' @export
theme_tq_dark <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey50"

    # Starts with theme_tq and then invert some colors
    theme_tq(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(

            # Axes
            axis.ticks         = ggplot2::element_line(color = blue, size = ggplot2::rel(1/3)),

            # Panel
            panel.background   = ggplot2::element_rect(fill = grey, color = NA),
            panel.grid.major   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),
            panel.grid.minor   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),

            # Complete theme
            complete = TRUE
        )
}

#' @rdname theme_tq
#' @export
theme_tq_green <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey80"

    # Starts with theme_tq and then invert some colors
    theme_tq(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(

            # Axes
            axis.ticks         = ggplot2::element_line(color = blue, size = ggplot2::rel(1/3)),

            # Panel
            panel.background   = ggplot2::element_rect(fill = green, color = NA),
            panel.grid.major   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),
            panel.grid.minor   = ggplot2::element_line(color = white, size = ggplot2::rel(1/3)),

            # Complete theme
            complete = TRUE
        )
}
