#' tidyquant themes for ggplot2.
#'
#' The \code{theme_tq()} function creates a custom theme using tidyquant colors.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @seealso \code{\link{scale_manual}}
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' # Get stock prices
#' AAPL <- tq_get("AAPL")
#'
#' # Plot using ggplot with theme_tq
#' AAPL %>% ggplot(aes(x = date, y = close)) +
#'        geom_line() +
#'        geom_bbands(aes(high = high, low = low, close = close),
#'                    ma_fun = EMA,
#'                    wilder = TRUE,
#'                    ratio = NULL,
#'                    n = 50) +
#'        coord_x_date(xlim = c(today() - years(1), today()),
#'                     ylim = c(80, 130)) +
#'        labs(title = "Apple BBands",
#'             x = "Date",
#'             y = "Price") +
#'        theme_tq()
#'
#' @name theme_tq
NULL

#' @rdname theme_tq
#' @export
theme_tq <- function(base_size = 11, base_family = "") {

    # Tidyquant colors
    blue  <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey  <- "grey80"

    # Starts with theme_grey and then modify some parts
    theme_grey(base_size = base_size, base_family = base_family) %+replace%
        theme(

            # Base Inherited Elements
            line               =  element_line(colour = blue, size = 0.5, linetype = 1,
                                               lineend = "butt"),
            rect               =  element_rect(fill = white, colour = blue,
                                               size = 0.5, linetype = 1),
            text               =  element_text(family = base_family, face = "plain",
                                               colour = blue, size = base_size,
                                               lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
                                               margin = margin(), debug = FALSE),

            # Axes
            axis.line          = element_blank(),
            axis.text          = element_text(size = rel(0.8)),
            axis.ticks         = element_line(color = grey, size = rel(1/3)),
            axis.title         = element_text(size = rel(1.0)),

            # Panel
            panel.background   = element_rect(fill = white, color = NA),
            panel.border       = element_rect(fill = NA, size = rel(1/2), color = blue),
            panel.grid.major   = element_line(color = grey, size = rel(1/3)),
            panel.grid.minor   = element_line(color = grey, size = rel(1/3)),
            panel.grid.minor.x = element_blank(),
            panel.spacing      = unit(.75, "cm"),

            # Legend
            legend.key         = element_rect(fill = white, color = NA),
            legend.position    = "bottom",

            # Strip (Used with multiple panels)
            strip.background   = element_rect(fill = blue, color = blue),
            strip.text         = element_text(color = white, size = rel(0.8)),

            # Plot
            plot.title         = element_text(size = rel(1.2), hjust = 0,
                                              margin = margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")),
            plot.subtitle      = element_text(size = rel(0.9), hjust = 0,
                                              margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")),

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
        theme(

            # Axes
            axis.ticks         = element_line(color = blue, size = rel(1/3)),

            # Panel
            panel.background   = element_rect(fill = grey, color = NA),
            panel.grid.major   = element_line(color = white, size = rel(1/3)),
            panel.grid.minor   = element_line(color = white, size = rel(1/3)),

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
        theme(

            # Axes
            axis.ticks         = element_line(color = blue, size = rel(1/3)),

            # Panel
            panel.background   = element_rect(fill = green, color = NA),
            panel.grid.major   = element_line(color = white, size = rel(1/3)),
            panel.grid.minor   = element_line(color = white, size = rel(1/3)),

            # Complete theme
            complete = TRUE
        )
}
