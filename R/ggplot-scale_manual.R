#' tidyquant colors and fills for ggplot2.
#'
#' The tidyquant scales add colors that work nicely with `theme_tq()`.
#'
#' @details
#' \describe{
#'
#' \item{`scale_color_tq`}{
#' For use when `color` is specified as an `aes()` in a ggplot.}
#'
#' \item{`scale_fill_tq`}{
#' For use when `fill` is specified as an `aes()` in a ggplot.}
#' }
#'
#'
#'
#' @seealso [theme_tq()]
#'
#' @param theme one of "light", "dark", or "green". This should match the `theme_tq()` that is used with it.
#' @param ... common discrete scale parameters: `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`. See [discrete_scale()] for more details
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#' library(dplyr)
#' library(ggplot2)
#'
#' # Get stock prices
#' stocks <- c("AAPL", "FB", "NFLX") %>%
#'     tq_get(from = "2013-01-01",
#'            to   = "2017-01-01")
#'
#' # Plot for stocks
#' g <- stocks %>%
#'     ggplot(aes(date, adjusted, color = symbol)) +
#'     geom_line() +
#'     labs(title = "Multi stock example",
#'          xlab = "Date",
#'          ylab = "Adjusted Close")
#'
#' # Plot with tidyquant theme and colors
#' g +
#'     theme_tq() +
#'     scale_color_tq()
#'
#'
#' @name scale_manual
NULL

#' @rdname scale_manual
#' @export
#'
scale_color_tq <- function(..., theme = "light") {

  pal <- switch(theme,
         "light" = unname(palette_light()) %>% rep(100),
         "dark"  = unname(palette_dark()) %>% rep(100),
         "green" = unname(palette_green() %>% rep(100))
         )

  scale_color_manual(values = pal)
}

#' @rdname scale_manual
#' @export
scale_colour_tq <- scale_color_tq

#' @rdname scale_manual
#' @export
scale_fill_tq <- function(..., theme = "light") {

  pal <- switch(theme,
                "light" = unname(palette_light()) %>% rep(100),
                "dark"  = unname(palette_dark()) %>% rep(100),
                "green" = unname(palette_green()) %>% rep(100)
  )

  scale_fill_manual(values = pal)
}

#' tidyquant palettes for use with scales
#'
#' These palettes are mainly called internally by tidyquant `scale_*_tq()` functions.
#'
#' @examples
#' library(scales)
#' scales::show_col(palette_light())
#'
#' @name palette_tq
NULL

#' @rdname palette_tq
#' @export
palette_light <- function() {
  c(
    blue         = "#2c3e50", # blue
    red          = "#e31a1c", # red
    green        = "#18BC9C", # green
    yellow       = "#CCBE93", # yellow
    steel_blue   = "#a6cee3", # steel_blue
    navy_blue    = "#1f78b4", # navy_blue
    light_green  = "#b2df8a", # light_green
    pink         = "#fb9a99", # pink
    light_orange = "#fdbf6f", # light_orange
    orange       = "#ff7f00", # orange
    light_purple = "#cab2d6", # light_purple
    purple       = "#6a3d9a"  # purple
  ) %>% toupper()
}

#' @rdname palette_tq
#' @export
palette_dark <- function() {
  # Brighter version of palette_light
  c(
    blue         = "#0055AA", # blue
    red          = "#C40003", # red
    green        = "#00C19B", # green
    yellow       = "#EAC862", # yellow
    steel_blue   = "#7FD2FF", # steel_blue
    navy_blue    = "#007ED3", # navy_blue
    light_green  = "#b2df8a", # light_green
    pink         = "#FFACAA", # pink
    light_orange = "#FF9D1E", # light_orange
    lime_green   = "#C3EF00", # lime_green
    light_purple = "#cab2d6", # light_purple
    purple       = "#894FC6"  # purple
  ) %>% toupper()
}

#' @rdname palette_tq
#' @export
palette_green <- function() {
  # Green compatible version of palette_light
  c(
    blue         = "#0055AA", # blue
    red          = "#C40003", # red
    yellow       = "#EAC862", # yellow
    steel_blue   = "#7FD2FF", # steel_blue
    navy_blue    = "#007ED3", # navy_blue
    creme        = "#F6F4F3", # creme
    pink         = "#FFACAA", # pink
    light_orange = "#FF9D1E", # light_orange
    lime_green   = "#C3EF00", # lime_green
    light_purple = "#cab2d6", # light_purple
    purple       = "#894FC6", # purple
    brown        = "#592E2E"  # brown
  ) %>% toupper()
}
