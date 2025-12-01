# tidyquant colors and fills for ggplot2.

The tidyquant scales add colors that work nicely with
[`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md).

## Usage

``` r
scale_color_tq(..., theme = "light")

scale_colour_tq(..., theme = "light")

scale_fill_tq(..., theme = "light")
```

## Arguments

- ...:

  common parameters for
  [`scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  or
  [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html):
  `name`, `breaks`, `labels`, `na.value`, `limits` and `guide`.

- theme:

  one of "light", "dark", or "green". This should match the
  [`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md)
  that is used with it.

## Details

- `scale_color_tq`:

  For use when `color` is specified as an
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) in a
  ggplot.

- `scale_fill_tq`:

  For use when `fill` is specified as an
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) in a
  ggplot.

## See also

[`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md)

## Examples

``` r
# Load libraries
library(dplyr)
library(ggplot2)

# Get stock prices
stocks <- c("AAPL", "META", "NFLX") %>%
    tq_get(from = "2013-01-01",
           to   = "2017-01-01")

# Plot for stocks
g <- stocks %>%
    ggplot(aes(date, adjusted, color = symbol)) +
    geom_line() +
    labs(title = "Multi stock example",
         xlab = "Date",
         ylab = "Adjusted Close")

# Plot with tidyquant theme and colors
g +
    theme_tq() +
    scale_color_tq()
#> Ignoring unknown labels:
#> • xlab : "Date"
#> • ylab : "Adjusted Close"


```
