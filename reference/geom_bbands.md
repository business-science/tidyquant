# Plot Bollinger Bands using Moving Averages

Bollinger Bands plot a range around a moving average typically two
standard deviations up and down. The `geom_bbands()` function enables
plotting Bollinger Bands quickly using various moving average functions.
The moving average functions used are specified in
[`TTR::SMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) from the
TTR package. Use
[`coord_x_date()`](https://business-science.github.io/tidyquant/reference/coord_x_date.md)
to zoom into specific plot regions. The following moving averages are
available:

- **[Simple moving averages
  (SMA)](https://www.investopedia.com/terms/s/sma.asp)**: Rolling mean
  over a period defined by `n`.

- **[Exponential moving averages
  (EMA)](https://www.investopedia.com/terms/e/ema.asp)**: Includes
  exponentially-weighted mean that gives more weight to recent
  observations. Uses `wilder` and `ratio` args.

- **[Weighted moving averages
  (WMA)](https://www.investopedia.com/ask/answers/071414/whats-difference-between-moving-average-and-weighted-moving-average.asp)**:
  Uses a set of weights, `wts`, to weight observations in the moving
  average.

- **[Double exponential moving averages
  (DEMA)](https://www.investopedia.com/articles/trading/10/double-exponential-moving-average.asp)**:
  Uses `v` volume factor, `wilder` and `ratio` args.

- **Zero-lag exponential moving averages (ZLEMA)**: Uses `wilder` and
  `ratio` args.

- **[Volume-weighted moving averages
  (VWMA)](https://www.investopedia.com/articles/trading/11/trading-with-vwap-mvwap.asp)**:
  Requires `volume` aesthetic.

- **[Elastic, volume-weighted moving averages
  (EVWMA)](https://docs.motivewave.com/studies/e-f#elastic-volume-weighted-moving-average)**:
  Requires `volume` aesthetic.

## Usage

``` r
geom_bbands(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ma_fun = SMA,
  n = 20,
  sd = 2,
  wilder = FALSE,
  ratio = NULL,
  v = 1,
  wts = 1:n,
  color_ma = "darkblue",
  color_bands = "red",
  alpha = 0.15,
  fill = "grey20",
  ...
)

geom_bbands_(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ma_fun = "SMA",
  n = 10,
  sd = 2,
  wilder = FALSE,
  ratio = NULL,
  v = 1,
  wts = 1:n,
  color_ma = "darkblue",
  color_bands = "red",
  alpha = 0.15,
  fill = "grey20",
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  or
  [`ggplot2::aes_()`](https://ggplot2.tidyverse.org/reference/aes_.html).
  If specified and `inherit.aes = TRUE` (the default), it is combined
  with the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame.`, and will be used as the layer
  data.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- na.rm:

  If `TRUE`, silently removes `NA` values, which typically desired for
  moving averages.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behavior from the default
  plot specification, e.g.
  [`ggplot2::borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ma_fun:

  The function used to calculate the moving average. Seven options are
  available including: SMA, EMA, WMA, DEMA, ZLEMA, VWMA, and EVWMA. The
  default is `SMA`. See
  [`TTR::SMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  underlying functions.

- n:

  Number of periods to average over. Must be between 1 and `nrow(x)`,
  inclusive.

- sd:

  The number of standard deviations to use.

- wilder:

  logical; if `TRUE`, a Welles Wilder type EMA will be calculated; see
  notes.

- ratio:

  A smoothing/decay ratio. `ratio` overrides `wilder` in `EMA`.

- v:

  The 'volume factor' (a number in \[0,1\]). See Notes.

- wts:

  Vector of weights. Length of `wts` vector must equal the length of
  `x`, or `n` (the default).

- color_ma, color_bands:

  Select the line color to be applied for the moving average line and
  the Bollinger band line.

- alpha:

  Used to adjust the alpha transparency for the BBand ribbon.

- fill:

  Used to adjust the fill color for the BBand ribbon.

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  These are often aesthetics, used to set an aesthetic to a fixed value,
  like `color = "red"` or `size = 3`. They may also be parameters to the
  paired geom/stat.

## Aesthetics

The following aesthetics are understood (required are in bold):

- **`x`**, Typically a date

- **`high`**, Required to be the high price

- **`low`**, Required to be the low price

- **`close`**, Required to be the close price

- `volume`, Required for VWMA and EVWMA

- `colour`, Affects line colors

- `fill`, Affects ribbon fill color

- `alpha`, Affects ribbon alpha value

- `group`

- `linetype`

- `size`

## See also

See individual modeling functions for underlying parameters:

- [`TTR::SMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  simple moving averages

- [`TTR::EMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  exponential moving averages

- [`TTR::WMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  weighted moving averages

- [`TTR::DEMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  double exponential moving averages

- [`TTR::ZLEMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  zero-lag exponential moving averages

- [`TTR::VWMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  volume-weighted moving averages

- [`TTR::EVWMA()`](https://rdrr.io/pkg/TTR/man/MovingAverages.html) for
  elastic, volume-weighted moving averages

- [`coord_x_date()`](https://business-science.github.io/tidyquant/reference/coord_x_date.md)
  for zooming into specific regions of a plot

## Examples

``` r
library(dplyr)
library(ggplot2)
library(lubridate)

AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# SMA
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +           # Plot stock price
    geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 50) +
    coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
                 ylim = c(20, 35))



# EMA
AAPL %>%
   ggplot(aes(x = date, y = close)) +
   geom_line() +           # Plot stock price
   geom_bbands(aes(high = high, low = low, close = close),
                  ma_fun = EMA, wilder = TRUE, ratio = NULL, n = 50) +
   coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
                ylim = c(20, 35))



# VWMA
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_line() +           # Plot stock price
    geom_bbands(aes(high = high, low = low, close = close, volume = volume),
                   ma_fun = VWMA, n = 50) +
    coord_x_date(xlim = c(as_date("2016-12-31") - dyears(1), as_date("2016-12-31")),
                ylim = c(20, 35))
```
