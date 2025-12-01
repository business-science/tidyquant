# Plot moving averages

The underlying moving average functions used are specified in
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

- **[Zero-lag exponential moving averages
  (ZLEMA)](https://en.wikipedia.org/wiki/Zero_lag_exponential_moving_average)**:
  Uses `wilder` and `ratio` args.

- **[Volume-weighted moving averages
  (VWMA)](https://www.investopedia.com/articles/trading/11/trading-with-vwap-mvwap.asp)**:
  Requires `volume` aesthetic.

- **[Elastic, volume-weighted moving averages
  (EVWMA)](https://www.motivewave.com/studies/elastic_volume_weighted_moving_average.htm)**:
  Requires `volume` aesthetic.

## Usage

``` r
geom_ma(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ma_fun = SMA,
  n = 20,
  wilder = FALSE,
  ratio = NULL,
  v = 1,
  wts = 1:n,
  ...
)

geom_ma_(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  ma_fun = "SMA",
  n = 20,
  wilder = FALSE,
  ratio = NULL,
  v = 1,
  wts = 1:n,
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
  vector to finely select the aesthetics to display.

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

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  These are often aesthetics, used to set an aesthetic to a fixed value,
  like `color = "red"` or `size = 3`. They may also be parameters to the
  paired geom/stat.

## Aesthetics

The following aesthetics are understood (required are in bold):

- **`x`**

- **`y`**

- `volume`, Required for VWMA and EVWMA

- `alpha`

- `colour`

- `group`

- `linetype`

- `linewidth`

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

AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# SMA
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                         # Plot stock price
    geom_ma(ma_fun = SMA, n = 50) +                 # Plot 50-day SMA
    geom_ma(ma_fun = SMA, n = 200, color = "red") + # Plot 200-day SMA
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(20, 30))                     # Zoom in


# EVWMA
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                                                   # Plot stock price
    geom_ma(aes(volume = volume), ma_fun = EVWMA, n = 50) +   # Plot 50-day EVWMA
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(20, 30))                                  # Zoom in
```
