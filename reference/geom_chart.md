# Plot Financial Charts in ggplot2

Financial charts provide visual cues to open, high, low, and close
prices. Use
[`coord_x_date()`](https://business-science.github.io/tidyquant/reference/coord_x_date.md)
to zoom into specific plot regions. The following financial chart geoms
are available:

- **[Bar Chart](https://www.investopedia.com/terms/b/barchart.asp)**

- **[Candlestick
  Chart](https://www.investopedia.com/terms/c/candlestick.asp)**

## Usage

``` r
geom_barchart(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  colour_up = "darkblue",
  colour_down = "red",
  fill_up = "darkblue",
  fill_down = "red",
  ...
)

geom_candlestick(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  colour_up = "darkblue",
  colour_down = "red",
  fill_up = "darkblue",
  fill_down = "red",
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

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used the override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

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

- colour_up, colour_down:

  Select colors to be applied based on price movement from open to
  close. If `close >= open`, `colour_up` is used. Otherwise,
  `colour_down` is used. The default is `"darkblue"` and `"red"`,
  respectively.

- fill_up, fill_down:

  Select fills to be applied based on price movement from open to close.
  If close \>= open, `fill_up` is used. Otherwise, `fill_down` is used.
  The default is `"darkblue"` and "red", respectively. Only affects
  `geom_candlestick()`.

- ...:

  Other arguments passed on to
  [`ggplot2::layer()`](https://ggplot2.tidyverse.org/reference/layer.html).
  These are often aesthetics, used to set an aesthetic to a fixed value,
  like `color = "red"` or `size = 3`. They may also be parameters to the
  paired geom/stat.

## Aesthetics

The following aesthetics are understood (required are in bold):

- **`x`**, Typically a date

- **`open`**, Required to be the open price

- **`high`**, Required to be the high price

- **`low`**, Required to be the low price

- **`close`**, Required to be the close price

- `alpha`

- `group`

- `linetype`

- `size`

## See also

See individual modeling functions for underlying parameters:

- [`geom_ma()`](https://business-science.github.io/tidyquant/reference/geom_ma.md)
  for adding moving averages to ggplots

- [`geom_bbands()`](https://business-science.github.io/tidyquant/reference/geom_bbands.md)
  for adding Bollinger Bands to ggplots

- [`coord_x_date()`](https://business-science.github.io/tidyquant/reference/coord_x_date.md)
  for zooming into specific regions of a plot

## Examples

``` r
library(dplyr)
library(ggplot2)
library(lubridate)

AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")

# Bar Chart
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_barchart(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(color = "darkgreen") +
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(20, 30))


# Candlestick Chart
AAPL %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
    geom_ma(color = "darkgreen") +
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"),
                 ylim = c(20, 30))
```
