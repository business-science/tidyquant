# Deprecated functions

A record of functions that have been deprecated.

## Usage

``` r
tq_transform(data, ohlc_fun = OHLCV, mutate_fun, col_rename = NULL, ...)

tq_transform_xy(data, x, y = NULL, mutate_fun, col_rename = NULL, ...)
```

## Arguments

- data:

  A `tibble` (tidy data frame) of data typically from
  [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md).

- ohlc_fun:

  Deprecated. Use `select`.

- mutate_fun:

  The mutation function from either the `xts`, `quantmod`, or `TTR`
  package. Execute
  [`tq_mutate_fun_options()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  to see the full list of options by package.

- col_rename:

  A string or character vector containing names that can be used to
  quickly rename columns.

- ...:

  Additional parameters passed to the appropriate mutatation function.

- x, y:

  Parameters used with `_xy` that consist of column names of variables
  to be passed to the mutatation function (instead of OHLC functions).

## Details

- `tq_transform()` - use
  [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)

- `tq_transform_xy()` - use
  [`tq_transmute_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)

- `as_xts()` - use
  [`timetk::tk_xts()`](https://business-science.github.io/timetk/reference/tk_xts.html)

- [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html) -
  use
  [`timetk::tk_tbl()`](https://business-science.github.io/timetk/reference/tk_tbl.html)

- `summarise_by_time()` - Moved to `timetk` package. Use
  [`timetk::summarise_by_time()`](https://business-science.github.io/timetk/reference/summarise_by_time.html)
