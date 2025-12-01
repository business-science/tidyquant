# Mutates quantitative data

`tq_mutate()` adds new variables to an existing tibble; `tq_transmute()`
returns only newly created columns and is typically used when
periodicity changes

## Usage

``` r
tq_mutate(
  data,
  select = NULL,
  mutate_fun,
  col_rename = NULL,
  ohlc_fun = NULL,
  ...
)

tq_mutate_(data, select = NULL, mutate_fun, col_rename = NULL, ...)

tq_mutate_xy(data, x, y = NULL, mutate_fun, col_rename = NULL, ...)

tq_mutate_xy_(data, x, y = NULL, mutate_fun, col_rename = NULL, ...)

tq_mutate_fun_options()

tq_transmute(
  data,
  select = NULL,
  mutate_fun,
  col_rename = NULL,
  ohlc_fun = NULL,
  ...
)

tq_transmute_(data, select = NULL, mutate_fun, col_rename = NULL, ...)

tq_transmute_xy(data, x, y = NULL, mutate_fun, col_rename = NULL, ...)

tq_transmute_xy_(data, x, y = NULL, mutate_fun, col_rename = NULL, ...)

tq_transmute_fun_options()
```

## Arguments

- data:

  A `tibble` (tidy data frame) of data typically from
  [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md).

- select:

  The columns to send to the mutation function.

- mutate_fun:

  The mutation function from either the `xts`, `quantmod`, or `TTR`
  package. Execute `tq_mutate_fun_options()` to see the full list of
  options by package.

- col_rename:

  A string or character vector containing names that can be used to
  quickly rename columns.

- ohlc_fun:

  Deprecated. Use `select`.

- ...:

  Additional parameters passed to the appropriate mutatation function.

- x, y:

  Parameters used with `_xy` that consist of column names of variables
  to be passed to the mutatation function (instead of OHLC functions).

## Value

Returns mutated data in the form of a `tibble` object.

## Details

`tq_mutate` and `tq_transmute` are very flexible wrappers for various
`xts`, `quantmod` and `TTR` functions. The main advantage is the results
are returned as a `tibble` and the function can be used with the
`tidyverse`. `tq_mutate` is used when additional columns are added to
the return data frame. `tq_transmute` works exactly like `tq_mutate`
except it only returns the newly created columns. This is helpful when
changing periodicity where the new columns would not have the same
number of rows as the original tibble.

`select` specifies the columns that get passed to the mutation function.
Select works as a more flexible version of the OHLC extractor functions
from `quantmod` where non-OHLC data works as well. When `select` is
`NULL`, all columns are selected. In Example 1 below, `close` returns
the "close" price and sends this to the mutate function, `periodReturn`.

`mutate_fun` is the function that performs the work. In Example 1, this
is `periodReturn`, which calculates the period returns. The `...` are
additional arguments passed to the `mutate_fun`. Think of the whole
operation in Example 1 as the close price, obtained by `select = close`,
being sent to the `periodReturn` function along with additional
arguments defining how to perform the period return, which includes
`period = "daily"` and `type = "log"`. Example 4 shows how to apply a
rolling regression.

`tq_mutate_xy` and `tq_transmute_xy` are designed to enable working with
mutatation functions that require two primary inputs (e.g. EVWMA, VWAP,
etc). Example 2 shows this benefit in action: using the EVWMA function
that uses volume to define the moving average period.

`tq_mutate_`, `tq_mutate_xy_`, `tq_transmute_`, and `tq_transmute_xy_`
are setup for Non-Standard Evaluation (NSE). This enables
programatically changing column names by modifying the text
representations. Example 5 shows the difference in implementation. Note
that character strings are being passed to the variables instead of
unquoted variable names. See `vignette("nse")` for more information.

`tq_mutate_fun_options` and `tq_transmute_fun_options` return a list of
various financial functions that are compatible with `tq_mutate` and
`tq_transmute`, respectively.

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)

## Examples

``` r
# Load libraries
library(dplyr)

##### Basic Functionality

fb_stock_prices  <- tidyquant::FANG %>%
    filter(symbol == "META") %>%
        filter(
            date >= "2016-01-01",
            date <= "2016-12-31"
        )

goog_stock_prices  <- FANG %>%
    filter(symbol == "GOOG") %>%
        filter(
            date >= "2016-01-01",
            date <= "2016-12-31"
        )

# Example 1: Return logarithmic daily returns using periodReturn()
fb_stock_prices %>%
    tq_mutate(select = close, mutate_fun = periodReturn,
              period = "daily", type = "log")
#> # A tibble: 252 × 9
#>    symbol date        open  high   low close   volume adjusted daily.returns
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>         <dbl>
#>  1 META   2016-01-04 102.  102.   99.8 102.  37912400    102.        0      
#>  2 META   2016-01-05 103.  104.  102.  103.  23258200    103.        0.00498
#>  3 META   2016-01-06 101.  104.  101.  103.  25096200    103.        0.00233
#>  4 META   2016-01-07 100.  101.   97.3  97.9 45172900     97.9      -0.0503 
#>  5 META   2016-01-08  99.9 100.   97.0  97.3 35402300     97.3      -0.00604
#>  6 META   2016-01-11  97.9  98.6  95.4  97.5 29932400     97.5       0.00185
#>  7 META   2016-01-12  99   100.0  97.6  99.4 28395400     99.4       0.0189 
#>  8 META   2016-01-13 101.  101.   95.2  95.4 33410600     95.4      -0.0404 
#>  9 META   2016-01-14  95.8  98.9  92.4  98.4 48658600     98.4       0.0302 
#> 10 META   2016-01-15  94.0  96.4  93.5  95.0 45935600     95.0      -0.0352 
#> # ℹ 242 more rows

# Example 2: Use tq_mutate_xy to use functions with two columns required
fb_stock_prices %>%
    tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA,
                 col_rename = "EVWMA")
#> # A tibble: 252 × 9
#>    symbol date        open  high   low close   volume adjusted EVWMA
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl> <dbl>
#>  1 META   2016-01-04 102.  102.   99.8 102.  37912400    102.   NA  
#>  2 META   2016-01-05 103.  104.  102.  103.  23258200    103.   NA  
#>  3 META   2016-01-06 101.  104.  101.  103.  25096200    103.   NA  
#>  4 META   2016-01-07 100.  101.   97.3  97.9 45172900     97.9  NA  
#>  5 META   2016-01-08  99.9 100.   97.0  97.3 35402300     97.3  NA  
#>  6 META   2016-01-11  97.9  98.6  95.4  97.5 29932400     97.5  NA  
#>  7 META   2016-01-12  99   100.0  97.6  99.4 28395400     99.4  NA  
#>  8 META   2016-01-13 101.  101.   95.2  95.4 33410600     95.4  NA  
#>  9 META   2016-01-14  95.8  98.9  92.4  98.4 48658600     98.4  NA  
#> 10 META   2016-01-15  94.0  96.4  93.5  95.0 45935600     95.0  95.0
#> # ℹ 242 more rows

# Example 3: Using tq_mutate to work with non-OHLC data
tq_get("DCOILWTICO", get = "economic.data") %>%
    tq_mutate(select = price, mutate_fun = lag.xts, k = 1, na.pad = TRUE)
#> # A tibble: 2,843 × 4
#>    symbol     date       price lag.xts
#>    <chr>      <date>     <dbl>   <dbl>
#>  1 DCOILWTICO 2015-01-01  NA      NA  
#>  2 DCOILWTICO 2015-01-02  52.7    NA  
#>  3 DCOILWTICO 2015-01-05  50.0    52.7
#>  4 DCOILWTICO 2015-01-06  48.0    50.0
#>  5 DCOILWTICO 2015-01-07  48.7    48.0
#>  6 DCOILWTICO 2015-01-08  48.8    48.7
#>  7 DCOILWTICO 2015-01-09  48.4    48.8
#>  8 DCOILWTICO 2015-01-12  46.1    48.4
#>  9 DCOILWTICO 2015-01-13  45.9    46.1
#> 10 DCOILWTICO 2015-01-14  48.5    45.9
#> # ℹ 2,833 more rows

# Example 4: Using tq_mutate to apply a rolling regression
fb_returns <- fb_stock_prices %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "fb.returns")
goog_returns <- goog_stock_prices %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "goog.returns")
returns_combined <- left_join(fb_returns, goog_returns, by = "date")
regr_fun <- function(data) {
    coef(lm(fb.returns ~ goog.returns, data = as_tibble(data)))
}
returns_combined %>%
    tq_mutate(mutate_fun = rollapply,
              width      = 6,
              FUN        = regr_fun,
              by.column  = FALSE,
              col_rename = c("coef.0", "coef.1"))
#> # A tibble: 12 × 5
#>    date       fb.returns goog.returns   coef.0 coef.1
#>    <date>          <dbl>        <dbl>    <dbl>  <dbl>
#>  1 2016-01-29     0.0977      0.00150 NA       NA    
#>  2 2016-02-29    -0.0471     -0.0608  NA       NA    
#>  3 2016-03-31     0.0672      0.0676  NA       NA    
#>  4 2016-04-29     0.0305     -0.0697  NA       NA    
#>  5 2016-05-31     0.0105      0.0616  NA       NA    
#>  6 2016-06-30    -0.0381     -0.0593   0.0248   0.479
#>  7 2016-07-29     0.0845      0.111    0.0136   0.516
#>  8 2016-08-31     0.0176     -0.00226  0.0210   0.426
#>  9 2016-09-30     0.0170      0.0133   0.0169   0.382
#> 10 2016-10-31     0.0212      0.00933  0.00541  0.601
#> 11 2016-11-30    -0.0960     -0.0338  -0.00466  0.897
#> 12 2016-12-30    -0.0285      0.0182  -0.0172   1.03 

# Example 5: Non-standard evaluation:
# Programming with tq_mutate_() and tq_mutate_xy_()
col_name <- "adjusted"
mutate <- c("MACD", "SMA")
tq_mutate_xy_(fb_stock_prices, x = col_name, mutate_fun = mutate[[1]])
#> # A tibble: 252 × 10
#>    symbol date        open  high   low close   volume adjusted  macd signal
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl> <dbl>  <dbl>
#>  1 META   2016-01-04 102.  102.   99.8 102.  37912400    102.     NA     NA
#>  2 META   2016-01-05 103.  104.  102.  103.  23258200    103.     NA     NA
#>  3 META   2016-01-06 101.  104.  101.  103.  25096200    103.     NA     NA
#>  4 META   2016-01-07 100.  101.   97.3  97.9 45172900     97.9    NA     NA
#>  5 META   2016-01-08  99.9 100.   97.0  97.3 35402300     97.3    NA     NA
#>  6 META   2016-01-11  97.9  98.6  95.4  97.5 29932400     97.5    NA     NA
#>  7 META   2016-01-12  99   100.0  97.6  99.4 28395400     99.4    NA     NA
#>  8 META   2016-01-13 101.  101.   95.2  95.4 33410600     95.4    NA     NA
#>  9 META   2016-01-14  95.8  98.9  92.4  98.4 48658600     98.4    NA     NA
#> 10 META   2016-01-15  94.0  96.4  93.5  95.0 45935600     95.0    NA     NA
#> # ℹ 242 more rows
```
