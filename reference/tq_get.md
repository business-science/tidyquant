# Get quantitative data in `tibble` format

Get quantitative data in `tibble` format

## Usage

``` r
tq_get(x, get = "stock.prices", complete_cases = TRUE, ...)

tq_get_options()
```

## Arguments

- x:

  A single character string, a character vector or tibble representing a
  single (or multiple) stock symbol, metal symbol, currency combination,
  FRED code, etc.

- get:

  A character string representing the type of data to get for `x`.
  Options include:

  - `"stock.prices"`: Get the open, high, low, close, volume and
    adjusted stock prices for a stock symbol from Yahoo Finance
    (https://finance.yahoo.com/). Wrapper for
    [`quantmod::getSymbols()`](https://rdrr.io/pkg/quantmod/man/getSymbols.html).

  - `"dividends"`: Get the dividends for a stock symbol from Yahoo
    Finance (https://finance.yahoo.com/). Wrapper for
    [`quantmod::getDividends()`](https://rdrr.io/pkg/quantmod/man/getDividends.html).

  - `"splits"`: Get the split ratio for a stock symbol from Yahoo
    Finance (https://finance.yahoo.com/). Wrapper for
    [`quantmod::getSplits()`](https://rdrr.io/pkg/quantmod/man/getSplits.html).

  - `"stock.prices.japan"`: Get the open, high, low, close, volume and
    adjusted stock prices for a stock symbol from Yahoo Finance Japan.
    Wrapper for
    [`quantmod::getSymbols.yahooj()`](https://rdrr.io/pkg/quantmod/man/getSymbols.yahooj.html).

  - `"economic.data"`: Get economic data from
    [FRED](https://fred.stlouisfed.org/). rapper for
    [`quantmod::getSymbols.FRED()`](https://rdrr.io/pkg/quantmod/man/getSymbols.FRED.html).

  - `"quandl"`: Get data sets from [Nasdaq Data
    Link](https://data.nasdaq.com/). Wrapper for tidyquant's built-in
    Data Link client. See also
    [`quandl_api_key()`](https://business-science.github.io/tidyquant/reference/quandl_api_key.md).

  - `"quandl.datatable"`: Get data tables from [Nasdaq Data
    Link](https://data.nasdaq.com/). Wrapper for tidyquant's built-in
    Data Link datatable client. See also
    [`quandl_api_key()`](https://business-science.github.io/tidyquant/reference/quandl_api_key.md).

  - `"tiingo"`: Get data sets from Tingo (https://www.tiingo.com/).
    Wrapper for
    [`riingo::riingo_prices()`](https://rdrr.io/pkg/riingo/man/riingo_prices.html).
    See also
    [`tiingo_api_key()`](https://business-science.github.io/tidyquant/reference/tiingo_api_key.md).

  - `"tiingo.iex"`: Get data sets from Tingo (https://www.tiingo.com/).
    Wrapper for
    [`riingo::riingo_iex_prices()`](https://rdrr.io/pkg/riingo/man/riingo_iex_prices.html).
    See also
    [`tiingo_api_key()`](https://business-science.github.io/tidyquant/reference/tiingo_api_key.md).

  - `"tiingo.crypto"`: Get data sets from Tingo
    (https://www.tiingo.com/). Wrapper for
    [`riingo::riingo_crypto_prices()`](https://rdrr.io/pkg/riingo/man/riingo_crypto_prices.html).
    See also
    [`tiingo_api_key()`](https://business-science.github.io/tidyquant/reference/tiingo_api_key.md).

  - `"alphavantager"`: Get data sets from [Alpha
    Vantage](https://www.alphavantage.co/). Wrapper for
    [`alphavantager::av_get()`](https://rdrr.io/pkg/alphavantager/man/av_get.html).
    See also
    [`av_api_key()`](https://business-science.github.io/tidyquant/reference/av_api_key.md).

  - `"rblpapi"`: Get data sets from
    [Bloomberg](https://www.bloomberg.com/professional/solution/bloomberg-terminal/).
    Wrapper for `Rblpapi`. See also
    [`Rblpapi::blpConnect()`](https://rdrr.io/pkg/Rblpapi/man/blpConnect.html)
    to connect to Bloomberg terminal (required). Use the argument
    `rblpapi_fun` to set the function such as "bdh" (default), "bds", or
    "bdp".

- complete_cases:

  Removes symbols that return an NA value due to an error with the get
  call such as sending an incorrect symbol "XYZ" to get =
  "stock.prices". This is useful in scaling so user does not need to add
  an extra step to remove these rows. `TRUE` by default, and a warning
  message is generated for any rows removed.

- ...:

  Additional parameters passed to the "wrapped" function. Investigate
  underlying functions to see full list of arguments. Common optional
  parameters include:

  - `from`: Standardized for time series functions in `quantmod`,
    `quandl`, `tiingo`, `alphavantager` packages. A character string
    representing a start date in YYYY-MM-DD format.

  - `to`: Standardized for time series functions in `quantmod`,
    `quandl`, `tiingo`, `alphavantager` packages. A character string
    representing a end date in YYYY-MM-DD format.

## Value

Returns data in the form of a `tibble` object.

## Details

`tq_get()` is a consolidated function that gets data from various web
sources. The function is a wrapper for several `quantmod` functions,
several web APIs, and also gets data from websources unavailable in
other packages. The results are always returned as a `tibble`. The
advantages are (1) only one function is needed for all data sources and
(2) the function can be seamlessly used with the tidyverse: `purrr`,
`tidyr`, and `dplyr` verbs.

`tq_get_options()` returns a list of valid `get` options you can choose
from.

`tq_get_stock_index_options()` Is deprecated and will be removed in the
next version. Please use
[`tq_index_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
instead.

## See also

- [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  to get a ful list of stocks in an index.

- [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  to get a ful list of stocks in an exchange.

- [`quandl_api_key()`](https://business-science.github.io/tidyquant/reference/quandl_api_key.md)
  to set the api key for collecting data via the `"quandl"` get option.

- [`tiingo_api_key()`](https://business-science.github.io/tidyquant/reference/tiingo_api_key.md)
  to set the api key for collecting data via the `"tiingo"` get option.

- [`av_api_key()`](https://business-science.github.io/tidyquant/reference/av_api_key.md)
  to set the api key for collecting data via the `"alphavantage"` get
  option.

## Examples

``` r
# Load libraries

# Get the list of `get` options
tq_get_options()
#>  [1] "stock.prices"       "stock.prices.japan" "dividends"         
#>  [4] "splits"             "economic.data"      "quandl"            
#>  [7] "quandl.datatable"   "tiingo"             "tiingo.iex"        
#> [10] "tiingo.crypto"      "alphavantager"      "alphavantage"      
#> [13] "rblpapi"           

# Get stock prices for a stock from Yahoo
aapl_stock_prices <- tq_get("AAPL")

# Get stock prices for multiple stocks
mult_stocks <- tq_get(c("META", "AMZN"),
                      get  = "stock.prices",
                      from = "2016-01-01",
                      to   = "2017-01-01")


if (FALSE) { # \dontrun{

# --- Nasdaq Data Link (formerly Quandl) ---
quandl_api_key('<your_api_key>')
tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01")


# Energy data from EIA



# --- Tiingo ---
if (rlang::is_installed("riingo")) {
tiingo_api_key('<your_api_key>')

# Tiingo Prices (Free alternative to Yahoo Finance!)
tq_get(c("AAPL", "GOOG"), get = "tiingo", from = "2010-01-01")

# Sub-daily prices from IEX ----
tq_get(c("AAPL", "GOOG"),
       get = "tiingo.iex",
       from   = "2020-01-01",
       to     = "2020-01-15",
       resample_frequency = "5min")

# Tiingo Bitcoin Prices ----
tq_get(c("btcusd", "btceur"),
       get    = "tiingo.crypto",
       from   = "2020-01-01",
       to     = "2020-01-15",
       resample_frequency = "5min")


}

# --- Alpha Vantage ---

if (rlang::is_installed("alphavantager")) {
av_api_key('<your_api_key>')

# Daily Time Series
tq_get("AAPL",
       get        = "alphavantager",
       av_fun     = "TIME_SERIES_DAILY_ADJUSTED",
       outputsize = "full")

# Intraday 15 Min Interval
tq_get("AAPL",
       get        = "alphavantage",
       av_fun     = "TIME_SERIES_INTRADAY",
       interval   = "15min",
       outputsize = "full")
# FX DAILY
tq_get("USD/EUR", get = "alphavantage", av_fun = "FX_DAILY", outputsize = "full")

# FX REAL-TIME QUOTE
tq_get("USD/EUR", get = "alphavantage", av_fun = "CURRENCY_EXCHANGE_RATE")

}
} # }
```
