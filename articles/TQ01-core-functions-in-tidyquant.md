# Core Functions in tidyquant

> A few core functions with a lot of power

## Overview

The `tidyquant` package has a **few core functions with a lot of
power**. Few functions means less of a learning curve for the user,
which is why there are only a handful of functions the user needs to
learn to perform the vast majority of financial analysis tasks. The main
functions are:

- **Get a Stock Index,
  [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md),
  or a Stock Exchange,
  [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md)**:
  Returns the stock symbols and various attributes for every stock in an
  index or exchange. Eighteen indexes and three exchanges are available.

- **Get Quantitative Data,
  [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)**:
  A one-stop shop to get data from various web-sources.

- **Transmute,
  [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md),
  and Mutate,
  [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md),
  Quantitative Data**: Perform and scale financial calculations
  completely within the `tidyverse`. These workhorse functions integrate
  the `xts`, `zoo`, `quantmod`, and `TTR` packages.

- **Performance analysis,
  [`tq_performance()`](https://business-science.github.io/tidyquant/reference/tq_performance.md),
  and portfolio aggregation,
  [`tq_portfolio()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)**:
  The `PerformanceAnalytics` integration enables analyzing performance
  of assets and portfolios. Because of the breadth of this topic, refer
  to [Performance Analysis with
  tidyquant](https://business-science.github.io/tidyquant/articles/TQ05-performance-analysis-with-tidyquant.md)
  for a tutorial on these functions.

## Prerequisites

Load the `tidyquant` package to get started.

``` r
# Loads tidyquant, lubridate, xts, quantmod, TTR 
library(tidyverse)
library(tidyquant)
```

## 1.0 Retrieve Consolidated Symbol Data

### 1.1 Stock Indexes

A wide range of stock index / exchange lists can be retrieved using
[`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md).
To get a full list of the options, use
[`tq_index_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md).

``` r
tq_index_options()
```

    ## [1] "DOW"       "DOWGLOBAL" "SP400"     "SP500"     "SP600"

Set `x` as one of the options in the list of options above to get the
desired stock index / exchange.

``` r
tq_index("SP500")
```

The data source is State Street Global Advisors.

### 1.2 Stock Exchanges

Stock lists for three stock exchanges are available: NASDAQ, NYSE, and
AMEX. If you forget, just use
[`tq_exchange_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md).
We can easily get the full list of stocks on the NASDAQ exchange.

``` r
tq_exchange("NASDAQ")
```

## 1.0 Get Quantitative Data

The
[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
function is used to collect data by changing the `get` argument. The
data sources:

1.  **Yahoo Finance** - Daily stock data
2.  **FRED** - Economic data
3.  **Quandl** - Economic, Energy, & Financial Data API
4.  **Tiingo** - Financial API with sub-daily stock data and
    crypto-currency
5.  **Alpha Vantage** - Financial API with sub-daily, ForEx, and
    crypto-currency data
6.  **Bloomberg** - Financial API. Paid account is required.

Use
[`tq_get_options()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
to see the full list.

``` r
tq_get_options()
```

    ##  [1] "stock.prices"       "stock.prices.japan" "dividends"         
    ##  [4] "splits"             "economic.data"      "quandl"            
    ##  [7] "quandl.datatable"   "tiingo"             "tiingo.iex"        
    ## [10] "tiingo.crypto"      "alphavantager"      "alphavantage"      
    ## [13] "rblpapi"

### 2.1 Yahoo! Finance

The stock prices can be retrieved succinctly using
`get = "stock.prices"`. This returns stock price data from Yahoo
Finance.

``` r
aapl_prices  <- tq_get("AAPL", get = "stock.prices", from = " 1990-01-01")
aapl_prices 
```

    ## # A tibble: 9,116 × 8
    ##    symbol date        open  high   low close    volume adjusted
    ##    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
    ##  1 AAPL   1990-01-02 0.315 0.335 0.312 0.333 183198400    0.261
    ##  2 AAPL   1990-01-03 0.339 0.339 0.335 0.335 207995200    0.262
    ##  3 AAPL   1990-01-04 0.342 0.346 0.333 0.336 221513600    0.263
    ##  4 AAPL   1990-01-05 0.337 0.342 0.330 0.337 123312000    0.264
    ##  5 AAPL   1990-01-08 0.335 0.339 0.330 0.339 101572800    0.266
    ##  6 AAPL   1990-01-09 0.339 0.339 0.330 0.336  86139200    0.263
    ##  7 AAPL   1990-01-10 0.336 0.336 0.319 0.321 199718400    0.252
    ##  8 AAPL   1990-01-11 0.324 0.324 0.308 0.308 211052800    0.241
    ##  9 AAPL   1990-01-12 0.306 0.310 0.301 0.308 171897600    0.241
    ## 10 AAPL   1990-01-15 0.308 0.319 0.306 0.306 161739200    0.240
    ## # ℹ 9,106 more rows

We can get multiple stocks:

``` r
stocks <- c("AAPL", "META", "NFLX") %>%
    tq_get(from = "2013-01-01",
           to   = "2017-01-01")
stocks
```

    ## # A tibble: 3,024 × 8
    ##    symbol date        open  high   low close    volume adjusted
    ##    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
    ##  1 AAPL   2013-01-02  19.8  19.8  19.3  19.6 560518000     16.6
    ##  2 AAPL   2013-01-03  19.6  19.6  19.3  19.4 352965200     16.4
    ##  3 AAPL   2013-01-04  19.2  19.2  18.8  18.8 594333600     15.9
    ##  4 AAPL   2013-01-07  18.6  18.9  18.4  18.7 484156400     15.8
    ##  5 AAPL   2013-01-08  18.9  19.0  18.6  18.8 458707200     15.9
    ##  6 AAPL   2013-01-09  18.7  18.8  18.4  18.5 407604400     15.6
    ##  7 AAPL   2013-01-10  18.9  18.9  18.4  18.7 601146000     15.8
    ##  8 AAPL   2013-01-11  18.6  18.8  18.5  18.6 350506800     15.7
    ##  9 AAPL   2013-01-14  18.0  18.1  17.8  17.9 734207600     15.2
    ## 10 AAPL   2013-01-15  17.8  17.8  17.3  17.4 876772400     14.7
    ## # ℹ 3,014 more rows

Yahoo Japan stock prices can be retrieved using a similar call,
`get = "stock.prices.japan"`.

``` r
x8411T <- tq_get("8411.T", get = "stock.prices.japan", from = "2016-01-01", to  = "2016-12-31")
```

The data source is Yahoo Finance (<https://finance.yahoo.com/>) and
Yahoo Finance Japan.

### 2.2 FRED Economic Data

A wealth of economic data can be extracted from the Federal Reserve
Economic Data (FRED) database. The FRED contains over 10K data sets that
are free to use. See the [FRED
categories](https://fred.stlouisfed.org/categories) to narrow down the
data base and to get data codes. The [WTI Crude Oil
Prices](https://fred.stlouisfed.org/series/DCOILWTICO) are shown below.

``` r
wti_price_usd <- tq_get("DCOILWTICO", get = "economic.data")
wti_price_usd 
```

    ## # A tibble: 2,657 × 3
    ##    symbol     date       price
    ##    <chr>      <date>     <dbl>
    ##  1 DCOILWTICO 2016-01-01  NA  
    ##  2 DCOILWTICO 2016-01-04  36.8
    ##  3 DCOILWTICO 2016-01-05  36.0
    ##  4 DCOILWTICO 2016-01-06  34.0
    ##  5 DCOILWTICO 2016-01-07  33.3
    ##  6 DCOILWTICO 2016-01-08  33.2
    ##  7 DCOILWTICO 2016-01-11  31.4
    ##  8 DCOILWTICO 2016-01-12  30.4
    ##  9 DCOILWTICO 2016-01-13  30.4
    ## 10 DCOILWTICO 2016-01-14  31.2
    ## # ℹ 2,647 more rows

### 2.3 Nasdaq Data Link (Quandl) API

[Quandl](https://data.nasdaq.com/) provides access to a vast number of
financial and economic databases. The Quandl packages must be installed
separately.

``` r
install.packages("Quandl")
```

#### Authentication

To make full use of the integration we recommend you set your api key.
To do this create or sign into your Quandl account and go to your
account api key page.

``` r
quandl_api_key("<your-api-key>")
```

#### Search

Searching Quandl from within the R console is possible with
[`quandl_search()`](https://business-science.github.io/tidyquant/reference/quandl_search.md),
a wrapper for
[`Quandl::Quandl.search()`](https://rdrr.io/pkg/Quandl/man/Quandl.search.html).
An example search is shown below. The only required argument is `query`.
You can also visit the [Quandl Search](https://data.nasdaq.com/search)
webpage to search for available database codes.

``` r
quandl_search(query = "Oil", database_code = "NSE", per_page = 3)
```

#### Getting Quandl Data

Getting data is integrated into
[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md).
Two get options exist to retrieve Quandl data:

1.  `get = "quandl"`: Get’s Quandl time series data. A wrapper for
    `Quandl()`.
2.  `get = "quandl.datatable"`: Gets Quandl datatables (larger data sets
    that may not be time series). A wrapper for `Quandl.datatable()`.

Getting data from Quandl can be achieved in much the same way as the
other “get” options. Just pass the “codes” for the data along with
desired arguments for the underlying function.

The following uses `get = "quandl"` and the “WIKI” database to download
daily stock prices for AAPL in 2016. The output is a tidy data frame.

``` r
c("WIKI/AAPL") %>%
    tq_get(get  = "quandl",
           from = "2016-01-01",
           to   = "2016-12-31")
```

The following time series options are available to be passed to the
underlying `Quandl()` function:

- `start_date` (`from`) = “yyyy-mm-dd” \| `end_date` (`to`) =
  “yyyy-mm-dd”
- `column_index` = numeric column number (e.g. 1)
- `rows` = numeric row number indicating first n rows (e.g. 100)
- `collapse` = “none”, “daily”, “weekly”, “monthly”, “quarterly”,
  “annual”
- `transform` = “none”, “diff”, “rdiff”, “cumul”, “normalize”

Here’s an example to get period returns of the adj.close (column index
11) using the `column_index`, `collapse` and `transform` arguments.

``` r
"WIKI/AAPL" %>%
    tq_get(get          = "quandl",
           from         = "2007-01-01",
           to           = "2016-12-31",
           column_index = 11, 
           collapse     = "annual",      
           transform    = "rdiff")       
```

Datatables are larger data sets. These can be downloaded using
`get = "quandl.datatable"`. Note that the time series arguments do not
work with data tables.

Here’s several examples of [Zacks Fundamentals Collection
B](https://data.nasdaq.com/databases/ZFB/documentation/about)

``` r
# Zacks Fundamentals Collection B (DOW 30 Available to non subscribers)
tq_get("ZACKS/FC", get = "quandl.datatable")   # Zacks Fundamentals Condensed
tq_get("ZACKS/FR", get = "quandl.datatable")   # Zacks Fundamental Ratios
tq_get("ZACKS/MT", get = "quandl.datatable")   # Zacks Master Table
tq_get("ZACKS/MKTV", get = "quandl.datatable") # Zacks Market Value Supplement
tq_get("ZACKS/SHRS", get = "quandl.datatable") # Zacks Shares Out Supplement
```

### 2.4 Tiingo API

The Tiingo API is a free source for stock prices, cryptocurrencies, and
intraday feeds from the IEX (Investors Exchange). This can serve as an
alternate source of data to Yahoo! Finance.

#### Authentication

To make full use of the integration you need to get an API key and then
set your api key. If you don’t have one already, go to Tiingo account
and get your FREE API key. You can then set it as follows:

``` r
tiingo_api_key('<your-api-key>')
```

#### Getting Tiingo Data

The `tidyquant` package provides convenient wrappers to the `riingo`
package (R interface to Tiingo). Here’s how
[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
maps to `riingo`:

- Tiingo Prices: `tq_get(get = "tiingo") = riingo::riingo_prices()`
- Tiingo IEX Data:
  `tq_get(get = "tiingo.iex") = riingo::riingo_iex_prices()`
- Tiingo Crypto Data:
  `tq_get(get = "tiingo.crypto") = riingo::riingo_crypto_prices()`

``` r
# Tiingo Prices (Free alternative to Yahoo Finance!)
tq_get(c("AAPL", "GOOG"), get = "tiingo", from = "2010-01-01")

# Sub-daily prices from IEX ----
tq_get(c("AAPL", "GOOG"),
       get = "tiingo.iex",
       from   = "2020-01-01",
       to     = "2020-01-15",
       resample_frequency = "5min")

# Tiingo Bitcoin in USD ----
tq_get(c("btcusd"),
       get    = "tiingo.crypto",
       from   = "2020-01-01",
       to     = "2020-01-15",
       resample_frequency = "5min")
```

### 2.5 Alpha Vantage API

[Alpha Vantage](https://www.alphavantage.co/) provides access to a
real-time and historical financial data. The `alphavantager` package, a
lightweight R interface, has been integrated into `tidyquant` as
follows. The benefit of the integration is the **scalability since we
can now get multiple symbols returned in a tidy format**. You will need
to install it first.

``` r
install.packages("alphavantager")
```

#### Authentication

To make full use of the integration you need to get an API key and then
set your api key. If you don’t have one already, go to [Alpha
Vantage](https://www.alphavantage.co/) account and get your FREE API
key. You can then set it as follows:

``` r
# install.packages("alphavantager")
av_api_key("<your-api-key>")
```

#### Getting Alpha Vantage Data

Getting data is simple as the structure follows the [Alpha Vantage API
documentation](https://www.alphavantage.co/documentation/). For example,
if you wish to retrieve intraday data at 5 minute intervals for META and
MSFT, you can build the parameters
`x = c("META", "MSFT"), get = "alphavantager", av_fun = "TIME_SERIES_INTRADAY", interval = "5min"`.
The familiar `x` and `get` are the same as you always use. The `av_fun`
argument comes from
[`alphavantager::av_get()`](https://rdrr.io/pkg/alphavantager/man/av_get.html)
and the Alpha Vantage documentation. The `interval` argument comes from
the docs as well.

``` r
# Scaling is as simple as supplying multiple symbols
c("META", "MSFT") %>%
    tq_get(get = "alphavantage", av_fun = "TIME_SERIES_INTRADAY", interval = "5min")
```

### 2.6 Bloomberg

[Bloomberg](https://www.bloomberg.com/professional/solution/bloomberg-terminal/)
provides access to arguably the most comprehensive financial data and is
actively used by most major financial institutions that work with
financial data. The `Rblpapi` package, an R interface to Bloomberg, has
been integrated into `tidyquant` as follows. The benefit of the
integration is the **scalability since we can now get multiple symbols
returned in a tidy format**.

#### Authentication

To make full use of the integration you need to have a Bloomberg
Terminal account (Note this is not a free service). If you have
Bloomberg Terminal running on your machine, you can connect as follows:

``` r
# install.packages("Rblpapi")
Rblpapi::blpConnect()
```

#### Getting Bloomberg Data

Getting data is simple as the structure follows the [Rblpapi API
documentation](https://CRAN.R-project.org/package=Rblpapi). For example,
if you wish to retrieve monthly data for SPX Index and AGTHX Equity, you
can build the `tq_get` parameters as follows:

- `x = c('SPX Index','ODMAX Equity')`
- `get = "rblpapi"`
- `rblpapi_fun = "bdh"` Note that “bdh” is the default, and options
  include “bdh” (Bloomberg Data History), “bds” (Bloomberg Data Set),
  and “bdp” (Bloomberg Data Point)
- `from / to` These get passed to `start.date` and `end.date` and can be
  provided in “YYYY-MM-DD” character format. Note that `start.date` and
  `end.date` from `Rblpapi` can be used but must be converted to date or
  datetime.
- Other arguments: These are options that depend on the `rblpapi_fun`.
  See `Rblpapi` documentation.

``` r
# Get Bloomberg data in a tidy data frame
my_bloomberg_data <- c('SPX Index','ODMAX Equity') %>%
    tq_get(get         = "Rblpapi",
           rblpapi_fun = "bdh",
           fields      = c("PX_LAST"),
           options     = c("periodicitySelection" = "WEEKLY"),
           from        = "2016-01-01",
           to          = "2016-12-31")
```

## 3.0 Mutate Quantitative Data

Mutating functions enable the `xts`/`zoo`, `quantmod` and `TTR`
functions to shine. We’ll touch on the mutation functions briefly using
the `FANG` data set, which consists of daily prices for META, AMZN,
GOOG, and NFLX from the beginning of 2013 to the end of 2016. We’ll
apply the functions to grouped data sets to get a feel for how each
works

``` r
FANG
```

    ## # A tibble: 4,032 × 8
    ##    symbol date        open  high   low close    volume adjusted
    ##    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
    ##  1 META   2013-01-02  27.4  28.2  27.4  28    69846400     28  
    ##  2 META   2013-01-03  27.9  28.5  27.6  27.8  63140600     27.8
    ##  3 META   2013-01-04  28.0  28.9  27.8  28.8  72715400     28.8
    ##  4 META   2013-01-07  28.7  29.8  28.6  29.4  83781800     29.4
    ##  5 META   2013-01-08  29.5  29.6  28.9  29.1  45871300     29.1
    ##  6 META   2013-01-09  29.7  30.6  29.5  30.6 104787700     30.6
    ##  7 META   2013-01-10  30.6  31.5  30.3  31.3  95316400     31.3
    ##  8 META   2013-01-11  31.3  32.0  31.1  31.7  89598000     31.7
    ##  9 META   2013-01-14  32.1  32.2  30.6  31.0  98892800     31.0
    ## 10 META   2013-01-15  30.6  31.7  29.9  30.1 173242600     30.1
    ## # ℹ 4,022 more rows

For a detailed walkthrough of the compatible functions, see the next
vignette in the series, [R Quantitative Analysis Package Integrations in
tidyquant](https://business-science.github.io/tidyquant/articles/TQ02-quant-integrations-in-tidyquant.md).

### 3.1 Transmute Quantitative Data, tq_transmute

Transmute the results of
[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md).
Transmute here holds almost the same meaning as in `dplyr`, only the
newly created columns will be returned, but with
[`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md),
the number of rows returned can be different than the original data
frame. This is important for changing periodicity. An example is
periodicity aggregation from daily to monthly.

``` r
FANG %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = to.monthly, indexAt = "lastof")
```

    ## # A tibble: 192 × 3
    ## # Groups:   symbol [4]
    ##    symbol date       adjusted
    ##    <chr>  <date>        <dbl>
    ##  1 META   2013-01-31     31.0
    ##  2 META   2013-02-28     27.2
    ##  3 META   2013-03-31     25.6
    ##  4 META   2013-04-30     27.8
    ##  5 META   2013-05-31     24.4
    ##  6 META   2013-06-30     24.9
    ##  7 META   2013-07-31     36.8
    ##  8 META   2013-08-31     41.3
    ##  9 META   2013-09-30     50.2
    ## 10 META   2013-10-31     50.2
    ## # ℹ 182 more rows

Let’s go through what happened. `select` allows you to easily choose
what columns get passed to `mutate_fun`. In example above, `adjusted`
selects the “adjusted” column from `data`, and sends it to the mutate
function, `to.monthly`, which mutates the periodicity from daily to
monthly. Additional arguments can be passed to the `mutate_fun` by way
of `...`. We are passing the `indexAt` argument to return a date that
matches the first date in the period.

#### Working with non-OHLC data

Returns from FRED, Oanda, and other sources do not have open, high, low,
close (OHLC) format. However, this is not a problem with `select`. The
following example shows how to transmute WTI Crude daily prices to
monthly prices. Since we only have a single column to pass, we can leave
the `select` argument as `NULL` which selects all columns by default.
This sends the price column to the `to.period` mutate function.

``` r
wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 

wti_prices %>%    
    tq_transmute(mutate_fun = to.period,
                 period     = "months", 
                 col_rename = "WTI Price")
```

    ## # A tibble: 123 × 2
    ##    date       `WTI Price`
    ##    <date>           <dbl>
    ##  1 2016-01-29        33.7
    ##  2 2016-02-29        32.7
    ##  3 2016-03-31        36.9
    ##  4 2016-04-29        46.0
    ##  5 2016-05-31        49.1
    ##  6 2016-06-30        48.3
    ##  7 2016-07-29        41.5
    ##  8 2016-08-31        44.7
    ##  9 2016-09-30        47.7
    ## 10 2016-10-31        46.8
    ## # ℹ 113 more rows

### 3.2 Mutate Quantitative Data, tq_mutate

Adds a column or set of columns to the tibble with the calculated
attributes (hence the original tibble is returned, mutated with the
additional columns). An example is getting the `MACD` from `close`,
which mutates the original input by adding MACD and Signal columns. Note
that we can quickly rename the columns using the `col_rename` argument.

``` r
FANG %>%
    group_by(symbol) %>%
    tq_mutate(select     = close, 
              mutate_fun = MACD, 
              col_rename = c("MACD", "Signal"))
```

    ## # A tibble: 4,032 × 10
    ## # Groups:   symbol [4]
    ##    symbol date        open  high   low close    volume adjusted  MACD Signal
    ##    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl>
    ##  1 META   2013-01-02  27.4  28.2  27.4  28    69846400     28      NA     NA
    ##  2 META   2013-01-03  27.9  28.5  27.6  27.8  63140600     27.8    NA     NA
    ##  3 META   2013-01-04  28.0  28.9  27.8  28.8  72715400     28.8    NA     NA
    ##  4 META   2013-01-07  28.7  29.8  28.6  29.4  83781800     29.4    NA     NA
    ##  5 META   2013-01-08  29.5  29.6  28.9  29.1  45871300     29.1    NA     NA
    ##  6 META   2013-01-09  29.7  30.6  29.5  30.6 104787700     30.6    NA     NA
    ##  7 META   2013-01-10  30.6  31.5  30.3  31.3  95316400     31.3    NA     NA
    ##  8 META   2013-01-11  31.3  32.0  31.1  31.7  89598000     31.7    NA     NA
    ##  9 META   2013-01-14  32.1  32.2  30.6  31.0  98892800     31.0    NA     NA
    ## 10 META   2013-01-15  30.6  31.7  29.9  30.1 173242600     30.1    NA     NA
    ## # ℹ 4,022 more rows

Note that a mutation can occur if, and only if, the mutation has the
same structure of the original tibble. In other words, the calculation
must have the same number of rows and row.names (or date fields),
otherwise the mutation cannot be performed.

#### Mutate rolling regressions with rollapply

A very powerful example is applying **custom functions** across a
rolling window using `rollapply`. A specific example is using the
`rollapply` function to compute a rolling regression. This example is
slightly more complicated so it will be broken down into three steps:

1.  Get returns
2.  Create a custom function
3.  Apply the custom function across a rolling window using
    `tq_mutate(mutate_fun = rollapply)`

*Step 1: Get Returns*

First, get combined returns. The asset and baseline returns should be in
wide format, which is needed for the `lm` function in the next step.

``` r
fb_returns <- tq_get("META", get  = "stock.prices", from = "2016-01-01", to   = "2016-12-31") %>%
    tq_transmute(adjusted, periodReturn, period = "weekly", col_rename = "fb.returns")

xlk_returns <- tq_get("XLK", from = "2016-01-01", to = "2016-12-31") %>%
    tq_transmute(adjusted, periodReturn, period = "weekly", col_rename = "xlk.returns")

returns_combined <- left_join(fb_returns, xlk_returns, by = "date")
returns_combined
```

    ## # A tibble: 52 × 3
    ##    date       fb.returns xlk.returns
    ##    <date>          <dbl>       <dbl>
    ##  1 2016-01-08   -0.0478     -0.0516 
    ##  2 2016-01-15   -0.0242     -0.0187 
    ##  3 2016-01-22    0.0313      0.0264 
    ##  4 2016-01-29    0.146       0.0213 
    ##  5 2016-02-05   -0.0725     -0.0422 
    ##  6 2016-02-12   -0.0198     -0.00582
    ##  7 2016-02-19    0.0251      0.0354 
    ##  8 2016-02-26    0.0320      0.0148 
    ##  9 2016-03-04    0.00436     0.0281 
    ## 10 2016-03-11    0.00941     0.0106 
    ## # ℹ 42 more rows

*Step 2: Create a custom function*

Next, create a custom regression function, which will be used to apply
over the rolling window in Step 3. An important point is that the “data”
will be passed to the regression function as an `xts` object. The
[`timetk::tk_tbl`](https://business-science.github.io/timetk/reference/tk_tbl.html)
function takes care of converting to a data frame for the `lm` function
to work properly with the columns “fb.returns” and “xlk.returns”.

``` r
regr_fun <- function(data) {
    coef(lm(fb.returns ~ xlk.returns, data = timetk::tk_tbl(data, silent = TRUE)))
}
```

*Step 3: Apply the custom function*

Now we can use
[`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
to apply the custom regression function over a rolling window using
`rollapply` from the `zoo` package. Internally, since we left
`select = NULL`, the `returns_combined` data frame is being passed
automatically to the `data` argument of the `rollapply` function. All
you need to specify is the `mutate_fun = rollapply` and any additional
arguments necessary to apply the `rollapply` function. We’ll specify a
12 week window via `width = 12`. The `FUN` argument is our custom
regression function, `regr_fun`. It’s extremely important to specify
`by.column = FALSE`, which tells `rollapply` to perform the computation
using the data as a whole rather than apply the function to each column
independently. The `col_rename` argument is used to rename the added
columns.

``` r
returns_combined %>%
    tq_mutate(mutate_fun = rollapply,
              width      = 12,
              FUN        = regr_fun,
              by.column  = FALSE,
              col_rename = c("coef.0", "coef.1"))
```

    ## # A tibble: 52 × 5
    ##    date       fb.returns xlk.returns coef.0 coef.1
    ##    <date>          <dbl>       <dbl>  <dbl>  <dbl>
    ##  1 2016-01-08   -0.0478     -0.0516      NA     NA
    ##  2 2016-01-15   -0.0242     -0.0187      NA     NA
    ##  3 2016-01-22    0.0313      0.0264      NA     NA
    ##  4 2016-01-29    0.146       0.0213      NA     NA
    ##  5 2016-02-05   -0.0725     -0.0422      NA     NA
    ##  6 2016-02-12   -0.0198     -0.00582     NA     NA
    ##  7 2016-02-19    0.0251      0.0354      NA     NA
    ##  8 2016-02-26    0.0320      0.0148      NA     NA
    ##  9 2016-03-04    0.00436     0.0281      NA     NA
    ## 10 2016-03-11    0.00941     0.0106      NA     NA
    ## # ℹ 42 more rows

``` r
returns_combined
```

    ## # A tibble: 52 × 3
    ##    date       fb.returns xlk.returns
    ##    <date>          <dbl>       <dbl>
    ##  1 2016-01-08   -0.0478     -0.0516 
    ##  2 2016-01-15   -0.0242     -0.0187 
    ##  3 2016-01-22    0.0313      0.0264 
    ##  4 2016-01-29    0.146       0.0213 
    ##  5 2016-02-05   -0.0725     -0.0422 
    ##  6 2016-02-12   -0.0198     -0.00582
    ##  7 2016-02-19    0.0251      0.0354 
    ##  8 2016-02-26    0.0320      0.0148 
    ##  9 2016-03-04    0.00436     0.0281 
    ## 10 2016-03-11    0.00941     0.0106 
    ## # ℹ 42 more rows

As shown above, the rolling regression coefficients were added to the
data frame.

### 3.3 \_xy Variants, tq_mutate_xy and tq_transmute_xy

Enables working with mutation functions that require two primary inputs
(e.g. EVWMA, VWAP, etc).

#### Mutate with two primary inputs

EVWMA (exponential volume-weighted moving average) requires two inputs,
price and volume. To work with these columns, we can switch to the xy
variants,
[`tq_transmute_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
and
[`tq_mutate_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md).
The only difference is instead of the `select` argument, you use `x` and
`y` arguments to pass the columns needed based on the `mutate_fun`
documentation.

``` r
FANG %>%
    group_by(symbol) %>%
    tq_mutate_xy(x = close, y = volume, 
                 mutate_fun = EVWMA, col_rename = "EVWMA")
```

    ## # A tibble: 4,032 × 9
    ## # Groups:   symbol [4]
    ##    symbol date        open  high   low close    volume adjusted EVWMA
    ##    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>
    ##  1 META   2013-01-02  27.4  28.2  27.4  28    69846400     28    NA  
    ##  2 META   2013-01-03  27.9  28.5  27.6  27.8  63140600     27.8  NA  
    ##  3 META   2013-01-04  28.0  28.9  27.8  28.8  72715400     28.8  NA  
    ##  4 META   2013-01-07  28.7  29.8  28.6  29.4  83781800     29.4  NA  
    ##  5 META   2013-01-08  29.5  29.6  28.9  29.1  45871300     29.1  NA  
    ##  6 META   2013-01-09  29.7  30.6  29.5  30.6 104787700     30.6  NA  
    ##  7 META   2013-01-10  30.6  31.5  30.3  31.3  95316400     31.3  NA  
    ##  8 META   2013-01-11  31.3  32.0  31.1  31.7  89598000     31.7  NA  
    ##  9 META   2013-01-14  32.1  32.2  30.6  31.0  98892800     31.0  NA  
    ## 10 META   2013-01-15  30.6  31.7  29.9  30.1 173242600     30.1  30.1
    ## # ℹ 4,022 more rows
