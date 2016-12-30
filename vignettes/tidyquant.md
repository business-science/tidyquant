# Introduction to tidyquant
Matt Dancho  
`r Sys.Date()`  

> Bringing quantitative financial analysis to the tidyverse

# Overview

`tidyquant` integrates the best quantitative resources for collecting and analyzing quantitative data, `xts`, `quantmod` and `TTR`, with the tidy data infrastructure of the `tidyverse` allowing for seamless interaction between each. 

The three primary quantitative packages that are the backbone for quantitative financial analysis in _R programming_ are: 

* [xts](https://cran.r-project.org/web/packages/xts/vignettes/xts.pdf), or [eXtensible time series](http://joshuaulrich.github.io/xts/index.html): The data structure for handling time-series data.
* [quantmod](http://www.quantmod.com/): A package designed for retrieving, manipulating, and modeling and  quantitative data.
* [TTR](https://cran.r-project.org/web/packages/TTR/TTR.pdf), or Technical Trading Rules: A package that includes various functions to compute technical trading equations for quantitative or trading data.

The [tidy data principles](https://www.jstatsoft.org/article/view/v059i10) are a cornerstone of data management and data modeling workflow. The foundation for tidy data management is the `tidyverse`, a collection of _R packages:_ `ggplot2`, `dplyr`, `tidyr`, `purrr`, `readr`, `tibble`, that work in harmony, are built for scalability, and are well documented in [R for Data Science](http://r4ds.had.co.nz/). Using this infrastructure and the core tidy concepts, we can integrate the tidy data principles with the best quantitative financial analysis packages using the package, `tidyquant`. 

# Prerequisites



Load the `tidyquant` package to get started.


```r
library(tidyquant)  # Loads tidyquant, tidyverse, xts, quantmod, TTR 
```

# Benefits

__The `tidyquant` philosophy:__

* __[A few core functions with a lot of power, that](#core-functions)__
* __[leverage the quantitative analysis power of `xts`, `quantmod` and `TTR`, and are](#quant-power)__
* __[designed to be scaled with the `tidyverse` workflow](#built-for-scale).__

<a class="anchor" id="core-functions"></a>

## A Few Core Functions with A Lot of Power

Minimizing the number of functions reduces the learning curve. Functions are grouped into verbs for efficient collection and manipulation of quantitative data: 

* __[Get Quantitative Data, `tq_get()`:](#tq-get)__ A one-stop shop to get data from various web-sources.  

* __[Transform, `tq_transform()`, and Mutate, `tq_mutate()`, Quantitative Data:](#tq-transform)__ These are the workhorse functions that wrap around the `xts`, `quantmod`, and `TTR` packages. 

* __[Coerce Quantitative Data Between tibble and xts formats, `as_tibble()` and `as_xts()`:](#tq-coerce)__ Coercing `xts`, `zoo`, `timeSeries`, and the other various _R_ time-based objects to and from `tibble` or `data.frame` objects was a pain due to the date/time being stored as row names in time-based objects. The tidyquant `as_tibble()` and `as_xts()` functions enable preservation of row names during coercion.

<a class="anchor" id="tq-get"></a>

### Get Quantitative Data

The `tq_get()` function is used to collect all data by changing the `get` argument. The options include stock lists for 18 stock indexes from marketvolume.com, stock prices, dividends and splits from Yahoo Finance, financial statements from Google Finance, metal prices and exchange rates from Oanda, and economic data from the FRED database. To see the full list, execute `tq_get_options()`.


```r
tq_get_options()
```

```
## [1] "stock.prices"   "stock.index"    "dividends"      "splits"        
## [5] "financials"     "economic.data"  "exchange.rates" "metal.prices"
```


__Stock Index:__

A wide range of stock index / exchange lists can be retrieved using `get = "stock.index"`. To get a full list of the options, use `tq_get_stock_index_options()`. 


```r
tq_get_stock_index_options()
```

```
##  [1] "DOWJONES"    "DJI"         "DJT"         "DJU"         "SP100"      
##  [6] "SP400"       "SP500"       "SP600"       "RUSSELL1000" "RUSSELL2000"
## [11] "RUSSELL3000" "AMEX"        "AMEXGOLD"    "AMEXOIL"     "NASDAQ"     
## [16] "NASDAQ100"   "NYSE"        "SOX"
```

Set `x` as one of the options in the list of options above, and `get = "stock.index"` to get the desired stock index / exchange.


```r
tq_get("sp500", get = "stock.index")
```

```
## # A tibble: 501 × 2
##    symbol                   company
##     <chr>                     <chr>
## 1     MMM                        3M
## 2     ABT       ABBOTT LABORATORIES
## 3    ABBV                ABBVIE INC
## 4     ACN                 ACCENTURE
## 5    ATVI       ACTIVISION BLIZZARD
## 6     AYI             ACUITY BRANDS
## 7    ADBE             ADOBE SYSTEMS
## 8     AAP        ADVANCE AUTO PARTS
## 9     AET                     AETNA
## 10    AMG AFFILIATED MANAGERS GROUP
## # ... with 491 more rows
```

The data source is [www.marketvolume.com](http://www.marketvolume.com/indexes_exchanges/).

__Stock Prices, Dividends and Splits:__

The stock prices can be retrieved succinctly using `get = "stock.prices"`.  


```r
appl_prices  <- tq_get("AAPL", get = "stock.prices", from = " 1990-01-01")
appl_prices 
```

```
## # A tibble: 6,804 × 7
##          date   open   high   low  close   volume adjusted
##        <date>  <dbl>  <dbl> <dbl>  <dbl>    <dbl>    <dbl>
## 1  1990-01-02 35.250 37.500 35.00 37.250 45799600 1.132075
## 2  1990-01-03 38.000 38.000 37.50 37.500 51998800 1.139673
## 3  1990-01-04 38.250 38.750 37.25 37.625 55378400 1.143471
## 4  1990-01-05 37.750 38.250 37.00 37.750 30828000 1.147270
## 5  1990-01-08 37.500 38.000 37.00 38.000 25393200 1.154868
## 6  1990-01-09 38.000 38.000 37.00 37.625 21534800 1.143471
## 7  1990-01-10 37.625 37.625 35.75 36.000 49929600 1.094086
## 8  1990-01-11 36.250 36.250 34.50 34.500 52763200 1.048499
## 9  1990-01-12 34.250 34.750 33.75 34.500 42974400 1.048499
## 10 1990-01-15 34.500 35.750 34.25 34.250 40434800 1.040901
## # ... with 6,794 more rows
```


Dividends are obtained using `get = "dividends"`. 


```r
appl_divs <- tq_get("AAPL", get = "dividends", from = "1990-01-01")
appl_divs
```

```
## # A tibble: 42 × 2
##          date dividends
##        <date>     <dbl>
## 1  1990-02-16   0.00393
## 2  1990-05-21   0.00393
## 3  1990-08-20   0.00393
## 4  1990-11-16   0.00429
## 5  1991-02-15   0.00429
## 6  1991-05-20   0.00429
## 7  1991-08-19   0.00429
## 8  1991-11-18   0.00429
## 9  1992-02-14   0.00429
## 10 1992-06-01   0.00429
## # ... with 32 more rows
```

Stock splits are obtained using `get = "splits"`. 


```r
appl_splits <- tq_get("AAPL", get = "splits", from = "1990-01-01")
appl_splits
```

```
## # A tibble: 3 × 2
##         date    splits
##       <date>     <dbl>
## 1 2000-06-21 0.5000000
## 2 2005-02-28 0.5000000
## 3 2014-06-09 0.1428571
```

The data source is [yahoo finance](https://finance.yahoo.com/).

__Financial Statements:__

For any given stock, a total of six financials statements are retrieved as nested tibbles, one for each combination of statement type (Income Statement, Balance Sheet, and Cash Flow) and period (by annual and quarter). 


```r
fb_financials <- tq_get("FB", get = "financials")
fb_financials
```

```
## # A tibble: 3 × 3
##    type             annual            quarter
## * <chr>             <list>             <list>
## 1    BS <tibble [168 × 4]> <tibble [210 × 4]>
## 2    CF  <tibble [76 × 4]>  <tibble [76 × 4]>
## 3    IS <tibble [196 × 4]> <tibble [245 × 4]>
```

The statement information can be extracted by selecting (`dplyr::select()`) and filtering (`dplyr::filter()`) to the desired statement and unnesting (`tidyr::unnest()`) the results.


```r
fb_financials %>%
    filter(type == "IS") %>%
    select(annual) %>%
    unnest()
```

```
## # A tibble: 196 × 4
##    group             category       date value
##    <int>                <chr>     <date> <dbl>
## 1      1              Revenue 2015-12-31 17928
## 2      1              Revenue 2014-12-31 12466
## 3      1              Revenue 2013-12-31  7872
## 4      1              Revenue 2012-12-31  5089
## 5      2 Other Revenue, Total 2015-12-31    NA
## 6      2 Other Revenue, Total 2014-12-31    NA
## 7      2 Other Revenue, Total 2013-12-31    NA
## 8      2 Other Revenue, Total 2012-12-31    NA
## 9      3        Total Revenue 2015-12-31 17928
## 10     3        Total Revenue 2014-12-31 12466
## # ... with 186 more rows
```

A slightly more powerful example is looking at all quarterly statements together. This is easy to do with `unnest` and `spread` from the `tidyr` package.


```r
fb_financials %>%
    unnest(quarter) %>% 
    spread(key = date, value = value)
```

```
## # A tibble: 110 × 8
##     type group                         category `2015-09-30` `2015-12-31`
## *  <chr> <int>                            <chr>        <dbl>        <dbl>
## 1     BS     1               Cash & Equivalents         1621         2409
## 2     BS     2           Short Term Investments        11526        14322
## 3     BS     3  Cash and Short Term Investments        15834        18434
## 4     BS     4 Accounts Receivable - Trade, Net         2010         2559
## 5     BS     5              Receivables - Other           NA           NA
## 6     BS     6           Total Receivables, Net         2010         2559
## 7     BS     7                  Total Inventory           NA           NA
## 8     BS     8                 Prepaid Expenses         1295          659
## 9     BS     9      Other Current Assets, Total           NA           NA
## 10    BS    10             Total Current Assets        19139        21652
## # ... with 100 more rows, and 3 more variables: `2016-03-31` <dbl>,
## #   `2016-06-30` <dbl>, `2016-09-30` <dbl>
```


The data source is [google finance](https://www.google.com/finance).


<a class="anchor" id="economic-data"></a>

__Economic Data__: 

A wealth of economic data can be extracted from the Federal Reserve Economic Data (FRED) database. The [WTI Crude Oil Prices](https://fred.stlouisfed.org/series/DCOILWTICO) are shown below.


```r
wti_price_usd <- tq_get("DCOILWTICO", get = "economic.data")
wti_price_usd 
```

```
## # A tibble: 2,867 × 2
##          date price
##        <date> <dbl>
## 1  2006-01-02    NA
## 2  2006-01-03 63.11
## 3  2006-01-04 63.41
## 4  2006-01-05 62.81
## 5  2006-01-06 64.21
## 6  2006-01-09 63.56
## 7  2006-01-10 63.41
## 8  2006-01-11 63.91
## 9  2006-01-12 63.96
## 10 2006-01-13 63.86
## # ... with 2,857 more rows
```


The FRED contains literally over 10K data sets that are free to use. See the [FRED categories](https://fred.stlouisfed.org/categories) to narrow down the data base and to get data codes. 


__Exchange Rates:__

Exchange rates are entered as currency pairs using "/" notation (e.g `"EUR/USD"`), and by setting `get = "exchange.rates"`. 


```r
eur_usd <- tq_get("EUR/USD", get = "exchange.rates", from = "2000-01-01")
eur_usd 
```

```
## # A tibble: 1,827 × 2
##          date exchange.rate
##        <date>         <dbl>
## 1  2011-12-30       1.29493
## 2  2011-12-31       1.29618
## 3  2012-01-01       1.29590
## 4  2012-01-02       1.29375
## 5  2012-01-03       1.30038
## 6  2012-01-04       1.30036
## 7  2012-01-05       1.28717
## 8  2012-01-06       1.27698
## 9  2012-01-07       1.27195
## 10 2012-01-08       1.27151
## # ... with 1,817 more rows
```

The data source is [Oanda](https://www.oanda.com/), and list of currencies to compare can be found on [Oanda's currency converter](https://www.oanda.com/currency/converter/). It may make more sense to get this data from the FRED (See [Economic Data](#economic-data)) since the max period for Oanda is 5-years.

__Metal Prices:__

Metal prices are very similar to stock prices. Set `get = "metal.prices"` along with the appropriate commodity symbol (e.g. XAU (gold) , XAG (silver), XPD (palladium), or XPT (platinum)). 


```r
plat_price_eur <- tq_get("plat", get = "metal.prices", 
                         from = "2000-01-01", base.currency = "EUR")
plat_price_eur 
```

```
## # A tibble: 1,827 × 2
##          date   price
##        <date>   <dbl>
## 1  2011-12-30 1081.91
## 2  2011-12-31 1080.87
## 3  2012-01-01 1081.11
## 4  2012-01-02 1085.99
## 5  2012-01-03 1080.45
## 6  2012-01-04 1080.47
## 7  2012-01-05 1091.55
## 8  2012-01-06 1100.26
## 9  2012-01-07 1104.61
## 10 2012-01-08 1097.12
## # ... with 1,817 more rows
```

The data source is [Oanda](https://www.oanda.com/). It may make more sense to get this data from the FRED (See [Economic Data](#economic-data)) since the max period for Oanda is 5-years.

<a class="anchor" id="tq-transform"></a>

### Transform and Mutate Quantitative Data

Transform and mutate functions enable the `xts`, `quantmod` and `TTR` functions to shine (see [Leverage the Quantitative Power of `xts`, `quantmod` and `TTR`](#quant-power)):

__Transform Quantitative Data, `tq_transform()`:__ 

Transforms the results of `tq_get()`. The result is typically a different shape than the input (hence "transformed"), although this is not a requirement. An example is periodicity aggregation from daily to monthly.


```r
fb_prices <- tq_get("FB") 
fb_prices %>%
    tq_transform(x_fun = OHLCV, transform_fun = to.monthly)
```

```
## # A tibble: 56 × 6
##        date  open  high   low close    volume
##       <chr> <dbl> <dbl> <dbl> <dbl>     <dbl>
## 1  May 2012 28.55 29.67 26.83 29.60 111639200
## 2  Jun 2012 31.92 31.99 30.76 31.10  19526900
## 3  Jul 2012 23.37 23.37 21.61 21.71  56179400
## 4  Aug 2012 18.68 18.70 18.03 18.06  58764200
## 5  Sep 2012 20.57 21.95 20.50 21.66  65486000
## 6  Oct 2012 20.82 21.50 20.73 21.11  99378200
## 7  Nov 2012 27.26 28.00 26.76 28.00 127049600
## 8  Dec 2012 26.20 26.99 26.11 26.62  60374500
## 9  Jan 2013 29.15 31.47 28.74 30.98 190744900
## 10 Feb 2013 26.84 27.30 26.34 27.25  83027800
## # ... with 46 more rows
```

Let's go through what happened. `x_fun` is one of the various quantmod Open, High, Low, Close (OHLC) functions (see `?quantmod::OHLC`). The function returns a column or set of columns from data that are passed to the `transform_fun`. In example above, `OHLCV` selects the full list of prices and volumes from `data`, and sends this to the transform function, `to.monthly`, which transforms the periodicity from daily to monthly. Additional arguments can be passed to the `transform_fun` by way of `...`. 


__Mutate Quantitative Data, `tq_mutate()`:__ 

Adds a column or set of columns to the tibble with the calculated attributes (hence the original tibble is returned, mutated with the additional columns). An example is getting the `MACD` from `Cl` (close price), which mutates the original input by adding MACD and Signal columns. 


```r
fb_prices %>%
    tq_mutate(x_fun = Cl, mutate_fun = MACD)
```

```
## # A tibble: 1,162 × 9
##          date  open  high   low close    volume adjusted  macd signal
##        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl>
## 1  2012-05-18 42.05 45.00 38.00 38.23 573576400    38.23    NA     NA
## 2  2012-05-21 36.53 36.66 33.00 34.03 168192700    34.03    NA     NA
## 3  2012-05-22 32.61 33.59 30.94 31.00 101786600    31.00    NA     NA
## 4  2012-05-23 31.37 32.50 31.36 32.00  73600000    32.00    NA     NA
## 5  2012-05-24 32.95 33.21 31.77 33.03  50237200    33.03    NA     NA
## 6  2012-05-25 32.90 32.95 31.11 31.91  37149800    31.91    NA     NA
## 7  2012-05-29 31.48 31.69 28.65 28.84  78063400    28.84    NA     NA
## 8  2012-05-30 28.70 29.55 27.86 28.19  57267900    28.19    NA     NA
## 9  2012-05-31 28.55 29.67 26.83 29.60 111639200    29.60    NA     NA
## 10 2012-06-01 28.89 29.15 27.39 27.72  41855500    27.72    NA     NA
## # ... with 1,152 more rows
```

Note that a mutation can occur if, and only if, the mutation has the same structure of the original tibble. In other words, the calculation must have the same number of rows and row.names (or date fields), otherwise the mutation cannot be performed.

__xy Variants, `tq_transform_xy` and `tq_mutate_xy`:__ 

Enables working with:

1. Transformation functions that require two primary inputs (e.g. EVWMA, VWAP, etc) 
2. Data that is not in OHLC format. 

_Transformation with two primary inputs: _

EVWMA (exponential volume-weighted moving average) requires two inputs, price and volume, that are not in OHLC code format. To work with these columns, we can switch to the xy variants, `tq_transform_xy()` and `tq_mutate_xy()`. The only difference is instead of an `x_fun` argument, you use `.x` and `.y` arguments to pass the columns needed based on the `transform_fun` or `mutate_fun` documentation.


```r
fb_prices %>%
    tq_mutate_xy(.x = close, .y = volume, mutate_fun = EVWMA)
```

```
## # A tibble: 1,162 × 8
##          date  open  high   low close    volume adjusted    V1
##        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>
## 1  2012-05-18 42.05 45.00 38.00 38.23 573576400    38.23    NA
## 2  2012-05-21 36.53 36.66 33.00 34.03 168192700    34.03    NA
## 3  2012-05-22 32.61 33.59 30.94 31.00 101786600    31.00    NA
## 4  2012-05-23 31.37 32.50 31.36 32.00  73600000    32.00    NA
## 5  2012-05-24 32.95 33.21 31.77 33.03  50237200    33.03    NA
## 6  2012-05-25 32.90 32.95 31.11 31.91  37149800    31.91    NA
## 7  2012-05-29 31.48 31.69 28.65 28.84  78063400    28.84    NA
## 8  2012-05-30 28.70 29.55 27.86 28.19  57267900    28.19    NA
## 9  2012-05-31 28.55 29.67 26.83 29.60 111639200    29.60    NA
## 10 2012-06-01 28.89 29.15 27.39 27.72  41855500    27.72 27.72
## # ... with 1,152 more rows
```

_Working with non-OHLC data: _

Returns from FRED, Oanda, and other sources do not have open, high, low, close, and volume (OHLCV) format. The following example shows how to transform WTI Crude daily prices to monthly prices. Since we only have a single column to pass, set the `.x = price` and leave the `.y = NULL`. This sends the price column to the `to.period` transformation fuction. 


```r
wti_prices <- tq_get("DCOILWTICO", get = "economic.data") 
wti_prices %>%    
    tq_transform_xy(.x = price, transform_fun = to.period,
                    period = "months")
```

```
## # A tibble: 132 × 2
##          date price
##        <dttm> <dbl>
## 1  2006-01-31 67.86
## 2  2006-02-28 61.37
## 3  2006-03-31 66.25
## 4  2006-04-28 71.80
## 5  2006-05-31 71.42
## 6  2006-06-30 73.94
## 7  2006-07-31 74.56
## 8  2006-08-31 70.38
## 9  2006-09-29 62.90
## 10 2006-10-31 58.72
## # ... with 122 more rows
```

<a class="anchor" id="tq-coerce"></a>

### Coercing Time Series Objects To and From Tibble

Sometimes you want to work using a `tibble` and other times you want to work using a `xts` object. The `as_tibble()` and `as_xts()` functions are the key.

__Coerce from time-series to tibble, `as_tibble()`:__

The `tidyquant::as_tibble()` function includes a `preserve_row_names` argument, which is useful when coercing one of the many time formats (e.g. `xts`, `zoo`, `timeSeries`, `ts`) or `matrix` objects that contain valuable information in the row names. This makes bridging the gap between the various quantitative analysis packages and the `tidyverse` much easier.

Let's start with an `xts` object.


```r
# Create xts object from a matrix
vals = matrix(c(500, 504, 503))
date = c("2016-01-01", "2016-01-02", "2016-01-03") 
rownames(vals) <- date
time_series_xts <- as_xts(vals)
time_series_xts
```

```
##            [,1]
## 2016-01-01  500
## 2016-01-02  504
## 2016-01-03  503
```

We can easily coerce to `tibble` by setting `preserve_row_names = TRUE`. Note the return column is `row.names` with class of `character`.


```r
time_series_tbl <- as_tibble(time_series_xts, preserve_row_names = TRUE)
time_series_tbl
```

```
## # A tibble: 3 × 2
##    row.names    V1
##        <chr> <dbl>
## 1 2016-01-01   500
## 2 2016-01-02   504
## 3 2016-01-03   503
```

Converting to date is one extra step with `lubridate`.


```r
time_series_tbl <- time_series_tbl %>%
    mutate(row.names = lubridate::ymd(row.names))
time_series_tbl
```

```
## # A tibble: 3 × 2
##    row.names    V1
##       <date> <dbl>
## 1 2016-01-01   500
## 2 2016-01-02   504
## 3 2016-01-03   503
```

__Coerce from tibble to xts, `as_xts()`:__

We can convert back to `xts` with the tidyquant `as_xts()` function. Make sure to set the date column (`date_col`) argument to the column name containing the date (`date_col = row.names`). The date column must be in a date format (inherits either `Date` or `POSIXct` classes).


```r
time_series_xts <- time_series_tbl %>%
    as_xts(date_col = row.names)
time_series_xts
```

```
##             V1
## 2016-01-01 500
## 2016-01-02 504
## 2016-01-03 503
```

<a class="anchor" id="quant-power"></a>

## Leverage the Quantitative Power of `xts`, `quantmod` and `TTR`

You may already know and love `xts`, `quantmod` and `TTR`, which is why the core functionality is fully intact. Using `tq_transform()` and `tq_mutate()`, we can apply the `xts`, `quantmod` and `TTR` functions. Entering `tq_transform_fun_options()` returns a list the transform functions by each package. We'll discuss these options by package briefly.


```r
tq_transform_fun_options() %>% str()
```

```
## List of 3
##  $ xts     : chr [1:27] "apply.daily" "apply.monthly" "apply.quarterly" "apply.weekly" ...
##  $ quantmod: chr [1:25] "allReturns" "annualReturn" "ClCl" "dailyReturn" ...
##  $ TTR     : chr [1:61] "adjRatios" "ADX" "ALMA" "aroon" ...
```


### xts Functionality



```r
# Get xts functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$xts
```

```
##  [1] "apply.daily"     "apply.monthly"   "apply.quarterly"
##  [4] "apply.weekly"    "apply.yearly"    "diff.xts"       
##  [7] "lag.xts"         "period.apply"    "period.max"     
## [10] "period.min"      "period.prod"     "period.sum"     
## [13] "periodicity"     "to.daily"        "to.hourly"      
## [16] "to.minutes"      "to.minutes10"    "to.minutes15"   
## [19] "to.minutes3"     "to.minutes30"    "to.minutes5"    
## [22] "to.monthly"      "to.period"       "to.quarterly"   
## [25] "to.weekly"       "to.yearly"       "to_period"
```

The `xts` functions that are compatible are listed above. Generally speaking, these are the:

* Period Apply Functions:
    * Apply a function to a time segment (e.g. `max`, `min`, `mean`, etc).
    * Form: `apply.daily(x, FUN, ...)`.
    * Options include apply.daily, weekly, monthly, quarterly, yearly.

* To-Period Functions:
    * Convert a time series to time series of lower periodicity (e.g. convert daily to monthly periodicity).
    * Form: `to.period(x, period = 'months', k = 1, indexAt, name = NULL, OHLC = TRUE, ...)`.
    * Options include to.minutes, hourly, daily, weekly, monthly, quarterly, yearly.
    * __Note 1 (Important):__ The return structure is different for `to.period` and the `to.monthly` (`to.weekly`, `to.quarterly`, etc) forms. `to.period` returns a date, while `to.months` returns a character MON YYYY. Best to use `to.period` if you want to work with time-series via `lubridate`.  
     


### quantmod Functionality


```r
# Get quantmod functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$quantmod
```

```
##  [1] "allReturns"      "annualReturn"    "ClCl"           
##  [4] "dailyReturn"     "Delt"            "HiCl"           
##  [7] "Lag"             "LoCl"            "LoHi"           
## [10] "monthlyReturn"   "Next"            "OpCl"           
## [13] "OpHi"            "OpLo"            "OpOp"           
## [16] "periodReturn"    "quarterlyReturn" "seriesAccel"    
## [19] "seriesDecel"     "seriesDecr"      "seriesHi"       
## [22] "seriesIncr"      "seriesLo"        "weeklyReturn"   
## [25] "yearlyReturn"
```

The `quantmod` functions that are compatible are listed above. Generally speaking, these are the:

* Percentage Change (Delt) and Lag Functions
    * Delt: `Delt(x1, x2 = NULL, k = 0, type = c("arithmetic", "log"))`
        * Variations of Delt: ClCl, HiCl, LoCl, LoHi, OpCl, OpHi, OpLo, OpOp 
        * Form: `OpCl(OHLC)`
    * Lag: `Lag(x, k = 1)` / Next: `Next(x, k = 1)` (Can also use `dplyr::lag` and `dplyr::lead`)
    

* Period Return Functions: 
    * Get the arithmetic or logarithmic returns for various periodicities, which include daily, weekly, monthly, quarterly, and yearly.
    * Form: `periodReturn(x, period = 'monthly', subset = NULL, type = 'arithmetic', leading = TRUE, ...)`

* Series Functions: 
    * Return values that describe the series. Options include describing the increases/decreases, accelerations/decelerations, and hi/low.
    * Forms: `seriesHi(x)`, `seriesIncr(x, thresh = 0, diff. = 1L)`, `seriesAccel(x)`

### TTR Functionality


```r
# Get TTR functions that work with tq_transform and tq_mutate
tq_transform_fun_options()$TTR
```

```
##  [1] "adjRatios"          "ADX"                "ALMA"              
##  [4] "aroon"              "ATR"                "BBands"            
##  [7] "CCI"                "chaikinAD"          "chaikinVolatility" 
## [10] "CLV"                "CMF"                "CMO"               
## [13] "DEMA"               "DonchianChannel"    "DPO"               
## [16] "DVI"                "EMA"                "EMV"               
## [19] "EVWMA"              "GMMA"               "growth"            
## [22] "HMA"                "KST"                "lags"              
## [25] "MACD"               "MFI"                "momentum"          
## [28] "OBV"                "PBands"             "ROC"               
## [31] "rollSFM"            "RSI"                "runCor"            
## [34] "runCov"             "runMAD"             "runMax"            
## [37] "runMean"            "runMedian"          "runMin"            
## [40] "runPercentRank"     "runSD"              "runSum"            
## [43] "runVar"             "SAR"                "SMA"               
## [46] "SMI"                "stoch"              "TDI"               
## [49] "TRIX"               "ultimateOscillator" "VHF"               
## [52] "VMA"                "volatility"         "VWAP"              
## [55] "VWMA"               "wilderSum"          "williamsAD"        
## [58] "WMA"                "WPR"                "ZigZag"            
## [61] "ZLEMA"
```


Here' a brief description of the most popular functions from `TTR`:

* Welles Wilder's Directional Movement Index: 
    *  `ADX(HLC, n = 14, maType, ...)`
* Bollinger Bands: 
    *  `BBands(HLC, n = 20, maType, sd = 2, ...)`: Bollinger Bands
* Rate of Change / Momentum: 
    * `ROC(x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE)`: Rate of Change
    * `momentum(x, n = 1, na.pad = TRUE)`: Momentum
* Moving Averages (maType):
    * `SMA(x, n = 10, ...)`: Simple Moving Average
    * `EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...)`: Exponential Moving Average
    * `DEMA(x, n = 10, v = 1, wilder = FALSE, ratio = NULL)`: Double Exponential Moving Average
    * `WMA(x, n = 10, wts = 1:n, ...)`: Weighted Moving Average
    * `EVWMA(price, volume, n = 10, ...)`: Elastic, Volume-Weighted Moving Average
    * `ZLEMA(x, n = 10, ratio = NULL, ...)`: Zero Lag Exponential Moving Average
    * `VWAP(price, volume, n = 10, ...)`: Volume-Weighted Moving Average Price
    * `VMA(x, w, ratio = 1, ...)`: Variable-Length Moving Average
    * `HMA(x, n = 20, ...)`: Hull Moving Average
    * `ALMA(x, n = 9, offset = 0.85, sigma = 6, ...)`: Arnaud Legoux Moving Average
* MACD Oscillator: 
    *  `MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)`
* Relative Strength Index: 
    *  `RSI(price, n = 14, maType, ...)`
* runFun: 
    * `runSum(x, n = 10, cumulative = FALSE)`: returns sums over a n-period moving window.
    * `runMin(x, n = 10, cumulative = FALSE)`: returns minimums over a n-period moving window.
    * `runMax(x, n = 10, cumulative = FALSE)`: returns maximums over a n-period moving window.
    * `runMean(x, n = 10, cumulative = FALSE)`: returns means over a n-period moving window.
    * `runMedian(x, n = 10, non.unique = "mean", cumulative = FALSE)`: returns medians over a n-period moving window.
    * `runCov(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)`: returns covariances over a n-period moving window.
    * `runCor(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)`: returns correlations over a n-period moving window.
    * `runVar(x, y = NULL, n = 10, sample = TRUE, cumulative = FALSE)`: returns variances over a n-period moving window.
    * `runSD(x, n = 10, sample = TRUE, cumulative = FALSE)`: returns standard deviations over a n-period moving window.
    * `runMAD(x, n = 10, center = NULL, stat = "median", constant = 1.4826, non.unique = "mean", cumulative = FALSE)`: returns median/mean absolute deviations over a n-period moving window.
    * `wilderSum(x, n = 10)`: retuns a Welles Wilder style weighted sum over a n-period moving window.
* Stochastic Oscillator / Stochastic Momentum Index:
    * `stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, smooth = 1, ...)`: Stochastic Oscillator
    * `SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, bounded = TRUE, ...)`: Stochastic Momentum Index


### Quantitative Power In Action

We'll go through some examples, but first let's get some data. The default for `tq_get()` is `get = "stock.prices"`, so all we need is to give `x` a stock symbol.


```r
AAPL <- tq_get("AAPL")
```


#### Example 1: Getting the max close price for each quarter.

The `xts::apply.quarterly()` function that is part of the period apply group can be used to apply functions by quarterly time segments. Because we are seeking a return structure that is on a different time scale than the input (quarterly versus daily), we need to use a transform function. We select `tq_transform` and pass the close price using OHLC format via `x_fun = Cl`, and we send this subset of the data to the `apply.quarterly` function via the `transform_fun` argument. Looking at the documentation for `apply.quarterly`, we see that we can pass a function to the argument, `FUN`. We want the maximum values, so we set `FUN = max`. The result is the quarters returned as a date and the maximum closing price during the quarter returned as a double. 


```r
AAPL %>%
    tq_transform(x_fun = Cl, transform_fun = apply.quarterly, FUN = max)
```

```
## # A tibble: 44 × 2
##          date  close
##        <dttm>  <dbl>
## 1  2006-03-31  85.59
## 2  2006-06-30  71.89
## 3  2006-09-29  77.61
## 4  2006-12-29  91.81
## 5  2007-03-30  97.10
## 6  2007-06-29 125.09
## 7  2007-09-28 154.50
## 8  2007-12-31 199.83
## 9  2008-03-31 194.93
## 10 2008-06-30 189.96
## # ... with 34 more rows
```

Note that as an alternative you could use the xy form, replacing `x_fun = Cl` with `.x = close`.

#### Example 2: Getting daily log returns 

The `quantmod::periodReturn()` function generates returns by periodicity. We have a few options here. Normally I go with a transform function, `tq_transform`, because the `periodReturn` function accepts different periodicity options, and anything other than daily will blow up a mutation. But, in our situation the period returns periodicity is the same as the stock prices periodicity (both daily), so we can use either. We want to use the adjusted closing prices column (adjusted for stock splits, which can make it appear that a stock is performing poorly if a split is included), so we set `x_fun = Ad`. We researched the `periodReturn` function, and we found that it accepts `type = "log"` and `period = "daily"`, which returns the daily log returns. 



```r
AAPL %>%
    tq_transform(x_fun = Ad, transform_fun = periodReturn, 
                 type = "log", period = "daily")
```

```
## # A tibble: 2,768 × 2
##          date daily.returns
##        <dttm>         <dbl>
## 1  2006-01-03   0.000000000
## 2  2006-01-04   0.002938752
## 3  2006-01-05  -0.007900889
## 4  2006-01-06   0.025485766
## 5  2006-01-09  -0.003281888
## 6  2006-01-10   0.061328315
## 7  2006-01-11   0.036906288
## 8  2006-01-12   0.004637594
## 9  2006-01-13   0.015305252
## 10 2006-01-17  -0.010334731
## # ... with 2,758 more rows
```

#### Example 3: Adding MACD and Bollinger Bands to a OHLC data set

In reviewing the available options in the `TTR` package, we see that `MACD` and `BBands` functions will get us where we need to be. In researching the documentation, the return is in the same periodicity as the input and the functions work with OHLC functions, so we can use `tq_mutate()`. MACD requires a price, so we select close using `Cl`, BBands requires high, low, and close, prices so we use `HLC`. We can chain the inputs together using the pipe (`%>%`) since mutate just adds columns. The result is a tibble containing the MACD and Bollinger Band results. 


```r
AAPL %>%
    tq_mutate(Cl, MACD) %>%
    tq_mutate(HLC, BBands)
```

```
## # A tibble: 2,768 × 13
##          date  open  high   low close    volume  adjusted  macd signal
##        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl>
## 1  2006-01-03 72.38 74.75 72.25 74.75 201808600  9.726565    NA     NA
## 2  2006-01-04 75.13 75.98 74.50 74.97 154900900  9.755191    NA     NA
## 3  2006-01-05 74.83 74.90 73.75 74.38 112355600  9.678420    NA     NA
## 4  2006-01-06 75.25 76.70 74.55 76.30 176114400  9.928252    NA     NA
## 5  2006-01-09 76.73 77.20 75.74 76.05 168760200  9.895722    NA     NA
## 6  2006-01-10 76.25 81.89 75.83 80.86 569967300 10.521606    NA     NA
## 7  2006-01-11 83.84 84.80 82.59 83.90 373448600 10.917174    NA     NA
## 8  2006-01-12 84.97 86.40 83.62 84.29 320202400 10.967921    NA     NA
## 9  2006-01-13 84.99 86.01 84.60 85.59 194076400 11.137079    NA     NA
## 10 2006-01-17 85.70 86.38 83.87 84.71 208905900 11.022573    NA     NA
## # ... with 2,758 more rows, and 4 more variables: dn <dbl>, mavg <dbl>,
## #   up <dbl>, pctB <dbl>
```

Note that for the MACD, we could have used `tq_mutate_xy()`, setting `.x = close`. However, for the BBands, we are forced to use `tq_mutate()` because of the HLC input.

#### Example 4: Getting the Percentage Difference Between Open and Close from Zero to Five Periods 

We can't use the `OpCl` function for this task since it only returns the percentage difference for a period lag of zero. We keep digging and we find the base `Delt` function from quantmod. In researching the function, we see that `Delt` takes one or two inputs, `k` a series of lags, and the type of difference, either arithmetic or log. We will set `.x = open` and `.y = close` and `k = 0:5` to get zero through five periods. The default `type = "arithmetic"` is acceptable, so there is no need to specify. The result is the percentage difference between the open and close prices for periods zero to five.


```r
AAPL %>%
    tq_mutate_xy(.x = open, .y = close, mutate_fun = Delt, k = 0:5) %>%
    select(-c(high, low, volume, adjusted))
```

```
## # A tibble: 2,768 × 9
##          date  open close Delt.0.arithmetic Delt.1.arithmetic
##        <date> <dbl> <dbl>             <dbl>             <dbl>
## 1  2006-01-03 72.38 74.75      0.0327438653                NA
## 2  2006-01-04 75.13 74.97     -0.0021296021       0.035783351
## 3  2006-01-05 74.83 74.38     -0.0060135910      -0.009982657
## 4  2006-01-06 75.25 76.30      0.0139534485       0.019644528
## 5  2006-01-09 76.73 76.05     -0.0088622703       0.010631203
## 6  2006-01-10 76.25 80.86      0.0604590009       0.053825127
## 7  2006-01-11 83.84 83.90      0.0007155892       0.100327799
## 8  2006-01-12 84.97 84.29     -0.0080028479       0.005367330
## 9  2006-01-13 84.99 85.59      0.0070596538       0.007296705
## 10 2006-01-17 85.70 84.71     -0.0115518788      -0.003294505
## # ... with 2,758 more rows, and 4 more variables: Delt.2.arithmetic <dbl>,
## #   Delt.3.arithmetic <dbl>, Delt.4.arithmetic <dbl>,
## #   Delt.5.arithmetic <dbl>
```

For comparison we'll inspect the output from the `OpCl()` function using `tq_mutate()`. We send OHLC prices to the OpCl function. As expected the `OpCl..` column returned is the same as `Delt.0.arithmetic` from above.


```r
AAPL %>%
    tq_mutate(OHLC, OpCl) %>%
    select(-c(high, low, volume, adjusted))
```

```
## # A tibble: 2,768 × 4
##          date  open close        OpCl..
##        <date> <dbl> <dbl>         <dbl>
## 1  2006-01-03 72.38 74.75  0.0327438653
## 2  2006-01-04 75.13 74.97 -0.0021296021
## 3  2006-01-05 74.83 74.38 -0.0060135910
## 4  2006-01-06 75.25 76.30  0.0139534485
## 5  2006-01-09 76.73 76.05 -0.0088622703
## 6  2006-01-10 76.25 80.86  0.0604590009
## 7  2006-01-11 83.84 83.90  0.0007155892
## 8  2006-01-12 84.97 84.29 -0.0080028479
## 9  2006-01-13 84.99 85.59  0.0070596538
## 10 2006-01-17 85.70 84.71 -0.0115518788
## # ... with 2,758 more rows
```


<a class="anchor" id="built-for-scale"></a>

## Designed to be Scaled with the `tidyverse` Workflow

Each function has one primary input and one output. This allows chaining operations with the pipe (`%>%`), and mapping to extend to lists of many stocks, exchange rates, metals, economic data, financial statements, etc. The rationale behind this is simple: let the function handle the operation, let the `tidyverse` handle the iteration. 

Rather than explain, let's go through a simple workflow using the `tidyverse`. We setup a two step workflow:

1. Analyze a single stock
2. Scale to many stocks

### Analyze a Single Stock

In our hypothetical situation, we want to compare the mean monthly log returns (MMLR). First, let's come up with a function to help us collect log returns. The function below performs three operations internally. It first gets the stock prices using `tq_get()`. Then, it transforms the stock prices to period returns using `tq_transform()`. We add the `type = "log"` and `period = "monthly"` arguments to ensure we retrieve a tibble of monthly log returns. Last, we take the mean of the monthly returns to get MMLR.


```r
my_stock_analysis_fun <- function(stock.symbol) {
    period.returns <- stock.symbol %>%
        tq_get(get = "stock.prices") %>%
        tq_transform(x_fun = Ad, transform_fun = periodReturn, 
                     type = "log", period = "monthly")
    mean(period.returns$monthly.returns)
}
```

And, let's test it out. We now have the mean monthly log returns over the past ten years.


```r
my_stock_analysis_fun("AAPL")
```

```
## [1] 0.01882578
```



### Extrapolate to Many Stocks using `tidyverse`

Now that we have one stock down, we can scale to many stocks. For brevity, we'll randomly sample ten stocks from the S&amp;P500 with a call to `dplyr::sample_n()`.


```r
set.seed(100)
stocks <- tq_get("SP500", get = "stock.index") %>%
    sample_n(10)
stocks
```

```
## # A tibble: 10 × 2
##    symbol            company
##     <chr>              <chr>
## 1     EMC                EMC
## 2    XRAY      DENTSPLY INTL
## 3     MNK   MALLINCKRODT PLC
## 4     AIG      AMERICAN INTL
## 5    INTC              INTEL
## 6     IVZ            INVESCO
## 7      SE     SPECTRA ENERGY
## 8    FLIR       FLIR SYSTEMS
## 9       L              LOEWS
## 10    CNP CENTERPOINT ENERGY
```

We can now apply our analysis function to the stocks using `dplyr::mutate` and `purrr::map_dbl`. The `mutate()` function adds a column to our tibble, and the `map_dbl()` function maps our `my_stock_analysis_fun` to our tibble of stocks using the `symbol` column.


```r
stocks <- stocks %>%
    mutate(mmlr = map_dbl(symbol, my_stock_analysis_fun)) %>%
    arrange(desc(mmlr))
stocks
```

```
## # A tibble: 10 × 3
##    symbol            company         mmlr
##     <chr>              <chr>        <dbl>
## 1    FLIR       FLIR SYSTEMS  0.009394565
## 2     CNP CENTERPOINT ENERGY  0.008708440
## 3     IVZ            INVESCO  0.006947423
## 4      SE     SPECTRA ENERGY  0.006594122
## 5    XRAY      DENTSPLY INTL  0.006279602
## 6     EMC                EMC  0.006210331
## 7    INTC              INTEL  0.005244692
## 8       L              LOEWS  0.003391356
## 9     MNK   MALLINCKRODT PLC  0.002221213
## 10    AIG      AMERICAN INTL -0.021056917
```

And, we're done! We now have the MMLR for 10-years of stock data for 10 stocks. And, we can easily extend this to larger lists or stock indexes. For example, the entire S&amp;P500 could be analyzed removing the `sample_n()` following the call to `tq_get("SP500", get = "stock.index")`.

### Function `tq_get()` Designed to Handle Errors Gracefully

Eventually you will run into a stock index, stock symbol, FRED data code, etc that cannot be retrieved. Possible reasons are: 

* The website changes
* An index becomes out of date
* A company goes private
* A stock ticker symbol changes
* Yahoo / FRED just doesn't like your stock symbol / FRED code

This becomes painful when scaling if the functions return errors. So, the `tq_get()` function is designed to handle errors gracefully. What this means is a `NA` value is returned when an error is generated along with a gentle error warning. There are pros and cons to this approach that you may not agree with but I believe helps in the long run. Just be aware of what happens:

* __Pros:__ Long running scripts are not interrupted because of one error

* __Cons:__ Errors flow downstream if not looking at warnings and not reviewing results


__With `tq_get()`, Bad Apples Fail Gracefully:__

Let's see an example when mapping to `tq_get()` to a long list of stocks with one `BAD APPLE`.


```r
stock_list_with_one_bad_apple <- tibble( 
    symbol = c("AAPL", "GOOG", "AMZN", "FB", "BAD APPLE",
               "AVGO", "SWKS","NVDA", "V", "MA")
)
stock_list_with_one_bad_apple <- stock_list_with_one_bad_apple %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))
```

```
## Warning in value[[3L]](cond): Error at stock symbol BAD APPLE during call
## to quantmod::getSymbols.
```

We get warned that there was an issue in the operation. With that said, we still get the full list of stocks.


```r
stock_list_with_one_bad_apple
```

```
## # A tibble: 10 × 2
##       symbol         stock.prices
##        <chr>               <list>
## 1       AAPL <tibble [2,768 × 7]>
## 2       GOOG <tibble [2,768 × 7]>
## 3       AMZN <tibble [2,768 × 7]>
## 4         FB <tibble [1,162 × 7]>
## 5  BAD APPLE            <lgl [1]>
## 6       AVGO <tibble [1,864 × 7]>
## 7       SWKS <tibble [2,768 × 7]>
## 8       NVDA <tibble [2,768 × 7]>
## 9          V <tibble [2,213 × 7]>
## 10        MA <tibble [2,669 × 7]>
```


Say hypothetically we didn't recognize the error message. An error shows up during the next operation. As an example, we'll attempt to get yearly period returns using `tq_transform`. The operation is wrapped in a `tryCatch()` statement to enable printing the error message. 


```r
tryCatch({
    stock_list_with_one_bad_apple %>%
    mutate(annual.returns = map(.x = stock.prices, 
                                ~ tq_transform(.x,
                                               x_fun = Ad, 
                                               transform_fun = periodReturn, 
                                               period = "yearly")
                                )
           )
}, error = function(e) {
    print(e)
})
```

```
## <Rcpp::eval_error in eval(substitute(expr), envir, enclos): `data` must be a tibble or data.frame.>
```

The operation grinds to a hault because the `BAD APPLE` tried to send its value for stock.prices of `NA` to the `tq_transform()` function. The error message tells us that `data` is not a `tibble` or `data.frame`.

The rationale behind the error handling approach is that long-running scripts should not fail during minor issues. For example, if you have a list of 3000 stocks and the 3000th is bad, the program could take 20+ minutes to fail. This is disheartening. We allow `tq_get()` to continue to fetch data even if an error is encountered. Failure occurs during `tq_transform()` and `tq_mutate()` to prevent the error from getting too far downstream. 

Recognizing how `tq_get()` works (and gracefully fails), we can adjust our workflow. It's a good idea to collect stock information in one independent step, review any warnings / errors, and remove "bad apples" if present before moving on to any transformations or mutations.

Here's an example of a good workflow:


```r
stock_list_with_one_bad_apple <- tibble( 
    symbol = c("AAPL", "GOOG", "AMZN", "FB", "BAD APPLE",
               "AVGO", "SWKS","NVDA", "V", "MA")
    ) %>%
    # Step 1: Get stock prices
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")),
           class = map_chr(stock.prices, ~ class(.x)[[1]])) %>%
    # Step 2: Filter out errors; errors have a class of "logical"
    filter(class != "logical") %>%
    select(-class) %>%
    # Step 3: Perform period returns
    mutate(annual.returns = map(.x = stock.prices, 
                                ~ tq_transform(.x,
                                               x_fun = Ad, 
                                               transform_fun = periodReturn, 
                                               period = "yearly")
                                )
           )
stock_list_with_one_bad_apple
```

```
## # A tibble: 9 × 3
##   symbol         stock.prices    annual.returns
##    <chr>               <list>            <list>
## 1   AAPL <tibble [2,768 × 7]> <tibble [11 × 2]>
## 2   GOOG <tibble [2,768 × 7]> <tibble [11 × 2]>
## 3   AMZN <tibble [2,768 × 7]> <tibble [11 × 2]>
## 4     FB <tibble [1,162 × 7]>  <tibble [5 × 2]>
## 5   AVGO <tibble [1,864 × 7]>  <tibble [8 × 2]>
## 6   SWKS <tibble [2,768 × 7]> <tibble [11 × 2]>
## 7   NVDA <tibble [2,768 × 7]> <tibble [11 × 2]>
## 8      V <tibble [2,213 × 7]>  <tibble [9 × 2]>
## 9     MA <tibble [2,669 × 7]> <tibble [11 × 2]>
```


__Fall Back for Stock Indexes:__

There's a fallback for the stock indexes too. Since the source, www.marketvolume.com, could change over time, an option is provided to pull stored data within the `tidyquant` package. The downside is that the data is only as accurate as the last update to `tidyquant`. Here's how to get the stock indexes locally if for some reason the website is down or has changed.


```r
tq_get("SP500", get = "stock.index", use_fallback = TRUE)
```

```
## Using fallback dataset last downloaded 2016-12-23.
```

```
## # A tibble: 501 × 2
##    symbol                   company
##     <chr>                     <chr>
## 1     MMM                        3M
## 2     ABT       ABBOTT LABORATORIES
## 3    ABBV                ABBVIE INC
## 4     ACN                 ACCENTURE
## 5    ATVI       ACTIVISION BLIZZARD
## 6     AYI             ACUITY BRANDS
## 7    ADBE             ADOBE SYSTEMS
## 8     AAP        ADVANCE AUTO PARTS
## 9     AET                     AETNA
## 10    AMG AFFILIATED MANAGERS GROUP
## # ... with 491 more rows
```


# Recap

Hopefully now you see how `tidyquant` helps to integrate the best quantitative financial analysis packages with the `tidyverse`. The benefits are:

* A few core functions with a lot of power, that
* leverage the quantitative analysis power of `xts`, `quantmod` and `TTR`, and are
* designed to be scaled with the `tidyverse` workflow.

With a few, easy-to-use core functions, you can efficiently leverage the quantitative power of `xts`, `quantmod` and `TTR` with the data management infrastructure and scale-ability of the `tidyverse`. 

