
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyquant
=========

[![Travis-CI Build Status](https://travis-ci.org/mdancho84/tidyquant.svg?branch=master)](https://travis-ci.org/mdancho84/tidyquant) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidyquant)](https://cran.r-project.org/package=tidyquant) <!-- ![](http://cranlogs.r-pkg.org/badges/tidyquant?color=brightgreen)  --> <!-- ![](http://cranlogs.r-pkg.org/badges/grand-total/tidyquant?color=brightgreen) -->

> Bringing financial analysis to the tidyverse

`tidyquant` integrates the best resources for collecting and analyzing financial data, `zoo`, `xts`, `quantmod` and `TTR`, with the tidy data infrastructure of the `tidyverse` allowing for seamless interaction between each. You can now perform complete financial analyses in the `tidyverse`.

Benefits
--------

**The `tidyquant` philosophy:**

-   **A few core functions with a lot of power, that**
-   **leverage the quantitative analysis power of `zoo`, `xts`, `quantmod` and `TTR`, and are**
-   **designed to be used and scaled with the `tidyverse`.**

Installation
------------

Development Version with Latest Features:

``` r
# install.packages("devtools")
devtools::install_github("mdancho84/tidyquant")
```

CRAN Approved Version:

``` r
install.packages("tidyquant")
```

Examples
--------

Start by loading `tidyquant`.

``` r
# Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts/zoo
library(tidyquant) 
```

### Getting Data

`tq_get()` is the one-stop shop for retrieving data. The full list of get options are:

``` r
tq_get_options()
#> [1] "stock.prices"   "stock.index"    "dividends"      "splits"        
#> [5] "financials"     "key.ratios"     "economic.data"  "exchange.rates"
#> [9] "metal.prices"
```

**Stock Prices**:

Set `get = "stock.prices"` to get stock prices. Notice the output is *always* a `tibble`.

``` r
aapl_prices <- tq_get("AAPL", get = "stock.prices")
aapl_prices
#> # A tibble: 2,527 × 7
#>          date  open  high   low close    volume adjusted
#>        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#> 1  2007-01-03 86.29 86.58 81.90 83.80 309579900 10.90416
#> 2  2007-01-04 84.05 85.95 83.82 85.66 211815100 11.14619
#> 3  2007-01-05 85.77 86.20 84.40 85.05 208685400 11.06681
#> 4  2007-01-08 85.96 86.53 85.28 85.47 199276700 11.12147
#> 5  2007-01-09 86.45 92.98 85.15 92.57 837324600 12.04533
#> 6  2007-01-10 94.75 97.80 93.45 97.00 738220000 12.62176
#> 7  2007-01-11 95.94 96.78 95.10 95.80 360063200 12.46562
#> 8  2007-01-12 94.59 95.06 93.23 94.62 328172600 12.31207
#> 9  2007-01-16 95.68 97.25 95.45 97.10 311019100 12.63477
#> 10 2007-01-17 97.56 97.60 94.82 94.95 411565000 12.35501
#> # ... with 2,517 more rows
```

Get data for multiple stocks:

``` r
tq_get(c("AAPL", "GOOG", "FB"), get = "stock.prices")
#> # A tibble: 6,226 × 8
#>    symbol.x       date  open  high   low close    volume adjusted
#>       <chr>     <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#> 1      AAPL 2007-01-03 86.29 86.58 81.90 83.80 309579900 10.90416
#> 2      AAPL 2007-01-04 84.05 85.95 83.82 85.66 211815100 11.14619
#> 3      AAPL 2007-01-05 85.77 86.20 84.40 85.05 208685400 11.06681
#> 4      AAPL 2007-01-08 85.96 86.53 85.28 85.47 199276700 11.12147
#> 5      AAPL 2007-01-09 86.45 92.98 85.15 92.57 837324600 12.04533
#> 6      AAPL 2007-01-10 94.75 97.80 93.45 97.00 738220000 12.62176
#> 7      AAPL 2007-01-11 95.94 96.78 95.10 95.80 360063200 12.46562
#> 8      AAPL 2007-01-12 94.59 95.06 93.23 94.62 328172600 12.31207
#> 9      AAPL 2007-01-16 95.68 97.25 95.45 97.10 311019100 12.63477
#> 10     AAPL 2007-01-17 97.56 97.60 94.82 94.95 411565000 12.35501
#> # ... with 6,216 more rows
```

**Financial Statements**:

Set `get = "financials"` to get financial statements. The statements are returned as nested tibbles, that can be unnested and analyzed together.

``` r
tq_get("AAPL", get = "financials")
#> # A tibble: 3 × 3
#>    type             annual            quarter
#> * <chr>             <list>             <list>
#> 1    BS <tibble [168 × 4]> <tibble [210 × 4]>
#> 2    CF  <tibble [76 × 4]>  <tibble [76 × 4]>
#> 3    IS <tibble [196 × 4]> <tibble [245 × 4]>
```

Get financials for multiple stocks:

``` r
tq_get(c("AAPL", "GOOG", "FB"), get = "financials")
#> # A tibble: 9 × 4
#>   symbol.x  type             annual            quarter
#>      <chr> <chr>             <list>             <list>
#> 1     AAPL    BS <tibble [168 × 4]> <tibble [210 × 4]>
#> 2     AAPL    CF  <tibble [76 × 4]>  <tibble [76 × 4]>
#> 3     AAPL    IS <tibble [196 × 4]> <tibble [245 × 4]>
#> 4     GOOG    BS <tibble [168 × 4]> <tibble [210 × 4]>
#> 5     GOOG    CF  <tibble [76 × 4]>  <tibble [76 × 4]>
#> 6     GOOG    IS <tibble [196 × 4]> <tibble [245 × 4]>
#> 7       FB    BS <tibble [168 × 4]> <tibble [210 × 4]>
#> 8       FB    CF  <tibble [76 × 4]>  <tibble [76 × 4]>
#> 9       FB    IS <tibble [196 × 4]> <tibble [245 × 4]>
```

**Key Ratios**:

Set `get = "key.ratios"` to get 10 years of 89 different key ratios (e.g. P/E, P/S, EPS, ROA, ROE, current ratio, debt/equity, inventory turnover, and many more), separated into seven primary sections.

``` r
tq_get("AAPL", get = "key.ratios")
#> # A tibble: 7 × 2
#>             section               data
#>               <chr>             <list>
#> 1        Financials <tibble [150 × 5]>
#> 2     Profitability <tibble [170 × 5]>
#> 3            Growth <tibble [160 × 5]>
#> 4         Cash Flow  <tibble [50 × 5]>
#> 5  Financial Health <tibble [240 × 5]>
#> 6 Efficiency Ratios  <tibble [80 × 5]>
#> 7  Valuation Ratios  <tibble [40 × 5]>
```

Get key ratios for multiple stocks:

``` r
tq_get(c("AAPL", "GOOG", "FB"), get = "key.ratios")
#> # A tibble: 21 × 3
#>    symbol.x           section               data
#>       <chr>             <chr>             <list>
#> 1      AAPL        Financials <tibble [150 × 5]>
#> 2      AAPL     Profitability <tibble [170 × 5]>
#> 3      AAPL            Growth <tibble [160 × 5]>
#> 4      AAPL         Cash Flow  <tibble [50 × 5]>
#> 5      AAPL  Financial Health <tibble [240 × 5]>
#> 6      AAPL Efficiency Ratios  <tibble [80 × 5]>
#> 7      AAPL  Valuation Ratios  <tibble [40 × 5]>
#> 8      GOOG        Financials <tibble [150 × 5]>
#> 9      GOOG     Profitability <tibble [170 × 5]>
#> 10     GOOG            Growth <tibble [160 × 5]>
#> # ... with 11 more rows
```

**Stock Indexes**:

There are 18 indexes available to select from, which can be seen with a call to `tq_get_stock_index_options()`.

``` r
tq_get_stock_index_options()
#>  [1] "DOWJONES"    "DJI"         "DJT"         "DJU"         "SP100"      
#>  [6] "SP400"       "SP500"       "SP600"       "RUSSELL1000" "RUSSELL2000"
#> [11] "RUSSELL3000" "AMEX"        "AMEXGOLD"    "AMEXOIL"     "NASDAQ"     
#> [16] "NASDAQ100"   "NYSE"        "SOX"
```

Set `x` to one of the options and `get = "stock.index"` to get a full list of stocks within the specified index.

``` r
tq_get("SP500", get = "stock.index")
#> Getting data...
#> # A tibble: 501 × 2
#>    symbol                   company
#>     <chr>                     <chr>
#> 1     MMM                        3M
#> 2     ABT       ABBOTT LABORATORIES
#> 3    ABBV                ABBVIE INC
#> 4     ACN                 ACCENTURE
#> 5    ATVI       ACTIVISION BLIZZARD
#> 6     AYI             ACUITY BRANDS
#> 7    ADBE             ADOBE SYSTEMS
#> 8     AAP        ADVANCE AUTO PARTS
#> 9     AET                     AETNA
#> 10    AMG AFFILIATED MANAGERS GROUP
#> # ... with 491 more rows
```

Combine `tq_get` stock index and stock price options to get the prices for **every stock in an index**. *Note that this may take several minutes due to the number of stock prices being generated (results not shown because of this).*

``` r
sp_500_prices <- tq_get("SP500", get = "stock.index") %>%
    tq_get(get = "stock.prices")
```

**Other Options**:

There are many other get options including **dividends**, **splits**, **economic data** from the FRED, and **exchange rates** and **metal prices** from Oanda.

### Working in the tidyverse

You may already know and love `tidyverse` packages like `ggplot2`, `dplyr`, `tidyr`, `purrr`, `readr`, and `tibble` along with `lubridate` for working with date and datetime. `tidyquant` works solely in tibbles, so all of the `tidyverse` functionality is intact.

A simple example inspired by [Kan Nishida's blog](https://blog.exploratory.io/introducing-time-series-analysis-with-dplyr-60683587cf8a#.w6pvyi3d2) shows the `dplyr` and `lubridate` capability: Say we want the growth in the stock over the past year. We can do this with `dplyr` operations.

Getting the last year is simple with `dplyr` and `lubridate`. We first `select` the date and adjusted price (adjusted for stock splits). We then `filter` using `lubridate` date functions. We can use the `mutate` to add columns to the data frame: Add the baseline price using the `first` function, add the growth and growth percent versus baseline columns using standard mathematical operations. We tack on a final `select` statement to remove unnecessary columns. The final workflow looks like this:

``` r
aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1)) %>%
    mutate(baseline = first(adjusted),
           growth = adjusted - baseline,
           growth_pct = growth / baseline * 100) %>%
    select(-(baseline:growth))
#> # A tibble: 251 × 3
#>          date adjusted growth_pct
#>        <date>    <dbl>      <dbl>
#> 1  2016-01-19 94.55621  0.0000000
#> 2  2016-01-20 94.68337  0.1344893
#> 3  2016-01-21 94.20404 -0.3724399
#> 4  2016-01-22 99.21260  4.9244722
#> 5  2016-01-25 97.27570  2.8760587
#> 6  2016-01-26 97.81372  3.4450589
#> 7  2016-01-27 91.38672 -3.3519609
#> 8  2016-01-28 92.04213 -2.6588112
#> 9  2016-01-29 95.22140  0.7034895
#> 10 2016-02-01 94.33121 -0.2379505
#> # ... with 241 more rows
```

### Transforming & Mutating Data with zoo, xts, quantmod, and TTR Functions

You may already know and love `zoo`, `xts`, `quantmod`, and `TTR`, which is why the core functionality is fully integrated. The workhorse functions, `tq_transform()` and `tq_mutate()`, apply `zoo`, `xts`, `quantmod`, and `TTR` functions to `tibbles`. The full list of compatible functions are shown with a call to `tq_transform_fun_options()`. Remove the `%>% str()` to expand the list.

``` r
tq_transform_fun_options() %>% str()
#> List of 4
#>  $ zoo     : chr [1:14] "rollapply" "rollapplyr" "rollmax" "rollmax.default" ...
#>  $ xts     : chr [1:27] "apply.daily" "apply.monthly" "apply.quarterly" "apply.weekly" ...
#>  $ quantmod: chr [1:25] "allReturns" "annualReturn" "ClCl" "dailyReturn" ...
#>  $ TTR     : chr [1:61] "adjRatios" "ADX" "ALMA" "aroon" ...
```

#### tq\_transform

`tq_transform()` returns a new data set that can either be in the same periodicity or a different periodicity as the original data set. Let's use `tq_transform` to transform the periodicity of the `aapl_prices`. The `quantmod` OHLC codes are used to select the open, high, low, close, and volume (OHLCV) columns, which are then sent to the transformation function, `xts::to.period`, for transformation to monthly periodicity. We now have a much smaller data set containing the monthly prices.

``` r
aapl_prices %>%
    tq_transform(ohlc_fun = OHLCV, transform_fun = to.period, period = "months")
#> # A tibble: 121 × 6
#>          date   open   high    low  close    volume
#>        <dttm>  <dbl>  <dbl>  <dbl>  <dbl>     <dbl>
#> 1  2007-01-31  84.86  86.00  84.35  85.73 214017300
#> 2  2007-02-28  83.00  85.60  83.00  84.61 229868800
#> 3  2007-03-30  94.28  94.68  92.75  92.91 150139500
#> 4  2007-04-30 100.09 101.00  99.67  99.80 154127400
#> 5  2007-05-31 120.07 122.17 119.54 121.19 324266600
#> 6  2007-06-29 121.97 124.00 121.09 122.04 284460400
#> 7  2007-07-31 142.97 143.48 131.52 131.76 440598200
#> 8  2007-08-31 139.49 139.65 137.41 138.48 219221800
#> 9  2007-09-28 153.44 154.60 152.75 153.47 153775300
#> 10 2007-10-31 187.63 190.12 184.95 189.95 208327700
#> # ... with 111 more rows
```

#### tq\_mutate

The cousin of `tq_transform()` is `tq_mutate()`. While `tq_transform()` produces a new, transformed data set, `tq_mutate()` modifies the existing data set. This is very useful for applying `TTR` functions like `BBands`, `MACD`, Moving Averages, etc. There is one caveat: the mutation must be in the same periodicity as the original data set (otherwise you can't add columns because the rows will not match up). Let's use `tq_mutate()` to add some Bollinger Bands and MACD using the closing prices (Cl in OHLC notation).

``` r
aapl_prices %>%
    tq_mutate(ohlc_fun = Cl, mutate_fun = MACD) %>%
    tq_mutate(ohlc_fun = HLC, mutate_fun = BBands)
#> # A tibble: 2,527 × 13
#>          date  open  high   low close    volume adjusted  macd signal
#>        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>  <dbl>
#> 1  2007-01-03 86.29 86.58 81.90 83.80 309579900 10.90416    NA     NA
#> 2  2007-01-04 84.05 85.95 83.82 85.66 211815100 11.14619    NA     NA
#> 3  2007-01-05 85.77 86.20 84.40 85.05 208685400 11.06681    NA     NA
#> 4  2007-01-08 85.96 86.53 85.28 85.47 199276700 11.12147    NA     NA
#> 5  2007-01-09 86.45 92.98 85.15 92.57 837324600 12.04533    NA     NA
#> 6  2007-01-10 94.75 97.80 93.45 97.00 738220000 12.62176    NA     NA
#> 7  2007-01-11 95.94 96.78 95.10 95.80 360063200 12.46562    NA     NA
#> 8  2007-01-12 94.59 95.06 93.23 94.62 328172600 12.31207    NA     NA
#> 9  2007-01-16 95.68 97.25 95.45 97.10 311019100 12.63477    NA     NA
#> 10 2007-01-17 97.56 97.60 94.82 94.95 411565000 12.35501    NA     NA
#> # ... with 2,517 more rows, and 4 more variables: dn <dbl>, mavg <dbl>,
#> #   up <dbl>, pctB <dbl>
```

#### tq\_tranform\_xy and tq\_mutate\_xy

The "xy" variants are useful in situations where (1) you have two inputs (hence x and y) that don't fit into the OHLC function, or (2) you are working with a single column of non-OHLC data.

**Two inputs that don't fit OHLC mold**:

``` r
aapl_prices %>%
    tq_mutate_xy(x = close, y = volume, mutate_fun = EVWMA, col_rename = "EVWMA")
#> # A tibble: 2,527 × 8
#>          date  open  high   low close    volume adjusted EVWMA
#>        <date> <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <dbl>
#> 1  2007-01-03 86.29 86.58 81.90 83.80 309579900 10.90416    NA
#> 2  2007-01-04 84.05 85.95 83.82 85.66 211815100 11.14619    NA
#> 3  2007-01-05 85.77 86.20 84.40 85.05 208685400 11.06681    NA
#> 4  2007-01-08 85.96 86.53 85.28 85.47 199276700 11.12147    NA
#> 5  2007-01-09 86.45 92.98 85.15 92.57 837324600 12.04533    NA
#> 6  2007-01-10 94.75 97.80 93.45 97.00 738220000 12.62176    NA
#> 7  2007-01-11 95.94 96.78 95.10 95.80 360063200 12.46562    NA
#> 8  2007-01-12 94.59 95.06 93.23 94.62 328172600 12.31207    NA
#> 9  2007-01-16 95.68 97.25 95.45 97.10 311019100 12.63477    NA
#> 10 2007-01-17 97.56 97.60 94.82 94.95 411565000 12.35501 94.95
#> # ... with 2,517 more rows
```

**Working with a single column of non-OHLC data**:

``` r
tq_get("GDPC1", get = "economic.data") %>%
    tq_mutate_xy(x = price, mutate_fun = rollapply, width = 5, FUN = mean)
#> # A tibble: 39 × 3
#>          date   price rollapply
#>        <date>   <dbl>     <dbl>
#> 1  2007-01-01 14726.0        NA
#> 2  2007-04-01 14838.7        NA
#> 3  2007-07-01 14938.5        NA
#> 4  2007-10-01 14991.8        NA
#> 5  2008-01-01 14889.5  14876.90
#> 6  2008-04-01 14963.4  14924.38
#> 7  2008-07-01 14891.6  14934.96
#> 8  2008-10-01 14577.0  14862.66
#> 9  2009-01-01 14375.0  14739.30
#> 10 2009-04-01 14355.6  14632.52
#> # ... with 29 more rows
```

### Scaling with the tidyverse

All functions return data sets as `tibbles`, which allows for interaction within the `tidyverse`. This means we can:

-   Use `dplyr` and `tidyr` to select, filter, nest/unnest, group\_by, etc.
-   Use the pipe (`%>%`) for chaining operations.
-   Seamlessly scale data retrieval and transformations/mutations using `purrr` to map functions or `dplyr` to combine `group_by` with `tq_mutate` or `tq_transform`.

#### Getting Financial Data for Multiple Stocks

A very basic example is retrieving the stock prices for multiple stocks. There are three primary ways to do this:

**Method 1: Map a character vector with multiple stock symbols**

``` r
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")
#> # A tibble: 756 × 8
#>    symbol.x       date   open   high    low  close   volume  adjusted
#>       <chr>     <date>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>     <dbl>
#> 1      AAPL 2016-01-04 102.61 105.37 102.00 105.35 67649400 103.05706
#> 2      AAPL 2016-01-05 105.75 105.85 102.41 102.71 55791000 100.47452
#> 3      AAPL 2016-01-06 100.56 102.37  99.87 100.70 68457400  98.50827
#> 4      AAPL 2016-01-07  98.68 100.13  96.43  96.45 81094400  94.35077
#> 5      AAPL 2016-01-08  98.55  99.11  96.76  96.96 70798000  94.84967
#> 6      AAPL 2016-01-11  98.97  99.06  97.34  98.53 49739400  96.38550
#> 7      AAPL 2016-01-12 100.55 100.69  98.84  99.96 49154200  97.78438
#> 8      AAPL 2016-01-13 100.32 101.19  97.30  97.39 62439600  95.27031
#> 9      AAPL 2016-01-14  97.96 100.48  95.74  99.52 63170100  97.35395
#> 10     AAPL 2016-01-15  96.20  97.71  95.36  97.13 79010000  95.01597
#> # ... with 746 more rows
```

The output is a single level tibble with all or the stock prices in one tibble.

**Method 2: Map a tibble with stocks in first column**

First, obtain a tibble of stocks. The stock symbols must be in the first column.

``` r
stock_list <- tibble(symbols = c("AAPL", "JPM", "CVX"),
                     industry = c("Technology", "Financial", "Energy"))
stock_list
#> # A tibble: 3 × 2
#>   symbols   industry
#>     <chr>      <chr>
#> 1    AAPL Technology
#> 2     JPM  Financial
#> 3     CVX     Energy
```

Second, send the stock list to `tq_get`. Notice how the symbol and industry columns are expanded the length of the stock prices.

``` r
stock_list %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")
#> # A tibble: 756 × 9
#>    symbols   industry       date   open   high    low  close   volume
#>      <chr>      <chr>     <date>  <dbl>  <dbl>  <dbl>  <dbl>    <dbl>
#> 1     AAPL Technology 2016-01-04 102.61 105.37 102.00 105.35 67649400
#> 2     AAPL Technology 2016-01-05 105.75 105.85 102.41 102.71 55791000
#> 3     AAPL Technology 2016-01-06 100.56 102.37  99.87 100.70 68457400
#> 4     AAPL Technology 2016-01-07  98.68 100.13  96.43  96.45 81094400
#> 5     AAPL Technology 2016-01-08  98.55  99.11  96.76  96.96 70798000
#> 6     AAPL Technology 2016-01-11  98.97  99.06  97.34  98.53 49739400
#> 7     AAPL Technology 2016-01-12 100.55 100.69  98.84  99.96 49154200
#> 8     AAPL Technology 2016-01-13 100.32 101.19  97.30  97.39 62439600
#> 9     AAPL Technology 2016-01-14  97.96 100.48  95.74  99.52 63170100
#> 10    AAPL Technology 2016-01-15  96.20  97.71  95.36  97.13 79010000
#> # ... with 746 more rows, and 1 more variables: adjusted <dbl>
```

**Method 3: Use purrr to map a function**

We can pipe a tibble of stock symbols to a mutation that maps the `tq_get(get = "stock.prices")` function. The result is all of the stock prices in nested format.

``` r
tibble(symbol = c("AAPL", "GOOG", "AMZN", "FB", "AVGO", "SWKS","NVDA")) %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))
#> # A tibble: 7 × 2
#>   symbol         stock.prices
#>    <chr>               <list>
#> 1   AAPL <tibble [2,527 × 7]>
#> 2   GOOG <tibble [2,527 × 7]>
#> 3   AMZN <tibble [2,527 × 7]>
#> 4     FB <tibble [1,172 × 7]>
#> 5   AVGO <tibble [1,874 × 7]>
#> 6   SWKS <tibble [2,527 × 7]>
#> 7   NVDA <tibble [2,527 × 7]>
```

#### Manipulating Financial Data for Multiple Stocks

Once you get the data, you typically want to do something with it. You can easily do this at scale. Let's get the yearly returns for multiple stocks using `tq_transform`. First, get the prices. Second, use `group_by` to group by stock symbol. Third, apply the transformation. We can do this in one easy workflow:

``` r
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices") %>%
    group_by(symbol.x) %>%
    tq_transform(Ad, transform_fun = periodReturn, period = "yearly")
#> Source: local data frame [28 x 3]
#> Groups: symbol.x [3]
#> 
#>    symbol.x       date yearly.returns
#>       <chr>     <dttm>          <dbl>
#> 1      AAPL 2007-12-31     1.36372304
#> 2      AAPL 2008-12-31    -0.56911347
#> 3      AAPL 2009-12-31     1.46901003
#> 4      AAPL 2010-12-31     0.53067909
#> 5      AAPL 2011-12-30     0.25558032
#> 6      AAPL 2012-12-31     0.32566899
#> 7      AAPL 2013-12-31     0.08069481
#> 8      AAPL 2014-12-31     0.40622503
#> 9      AAPL 2015-12-31    -0.03013719
#> 10     AAPL 2016-12-30     0.12480428
#> # ... with 18 more rows
```

Further Information
-------------------

This just scratches the surface of the features. See the [`tidyquant` vignette](https://cran.r-project.org/web/packages/tidyquant/vignettes/tidyquant.html) for further details on the package.
