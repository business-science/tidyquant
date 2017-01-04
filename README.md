
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyquant
=========

[![Travis-CI Build Status](https://travis-ci.org/mdancho84/tidyquant.svg?branch=master)](https://travis-ci.org/mdancho84/tidyquant) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidyquant)](https://cran.r-project.org/package=tidyquant) <!-- ![](http://cranlogs.r-pkg.org/badges/tidyquant?color=brightgreen)  --> <!-- ![](http://cranlogs.r-pkg.org/badges/grand-total/tidyquant?color=brightgreen) -->

`tidyquant` integrates the best quantitative resources for collecting and analyzing quantitative data, `xts`, `quantmod` and `TTR`, with the tidy data infrastructure of the `tidyverse` allowing for seamless interaction between each and working within the `tidyverse`.

Benefits
--------

**The `tidyquant` philosophy:**

-   **A few core functions with a lot of power, that**
-   **leverage the quantitative analysis power of `xts`, `quantmod` and `TTR`, and are**
-   **designed to be used and scaled with the `tidyverse`.**

Installation
------------

To install from CRAN:

``` r
install.packages("tidyquant")
```

Or, to install development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("mdancho84/tidyquant")
```

Examples
--------

``` r
# Loads tidyquant, tidyverse, lubridate, quantmod, TTR, and xts
library(tidyquant) 
```

### Getting Data in Tibble Format:

`tq_get()` is the one-stop shop for retrieving data. The full list of get options are:

``` r
tq_get_options()
#> [1] "stock.prices"   "stock.index"    "dividends"      "splits"        
#> [5] "financials"     "economic.data"  "exchange.rates" "metal.prices"
```

Set `get = "stock.prices"` to get stock prices. Notice the output is *always* a `tibble`.

``` r
aapl_prices <- tq_get("AAPL", get = "stock.prices")
aapl_prices
#> # A tibble: 2,518 × 7
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
#> # ... with 2,508 more rows
```

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

There are many other get options including stock indexes, dividends, splits, economic data from the FRED, and exchange rates and metals from Oanda.

### Working in the tidyverse

You probably already know and love `tidyverse` packages like `dplyr`, `tidyr`, `purrr`, `readr`, and `tibble` along with `lubridate` for working with date and datetime. `tidyquant` works solely in tibbles, so all of the `tidyverse` functionality is intact.

A simple example inspired by [Kan Nishida's blog](https://blog.exploratory.io/introducing-time-series-analysis-with-dplyr-60683587cf8a#.w6pvyi3d2) shows the `dplyr` and `lubridate` capability: Say we want the growth in the stock over the past year. We can do this with `dplyr` operations.

Getting the last year is simple with `dplyr` and `lubridate`. We first `select` the date and adjusted price (adjusted for stock splits). We then `filter` using `lubridate` date functions. We can also get a baseline price using the `first` function. Growth and growth percent versus baseline columns can be added now. We tack on a final select statement to remove unnecessary columns. The final workflow looks like this:

``` r
aapl_prices %>%
    select(date, adjusted) %>%
    filter(date >= today() - years(1)) %>%
    mutate(baseline = first(adjusted),
           growth = adjusted - baseline,
           growth_pct = growth / baseline * 100) %>%
    select(-(baseline:growth))
#> # A tibble: 252 × 3
#>          date  adjusted growth_pct
#>        <date>     <dbl>      <dbl>
#> 1  2016-01-04 103.05706   0.000000
#> 2  2016-01-05 100.47452  -2.505932
#> 3  2016-01-06  98.50827  -4.413861
#> 4  2016-01-07  94.35077  -8.448032
#> 5  2016-01-08  94.84967  -7.963930
#> 6  2016-01-11  96.38550  -6.473659
#> 7  2016-01-12  97.78438  -5.116279
#> 8  2016-01-13  95.27031  -7.555766
#> 9  2016-01-14  97.35395  -5.533937
#> 10 2016-01-15  95.01597  -7.802565
#> # ... with 242 more rows
```

### Transforming & Mutating Data with xts, quantmod, and TTR Functions

You may already know and love `xts`, `quantmod`, and `TTR`, which is why the core functionality is fully integrated. The workhorse functions are `tq_transform()` and `tq_mutate()`. These functions leverage the power of `xts`, `quantmod`, and `TTR`. The full list of `xts`, `quantmod`, and `TTR` functions that can be used are:

``` r
tq_transform_fun_options()
#> $zoo
#>  [1] "rollapply"          "rollapplyr"         "rollmax"           
#>  [4] "rollmax.default"    "rollmaxr"           "rollmean"          
#>  [7] "rollmean.default"   "rollmeanr"          "rollmedian"        
#> [10] "rollmedian.default" "rollmedianr"        "rollsum"           
#> [13] "rollsum.default"    "rollsumr"          
#> 
#> $xts
#>  [1] "apply.daily"     "apply.monthly"   "apply.quarterly"
#>  [4] "apply.weekly"    "apply.yearly"    "diff.xts"       
#>  [7] "lag.xts"         "period.apply"    "period.max"     
#> [10] "period.min"      "period.prod"     "period.sum"     
#> [13] "periodicity"     "to.daily"        "to.hourly"      
#> [16] "to.minutes"      "to.minutes10"    "to.minutes15"   
#> [19] "to.minutes3"     "to.minutes30"    "to.minutes5"    
#> [22] "to.monthly"      "to.period"       "to.quarterly"   
#> [25] "to.weekly"       "to.yearly"       "to_period"      
#> 
#> $quantmod
#>  [1] "allReturns"      "annualReturn"    "ClCl"           
#>  [4] "dailyReturn"     "Delt"            "HiCl"           
#>  [7] "Lag"             "LoCl"            "LoHi"           
#> [10] "monthlyReturn"   "Next"            "OpCl"           
#> [13] "OpHi"            "OpLo"            "OpOp"           
#> [16] "periodReturn"    "quarterlyReturn" "seriesAccel"    
#> [19] "seriesDecel"     "seriesDecr"      "seriesHi"       
#> [22] "seriesIncr"      "seriesLo"        "weeklyReturn"   
#> [25] "yearlyReturn"   
#> 
#> $TTR
#>  [1] "adjRatios"          "ADX"                "ALMA"              
#>  [4] "aroon"              "ATR"                "BBands"            
#>  [7] "CCI"                "chaikinAD"          "chaikinVolatility" 
#> [10] "CLV"                "CMF"                "CMO"               
#> [13] "DEMA"               "DonchianChannel"    "DPO"               
#> [16] "DVI"                "EMA"                "EMV"               
#> [19] "EVWMA"              "GMMA"               "growth"            
#> [22] "HMA"                "KST"                "lags"              
#> [25] "MACD"               "MFI"                "momentum"          
#> [28] "OBV"                "PBands"             "ROC"               
#> [31] "rollSFM"            "RSI"                "runCor"            
#> [34] "runCov"             "runMAD"             "runMax"            
#> [37] "runMean"            "runMedian"          "runMin"            
#> [40] "runPercentRank"     "runSD"              "runSum"            
#> [43] "runVar"             "SAR"                "SMA"               
#> [46] "SMI"                "stoch"              "TDI"               
#> [49] "TRIX"               "ultimateOscillator" "VHF"               
#> [52] "VMA"                "volatility"         "VWAP"              
#> [55] "VWMA"               "wilderSum"          "williamsAD"        
#> [58] "WMA"                "WPR"                "ZigZag"            
#> [61] "ZLEMA"
```

#### tq\_transform

`tq_transform()` returns a new data set that can either be in the same periodicity or a different periodicity as the original data set. Let's use `tq_transform` to transform the periodicity of the `aapl_prices`. The `quantmod` OHLC codes are used to select the open, high, low, close, and volume (OHLCV) columns, which are then sent to the transformation function, `xts::to.period`, for transformation to monthly periodicity. We now have a much smaller data set containing the monthly prices.

``` r
aapl_prices %>%
    tq_transform(ohlc_fun = OHLCV, transform_fun = to.period, period = "months")
#> # A tibble: 120 × 6
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
#> # ... with 110 more rows
```

#### tq\_mutate

The cousin of `tq_transform()` is `tq_mutate()`. While `tq_transform()` produces a new, transformed data set, `tq_mutate()` modifies the existing data set. This is very useful for applying `TTR` functions like `BBands`, `MACD`, Moving Averages, etc. There is one caveat: the mutation must be in the same periodicity as the original data set (otherwise you can't add columns because the rows will not match up). Let's use `tq_mutate()` to add some Bollinger Bands and MACD using the closing prices (Cl in OHLC notation).

``` r
aapl_prices %>%
    tq_mutate(ohlc_fun = Cl, mutate_fun = MACD) %>%
    tq_mutate(ohlc_fun = HLC, mutate_fun = BBands)
#> # A tibble: 2,518 × 13
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
#> # ... with 2,508 more rows, and 4 more variables: dn <dbl>, mavg <dbl>,
#> #   up <dbl>, pctB <dbl>
```

### Scaling with the tidyverse

All functions return data sets as `tibbles`, which allows for interaction within the `tidyverse`. This means we can:

-   Use `dplyr` and `tidyr` to select, filter, nest/unnest, etc.
-   Use the pipe (`%>%`) for chaining operations.
-   Seamlessly scale data retrieval and transformations/mutations using `purrr` to map functions.

A very basic example is retrieving the stock prices for multiple stocks. We can do this by piping a tibble of stock symbols to a mutation that maps the `tq_get(get = "stock.prices")` function.

``` r
tibble(symbol = c("AAPL", "GOOG", "AMZN", "FB", "AVGO", "SWKS","NVDA")) %>%
    mutate(stock.prices = map(.x = symbol, ~ tq_get(.x, get = "stock.prices")))
#> # A tibble: 7 × 2
#>   symbol         stock.prices
#>    <chr>               <list>
#> 1   AAPL <tibble [2,518 × 7]>
#> 2   GOOG <tibble [2,518 × 7]>
#> 3   AMZN <tibble [2,518 × 7]>
#> 4     FB <tibble [1,163 × 7]>
#> 5   AVGO <tibble [1,865 × 7]>
#> 6   SWKS <tibble [2,518 × 7]>
#> 7   NVDA <tibble [2,518 × 7]>
```

Further Information
-------------------

This just scratches the surface of the features. See the [`tidyquant` vignette](https://cran.r-project.org/web/packages/tidyquant/vignettes/tidyquant.html) for further details on the package.
