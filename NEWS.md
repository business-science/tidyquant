# tidyquant 1.0.5

- FIXES: FB to META name change

# tidyquant 1.0.4

CRAN requested fixes:

- Moved `janitor` to suggests
- Reduced file size by removing vignettes from CRAN version. 

Other changes:

- Removed the package start-up message

# tidyquant 1.0.3

### Fixes

- `tq_exchange()`: Switch to new NASDAQ website. 

# tidyquant 1.0.2 

### Fixes

- `tq_exchange()`: Fix issue with NASDAQ changes to website. 
- `theme_tq()`: Fix issues with `%+replace%`, `theme_gray`, and `rel` not found. 


# tidyquant 1.0.1

### Improvements

* `tq_get()` - Add `"dividends"` and `"splits"` get options, which have been fixed in `quantmod` 0.4-16. Issue 150.

### Bug Fixes

* Issue 157 - Error on package load with `rstudioapi::getThemeInfo()` returns `NULL`. 
* `pivot_table` - Fix issues with `tidyverse` functions not being found.

### Deprecation

* `summarise_by_time()` - This function has moved to `timetk::summarise_by_time()`



# tidyquant 1.0.0

> This is the _"R for Excel Users"_ release. My aim is to build functionality that helps users coming from an __Excel Background__ (background I came from). It's important to have these users feel at home. I have a full suite of functionality to accomplish your Excel-to-R transition. 
>
>-Matt

* __Excel Functions__ 
    
    * __Why Excel functions?__ Designed to help users coming from an __Excel Background__ more easily transition to the `tidyverse` and _"tidy- finance / business analysis"_ in R.
    * __Pivot Table__ 
        - __`pivot_table()` - A tidyverse-style function to perform data summarizations just like the popular Excel Pivot Table.__ Enables stacking calculations using a tidy-esque syntax: `.rows = ~ YEAR(order_date)`. 
    
    * __Reference Functions__ 
        - __`VLOOKUP()`__ - Performs the classic __Excel VLOOKUP.__ Excel user's: rejoice. 
    
    * __Summarising "IFS" Functions__
        - __Summarising "IFS" Functions__ - Filtering versions of Excel summarization counterparts. Simply add "cases" that filter if a condition is true. `SUM_IFS()`, `COUNT_IFS()`, `AVERAGE_IFS()` 
        - __Create your own "IFS" functions__ - Have and idea for a new "IFS" function that hasn't been made yet? Use `CREATE_IFS()` to make your own by supplying a summarization function.
    
    * __Statistical, Date, and Financial Math Functions__ 
        - ___100+ Excel-based statistical, date, and financial math functions.___ Names are similar to Excel function names. By default, missing values are ignored (same as in Excel).
        - __Summarizations Functions__ - `SUM()`, `AVERAGE()`, `COUNT()`, and friends.
        - __Transformation Functions__ - `CHANGE()`, `PCT_CHANGE()`, `LAG()`, `CUMULATIVE_SUM()`, and friends.
        - __Date and Date Time Functions__ 
            - Integrated date calculations with `lubridate()` (e.g. `AS_DATE()`, `YEAR()`)
            - Holidays and business calendars with `timeDate` (e.g. `HOLIDAY_SEQUENCE()`, `HOLIDAY_LIST()`)
            - Excel Date Math functions: `NET_WORKDAYS()`, `EOMONTH()`
        - __Financial Math Functions__ - `NPV()`, `IRR()`, `FV()`, `PV()`, `PMT()`, `RATE()`
  
* __NEW Tidyverse Functionality__
    - `summarise_by_time()` - This is a new time-based variant of `summarise()` that allows collapsing the time-series by "day", "week", "month", "quarter", "year", and more. 
    - Note: I will evaluate the need for `summarise_at_by_time()`, `summarise_all_by_time()`, and `summarise_if_by_time()` after the release of `dplyr` v1.0.0.

* __NEW API Integrations__
    - __Tiingo API__ - A popular Open-Source for stock prices, cryptocurrencies, and intraday feeds from the IEX (Investors Exchange). This can serve as an alternate source of data to Yahoo Finance. Integrated via the `riingo` package.
    
* __Bug Fixes & Improvements__
    - `theme_tq()` - Fix issues with collisions with `dials::margin()` and `ggplot2::margin()`. Similar potential `ggplot2` collisions have been fixed.  
    - `theme_tq()` - Increased default top/bottom text margin on facet strips
    
* __Deprecation & Breaking Changes__

    * __Potential Breaking Change__ - Single values now return the symbol column (i.e. `tq_get("AAPL")` returns symbol = "AAPL" for the 1st column).
    * __Deprecated Sources:__ The following sources have been deprecated due to lack of support from the API:
        - Google Finance
        - Morningstar Key Ratios & Financials (Fundamentals) Data
        - Yahoo Dividends and Splits
        - Oanda FX and Metal Prices
    * __Deprecated Compound Getters__ - Stacking multiple get options (`tq_get("AAPL", get = c("stock.prices", "stock.prices.japan"))`) is no longer available. __Solution:__ Split these up into two calls to `tq_get()`. 

# tidyquant 0.5.10

* `tq_get()` - Temporarily adjust tests for `tq_get(get = "dividends")` and `tq_get(get = "splits")` until API is stabilizes. Yahoo! Dividends and Splits intermitently returns errors.
* Fix documentation warnings during package build checks. Documentation moved from `tq_stocklist` to `?tq_index`. 

# tidyquant 0.5.9

* `tq_index()` 
    - Fix issue #144 - `tq_index()` download issue. Note that "RUSSEL1000", "RUSSELL2000", "RUSSELL3000", and "SP1000" are no longer available due to changes from www.us.spdrs.com. 
    - Update Stock Index Fallback. 
 

# tidyquant 0.5.8

* `tq_index()` - Fix naming issue with stock index data downloaded from www.us.spdrs.com.

# tidyquant 0.5.7

_Stock Index & Exchanges_

* `tq_exchange()` - Fix NASDAQ URL change Issue #138.

_Visualizations & Color Palettes_

* `geom_candlestick` and `geom_barchart` - Issue #112.
* Added color names of `theme_tq` palettes (`palette_light`, `palette_dark`, and  `palette_green`) for easier identification.

_Compatability with `tidyr` v1.0.0_

* Improvements to ensure compatability with `tidyr` v1.0.0

_[Potential Breaking Change] Move `tidyverse` to suggests_

* This is actually potentially a "breaking change" (although most users will see no difference since you likely load `tidyverse` in your scripts) - if you do not load `tidyverse`, then you will now need to do so. Previously `tidyquant` loaded `tidyverse` behind the scenes.  

# tidyquant 0.5.6

* Morningstar Key Ratios: The `tq_get()` argument `get = "key.ratios"` has been deprecated due to a change in Morningstar's website. (Help Wanted - Ref. Issue #125)

* Remove dependency on `XLConnect`. Replace with `readxl`. Issue #119.

# tidyquant 0.5.5

* Bux fix

    * `tq_get()` `get = "financials"` now returns a warning and `NA`
    as Google Finance no longer provides data. We are actively looking for 
    alternative data sources.

    * `tq_get()` `get = "stock.prices.google"` now returns a warning and `NA`
    as Google Finance no longer provides data. Use `get = "stock.prices"` instead
    to use Yahoo Finance, or use the `riingo` package to download from Tiingo.
    
    * Catch duplicate names in `col_rename` when you are renaming more than 1 column.
    Duplicate names are not allowed and return an error.

    * Fix duplicate name collision issue when the original name already includes 
    a `.`. Duplicate names now get a `..1`, `..2`, etc. as opposed to `.1`, `.2`.

# tidyquant 0.5.4

* Features:
    * Incorporate `alphavantager`, a lightweight API to the [Alpha Vantage financial data provider](https://www.alphavantage.co/).
    * Integrate `Rblpapi`, R interface to "Bloomberg". You must have a Bloomberg account to use this.
    * Add Google Finance as a source in `tq_get(get = "stock.prices.google")`

* Important Changes:
    * Remove Key Statistics from `tq_get(get = "key.stats")`. Yahoo Finance no longer supports the Key Statistics CSV API. 
    * Completed deprecation of `tidyquant::as_tibble()` and `tidyquant::as_xts()`. Use `timetk::tk_tbl()` and `timetk::tk_xts()` instead. 
    * `tibbletime` support was added so that all `tidyquant` functions play nicely with `tbl_time` objects.
    * A hard dependency on `XLConnect` was removed. This should ease the use of the package, especially for Mac users.
    
* Bug Fixes:
    * Some tests failed with `testthat` 2.0. They have been updated.

# tidyquant 0.5.3

* Fixes for compatibility with `purrr` v0.2.3. 


# tidyquant 0.5.2

* Incorporated more robust `timetk` coercion functions. Deprecated `tidyquant::as_xts()` and `tidyquant::as_tibble()`. Use `timetk::tk_xts()` and `timetk::tk_tbl()` instead.
* Fixes:
    * `tq_index()` no longer pulls from marketvolume. Instead, 9 indices are available from SPDR. These indices are more reliable, and include weights for each stock in the index.
    * Fixed 2 tests where the results of `tq_get(get = "stock.prices")` were 1 or 2 rows off of what the tests expected. This likely has to do with the new yahoo finance API.

# tidyquant 0.5.1

* Improvements
    * Added `pkgdown` integration.

* Fixes:
    * Require new `quantmod` version 0.4-8 to fix Oanda and Yahoo bugs.
    * Quandl data returned newest to oldest. For consistency with other `tq_get()` data, it now returns oldest to newest.
    * Oanda only returns 180 days of FX and Metals data now. Updated the tests to account for this. Also added error handling to check for valid date ranges.
    * Fixed bug with `tq_portfolio()` where `weights = NULL` would not execute an equal weighting scheme.
    * Added error handling during dollar and percent conversion for get = "key.ratios" and get = "key.stats".

# tidyquant 0.5.0

* New Data:
    * Quandl Integration: 
        * `tq_get(get = "quandl")` is a wrapper for `Quandl::Quandl()` that pulls multiple Quandl Codes in a "tidy" fashion. 
        * `tq_get(get = "quandl.datatable")` is a wrapper for `Quandl::Quandl.datable()` that pulls Quandl datatables. 
        * `quandl_api_key()` is a wrapper for `Quandl::Quandl.api_key()`. 
        * `quandl_search` is a wrapper for `Quandl::Quandl.search()`.
    * Yahoo Japan Integration: `tq_get(get = "stock.prices.japan")` is a wrapper for `quantmod::getSymbols(src = "yahooj")` that enables getting stocks from Yahoo Finance Japan.
    
* Improvements and Fixes:
    * `tq_mutate()` and `tq_transmute()` now accept non-OHLC data through the `select` argument. They also now work with `rollapply`.
    * `tq_mutate()` and `tq_transmute()` now accept `PerformanceAnalytics` functions that work to clean and transform asset returns.
    * Deprecated the `ohlc_fun` argument to instead use `select` in `tq_mutate()` and `tq_transmute`
    * `.Deprecated` -> `.Defunct` for `tq_transform()` and `tq_transform_xy()`. Use `tq_transmute()` and `tq_transmute_xy()`. Move the sign post functions to deprecated.R
    * Remove the previously deprecated argument, `transform_fun` from `tq_transmute()`. Use `mutate_fun` instead.
    * Fix issue with `tq_mutate` returning rows incorrectly sorted
    * Fix issue with `tq_get` returning data frames as nested
    * Fix `tq_get` error to return full error when issues are present. 


# tidyquant 0.4.0

* New Features:
    * `tq_transmute()` replaces `tq_transform()` for consistency with `dplyr`.
    * `tq_performance()` which integrates the performance analysis functions of `PerformanceAnalytics`.
    * `tq_portfolio()` which enables aggregating portfolios from individual stock returns.
    * `tq_tranform()`: Added the NA-handling functions from `zoo` to the list of compatible, which provide a number of useful methods for handling `NA` values in data sets. Added `Return.calculate` and `Return.excess` for calculating returns and returns in excess of the risk-free rate, respectively.
* Documentation:
    * `tq_mutate()` and `tq_transmute()` help pages have been combined.
    * Split introduction into four separate vignettes, which improves flow and enables readers to more easily get to needed documentation. Now five docs total covering the primary needs of `tidyquant` users!
* New data:
    * `tq_exchange()` gets the stock list for NASDAQ, NYSE, and AMEX exchanges. Use `tq_exchange_options()` to exchange options.
    * `FANG` data set that can be loaded with `data(FANG)`.
* New visualizations that integrate with `ggplot2`:
    * `palette_()` functions used to create scales are exported.
    * `theme_tq()` creates light, dark, and green themes for tidyquant visualizations.
    * `scale_color_tq()` and `scale_fill_tq()` add color/fill scales for the data used in tidyquant visualizations.
* Improvements and Fixes:
    * The `transform_fun` argument of `tq_transmute()` has been replaced with `mutate_fun` for consistency with `tq_mutate()`.
    * Core functions are now generics to allow for extendability.
    * Issue #11: Part 2. Fix multiple stocks that only return 110 lines. Handle stocks that return csv with "We're sorry" message.
    * Issue #11: Part 1. Fix instability with `get = key.ratios` failing with HTTP 500 error on download. Use httr RETRY in case of failure.
    * Fixed issue with `get = "key.ratios"` where stocks listed on AMEX exchange were not able to return key ratios.
    * Issue #9: Fix problem with `get = "key.stats"` where NA's in multiple `x` (e.g. `c("AAPL", "GOOG")`) cause call to fail during coercion. 
    * Issue #8, Part 2: Enable compound gets (e.g. `tq_get("AAPL", get = c("stock.prices", "financials"))`).
    * Issue #8, Part 1: Create `tq_index()` function to return a stock index. `tq_get(get = "stock.index")` is deprecated and will be removed during the next version after 0.4.0. Use `tq_index_options()` for index options. 
    * Issue #7: Fixed issue with date column inadvertently being coerced to `dttm`. 

# tidyquant 0.3.0

* New data:
    * New `tq_get` option `get = "key.stats"`, which retrieves the current key statistics (55 total) from [www.finance.yahoo.com/](https://finance.yahoo.com/). These include various current data such as Ask, Bid, Day's High, Day's Low, Last Trade Price, current P/E Ratio, EPS, Current Market Cap, EPS Projected Current Year, EPS Projected Next Year and many more. Example: `tq_get("AAPL", get = "key.stats")`.
* New visualizations that integrate with `ggplot2`:
    * Chart geoms: Bar charts (`geom_barchart`) and candlestick charts (`geom_candlestick`) can be quickly created with the new geoms.
    * Moving Averages: Seven moving averages can be quickly visualized / prototyped using `geom_ma`. The geom wraps the `TTR::SMA` functions.
    * Bollinger bands can be visualized with `geom_bbands`. The same seven moving averages are compatible with the geom.
    * Zooming Into Chart Sections: Two functions (`coord_x_date` and `coord_x_datetime`) were added to enable zooming into chart sections using dates with no out-of-bounds data loss (e.g. out-of-bounds data loss with the `scale_x_` functions). 
* New Vignette: Covers "Charting with tidyquant".
* Fixes:
    * Issue #5: `tq_get` can now accept character vectors and data frames for the `x` arg, in addition to a single character input. This streamlines the getting of data for multiple inputs (e.g. stock symbols, stock indexes, etc).
    * Issue #4: Added `col_rename` arg to `tq_mutate` and `tq_transform`, which enables fast and easy renaming during the operation.
    * Issue #3: Integrated `dplyr::group_by()` with `tq_mutate()` and `tq_transform()`. The transform and mutate functions now work properly with grouped data frames.
    * Issue #2: Fixed bug with `tq_get()`, `get = "key.ratios"`, where key ratios for stocks from the NYSE returned `NA`.
* Removed support for deprecated arguments: `x_fun`, `.x`, and `.y` in the respective transform and mutate functions.

# tidyquant 0.2.0

* New `get = "key.ratios"` option for `tq_get()`, which retrieves 10-years of key performance ratios (89 total) from [www.morningstar.com](https://www.morningstar.com). These include various historical measures of financial performance including profitability, growth, cash flow, financial health, efficiency, and valuation ratios. Example: `tq_get("AAPL", get = "key.ratios")`.
* Added `zoo` `rollapply()` functions to list of compatible / integrated functions used with `tq_transform()` and `tq_mutate()`. See `tq_transform_fun_options()` for the full list.
* Changed `tq_mutate()`, `tq_transform()`, `tq_mutate_xy()` and `tq_transform_xy()` arguments to be more obvious:
    * `x_fun` is now `ohlc_fun` for `tq_mutate()` and `tq_transform()`
    * `.x` is now `x` and `.y` is now `y` for `tq_mutate_xy()` and `tq_transform_xy()`
* Fixed duplication of column names during `tq_mutate`. Names are now sequentually indexed with duplicate names starting at `.1` suffix.  


# tidyquant 0.1.0 

* Initial release of `tidyquant`, for seamless quantitative financial analysis (`xts`, `quantmod`, `TTR`) package integration with the `tidyverse`.
