# Changelog

## tidyquant 1.0.12

- Remove dependency to Quandl R package
- Patched the FRED-dependent CRAN failure path so an unavailable
  economic.data endpoint no longer cascades into example/test errors.
- Added explicit parameter docs for quandl_search() in R/api-quandl.R
  (line 38) and synced the generated Rd in man/quandl_search.Rd (line
  9). The missing items were query, silent, per_page, and …, which is
  what triggered the checking Rd sections warning.

## tidyquant 1.0.11

CRAN release: 2025-02-13

Fixes text: XYZ is now a live symbol.

## tidyquant 1.0.10

CRAN release: 2025-01-08

Fixes error with
[`tq_performance()`](https://business-science.github.io/tidyquant/reference/tq_performance.md)
[\#252](https://github.com/business-science/tidyquant/issues/252)

## tidyquant 1.0.9

CRAN release: 2024-09-02

### New Function:

- [`tq_fund_holdings()`](https://business-science.github.io/tidyquant/reference/tq_index.md):
  Retrieves the fund holdings and compositions for a fund and source
  combination. Example: `tq_fund_holdings("SPY", source = "SSGA")`
  [\#250](https://github.com/business-science/tidyquant/issues/250)

### Fixes and Improvements:

- Fixes to CRAN’s API policy
  [\#249](https://github.com/business-science/tidyquant/issues/249):

> “Packages which use Internet resources should fail gracefully with an
> informative message if the resource is not available or has changed
> (and not give a check warning nor error).”

## tidyquant 1.0.8

CRAN release: 2024-08-19

### TQ INDEX AND EXCHANGE:

- `tq_index("SP500")`: Fixed broken API call
  ([\#246](https://github.com/business-science/tidyquant/issues/246))
- `tq_exchange("NASDAQ")`: Fixed broken API call
  ([\#226](https://github.com/business-science/tidyquant/issues/226),
  [\#248](https://github.com/business-science/tidyquant/issues/248))

### Breaking changes

- tidyquant no longer loads lubridate. (@olivroy,
  [\#237](https://github.com/business-science/tidyquant/issues/237))

  If you use tidyquant with tidyverse, there is no change for you.

- tidyquant no longer loads many packages on load.

### Fixes

- tidyquant startup messages mimics the tidyverse messages for clarity.
  (@olivroy,
  [\#163](https://github.com/business-science/tidyquant/issues/163),
  [\#116](https://github.com/business-science/tidyquant/issues/116))
- Remove the dependency on tidyverse (@olivroy,
  [\#236](https://github.com/business-science/tidyquant/issues/236))
- tidyquant no longer loads lubridate as tidyverse 2.0 now loads
  lubridate.
- Changed the `size` argument to `linewidth` for ggplot2 3.4.0
- Removed the last tidyr and dplyr deprecated functions.
- Add `linewidth` to
  [`geom_ma()`](https://business-science.github.io/tidyquant/reference/geom_ma.md)
- Move `Quandl`, `riingo`, and `alphavantager` to Suggests. tidyquant
  will not explicitly install those, but you can install them from CRAN.
- Fixed CRAN package alias
- FB to META change in `FANG`
- [`geom_bbands()`](https://business-science.github.io/tidyquant/reference/geom_bbands.md),
  [`geom_candlestick()`](https://business-science.github.io/tidyquant/reference/geom_chart.md),
  and
  [`geom_barchart()`](https://business-science.github.io/tidyquant/reference/geom_chart.md)
  no longer emit dropped aesthetics warnings. (@olivroy,
  [\#235](https://github.com/business-science/tidyquant/issues/235))

## tidyquant 1.0.7

CRAN release: 2023-03-31

- Moved `tidyverse` from suggest to imports to pass cran tests

## tidyquant 1.0.6

CRAN release: 2022-11-16

- Fix tq_performance test failure under PerformanceAnalytics 2.0.6
  [\#223](https://github.com/business-science/tidyquant/issues/223)
- Fix failed r-devel test identified by CRAN
- Remove deprecated `spread_()` function
- Move `readxl` to imports.
  [\#222](https://github.com/business-science/tidyquant/issues/222)

## tidyquant 1.0.5

CRAN release: 2022-09-08

- FIX: FB to META name change
- IMPROVEMENT: `sp_index()` to convert symbols from “BRK.B” to “BRK-B”
  to work with Yahoo Finance

## tidyquant 1.0.4

CRAN release: 2022-05-20

CRAN requested fixes:

- Moved `janitor` to suggests
- Reduced file size by removing vignettes from CRAN version.

Other changes:

- Removed the package start-up message

## tidyquant 1.0.3

CRAN release: 2021-03-05

#### Fixes

- [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md):
  Switch to new NASDAQ website.

## tidyquant 1.0.2

CRAN release: 2020-10-21

#### Fixes

- [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md):
  Fix issue with NASDAQ changes to website.
- [`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md):
  Fix issues with `%+replace%`, `theme_gray`, and `rel` not found.

## tidyquant 1.0.1

CRAN release: 2020-07-02

#### Improvements

- [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md) -
  Add `"dividends"` and `"splits"` get options, which have been fixed in
  `quantmod` 0.4-16. Issue 150.

#### Bug Fixes

- Issue 157 - Error on package load with `rstudioapi::getThemeInfo()`
  returns `NULL`.
- `pivot_table` - Fix issues with `tidyverse` functions not being found.

#### Deprecation

- [`summarise_by_time()`](https://business-science.github.io/timetk/reference/summarise_by_time.html) -
  This function has moved to
  [`timetk::summarise_by_time()`](https://business-science.github.io/timetk/reference/summarise_by_time.html)

## tidyquant 1.0.0

CRAN release: 2020-03-04

> This is the *“R for Excel Users”* release. My aim is to build
> functionality that helps users coming from an **Excel Background**
> (background I came from). It’s important to have these users feel at
> home. I have a full suite of functionality to accomplish your
> Excel-to-R transition.
>
> -Matt

- **Excel Functions**

  - **Why Excel functions?** Designed to help users coming from an
    **Excel Background** more easily transition to the `tidyverse` and
    *“tidy- finance / business analysis”* in R.
  - **Pivot Table**
    - **[`pivot_table()`](https://business-science.github.io/tidyquant/reference/excel_pivot_table.md) -
      A tidyverse-style function to perform data summarizations just
      like the popular Excel Pivot Table.** Enables stacking
      calculations using a tidy-esque syntax:
      `.rows = ~ YEAR(order_date)`.
  - **Reference Functions**
    - **[`VLOOKUP()`](https://business-science.github.io/tidyquant/reference/excel_ref_functions.md)** -
      Performs the classic **Excel VLOOKUP.** Excel user’s: rejoice.
  - **Summarising “IFS” Functions**
    - **Summarising “IFS” Functions** - Filtering versions of Excel
      summarization counterparts. Simply add “cases” that filter if a
      condition is true.
      [`SUM_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md),
      [`COUNT_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md),
      [`AVERAGE_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
    - **Create your own “IFS” functions** - Have and idea for a new
      “IFS” function that hasn’t been made yet? Use
      [`CREATE_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
      to make your own by supplying a summarization function.
  - **Statistical, Date, and Financial Math Functions**
    - ***100+ Excel-based statistical, date, and financial math
      functions.*** Names are similar to Excel function names. By
      default, missing values are ignored (same as in Excel).
    - **Summarizations Functions** -
      [`SUM()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
      [`AVERAGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
      [`COUNT()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
      and friends.
    - **Transformation Functions** -
      [`CHANGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md),
      [`PCT_CHANGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md),
      [`LAG()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md),
      [`CUMULATIVE_SUM()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md),
      and friends.
    - **Date and Date Time Functions**
      - Integrated date calculations with `lubridate()`
        (e.g. [`AS_DATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md),
        [`YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md))
      - Holidays and business calendars with `timeDate`
        (e.g. [`HOLIDAY_SEQUENCE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md),
        `HOLIDAY_LIST()`)
      - Excel Date Math functions:
        [`NET_WORKDAYS()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md),
        [`EOMONTH()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
    - **Financial Math Functions** -
      [`NPV()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md),
      [`IRR()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md),
      [`FV()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md),
      [`PV()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md),
      [`PMT()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md),
      [`RATE()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)

- **NEW tidyverse Functionality**

  - [`summarise_by_time()`](https://business-science.github.io/timetk/reference/summarise_by_time.html) -
    This is a new time-based variant of
    [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
    that allows collapsing the time-series by “day”, “week”, “month”,
    “quarter”, “year”, and more.
  - Note: I will evaluate the need for `summarise_at_by_time()`,
    `summarise_all_by_time()`, and `summarise_if_by_time()` after the
    release of `dplyr` v1.0.0.

- **NEW API Integrations**

  - **Tiingo API** - A popular Open-Source for stock prices,
    cryptocurrencies, and intraday feeds from the IEX (Investors
    Exchange). This can serve as an alternate source of data to Yahoo
    Finance. Integrated via the `riingo` package.

- **Bug Fixes & Improvements**

  - [`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md) -
    Fix issues with collisions with `dials::margin()` and
    [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html).
    Similar potential `ggplot2` collisions have been fixed.  
  - [`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md) -
    Increased default top/bottom text margin on facet strips

- **Deprecation & Breaking Changes**

  - **Potential Breaking Change** - Single values now return the symbol
    column (i.e. `tq_get("AAPL")` returns symbol = “AAPL” for the 1st
    column).
  - **Deprecated Sources:** The following sources have been deprecated
    due to lack of support from the API:
    - Google Finance
    - Morningstar Key Ratios & Financials (Fundamentals) Data
    - Yahoo Dividends and Splits
    - Oanda FX and Metal Prices
  - **Deprecated Compound Getters** - Stacking multiple get options
    (`tq_get("AAPL", get = c("stock.prices", "stock.prices.japan"))`) is
    no longer available. **Solution:** Split these up into two calls to
    [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md).

## tidyquant 0.5.10

CRAN release: 2020-01-27

- [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md) -
  Temporarily adjust tests for `tq_get(get = "dividends")` and
  `tq_get(get = "splits")` until API is stabilizes. Yahoo! Dividends and
  Splits intermittently returns errors.
- Fix documentation warnings during package build checks. Documentation
  moved from `tq_stocklist` to
  [`?tq_index`](https://business-science.github.io/tidyquant/reference/tq_index.md).

## tidyquant 0.5.9

CRAN release: 2019-12-15

- [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  - Fix issue
    [\#144](https://github.com/business-science/tidyquant/issues/144) -
    [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
    download issue. Note that “RUSSEL1000”, “RUSSELL2000”,
    “RUSSELL3000”, and “SP1000” are no longer available due to changes
    from www.us.spdrs.com.
  - Update Stock Index Fallback.

## tidyquant 0.5.8

CRAN release: 2019-10-11

- [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md) -
  Fix naming issue with stock index data downloaded from
  www.us.spdrs.com.

## tidyquant 0.5.7

CRAN release: 2019-09-20

*Stock Index & Exchanges*

- [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md) -
  Fix NASDAQ URL change Issue
  [\#138](https://github.com/business-science/tidyquant/issues/138).

*Visualizations & Color Palettes*

- `geom_candlestick` and `geom_barchart` - Issue
  [\#112](https://github.com/business-science/tidyquant/issues/112).
- Added color names of `theme_tq` palettes (`palette_light`,
  `palette_dark`, and `palette_green`) for easier identification.

*Compatibility with `tidyr` v1.0.0*

- Improvements to ensure compatibility with `tidyr` v1.0.0

*\[Potential Breaking Change\] Move `tidyverse` to suggests*

- This is actually potentially a “breaking change” (although most users
  will see no difference since you likely load `tidyverse` in your
  scripts) - if you do not load `tidyverse`, then you will now need to
  do so. Previously `tidyquant` loaded `tidyverse` behind the scenes.

## tidyquant 0.5.6

CRAN release: 2019-04-22

- Morningstar Key Ratios: The
  [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
  argument `get = "key.ratios"` has been deprecated due to a change in
  Morningstar’s website. (Help Wanted - Ref. Issue
  [\#125](https://github.com/business-science/tidyquant/issues/125))

- Remove dependency on `XLConnect`. Replace with `readxl`. Issue
  [\#119](https://github.com/business-science/tidyquant/issues/119).

## tidyquant 0.5.5

CRAN release: 2018-05-09

- Bux fix

  - [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
    `get = "financials"` now returns a warning and `NA` as Google
    Finance no longer provides data. We are actively looking for
    alternative data sources.

  - [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
    `get = "stock.prices.google"` now returns a warning and `NA` as
    Google Finance no longer provides data. Use `get = "stock.prices"`
    instead to use Yahoo Finance, or use the `riingo` package to
    download from Tiingo.

  - Catch duplicate names in `col_rename` when you are renaming more
    than 1 column. Duplicate names are not allowed and return an error.

  - Fix duplicate name collision issue when the original name already
    includes a `.`. Duplicate names now get a `..1`, `..2`, etc. as
    opposed to `.1`, `.2`.

## tidyquant 0.5.4

CRAN release: 2018-02-19

- Features:
  - Incorporate `alphavantager`, a lightweight API to the [Alpha Vantage
    financial data provider](https://www.alphavantage.co/).
  - Integrate `Rblpapi`, R interface to “Bloomberg”. You must have a
    Bloomberg account to use this.
  - Add Google Finance as a source in
    `tq_get(get = "stock.prices.google")`
- Important Changes:
  - Remove Key Statistics from `tq_get(get = "key.stats")`. Yahoo
    Finance no longer supports the Key Statistics CSV API.
  - Completed deprecation of
    [`tidyquant::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
    and `tidyquant::as_xts()`. Use
    [`timetk::tk_tbl()`](https://business-science.github.io/timetk/reference/tk_tbl.html)
    and
    [`timetk::tk_xts()`](https://business-science.github.io/timetk/reference/tk_xts.html)
    instead.
  - `tibbletime` support was added so that all `tidyquant` functions
    play nicely with `tbl_time` objects.
  - A hard dependency on `XLConnect` was removed. This should ease the
    use of the package, especially for Mac users.
- Bug Fixes:
  - Some tests failed with `testthat` 2.0. They have been updated.

## tidyquant 0.5.3

CRAN release: 2017-08-03

- Fixes for compatibility with `purrr` v0.2.3.

## tidyquant 0.5.2

CRAN release: 2017-07-27

- Incorporated more robust `timetk` coercion functions. Deprecated
  `tidyquant::as_xts()` and
  [`tidyquant::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html).
  Use
  [`timetk::tk_xts()`](https://business-science.github.io/timetk/reference/tk_xts.html)
  and
  [`timetk::tk_tbl()`](https://business-science.github.io/timetk/reference/tk_tbl.html)
  instead.
- Fixes:
  - [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
    no longer pulls from marketvolume. Instead, 9 indices are available
    from SPDR. These indices are more reliable, and include weights for
    each stock in the index.
  - Fixed 2 tests where the results of `tq_get(get = "stock.prices")`
    were 1 or 2 rows off of what the tests expected. This likely has to
    do with the new yahoo finance API.

## tidyquant 0.5.1

CRAN release: 2017-04-24

- Improvements
  - Added `pkgdown` integration.
- Fixes:
  - Require new `quantmod` version 0.4-8 to fix Oanda and Yahoo bugs.
  - Quandl data returned newest to oldest. For consistency with other
    [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
    data, it now returns oldest to newest.
  - Oanda only returns 180 days of FX and Metals data now. Updated the
    tests to account for this. Also added error handling to check for
    valid date ranges.
  - Fixed bug with
    [`tq_portfolio()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)
    where `weights = NULL` would not execute an equal weighting scheme.
  - Added error handling during dollar and percent conversion for get =
    “key.ratios” and get = “key.stats”.

## tidyquant 0.5.0

CRAN release: 2017-04-03

- New Data:
  - Quandl Integration:
    - `tq_get(get = "quandl")` is a wrapper for
      [`Quandl::Quandl()`](https://rdrr.io/pkg/Quandl/man/Quandl.html)
      that pulls multiple Quandl Codes in a “tidy” fashion.
    - `tq_get(get = "quandl.datatable")` is a wrapper for
      `Quandl::Quandl.datable()` that pulls Quandl datatables.
    - [`quandl_api_key()`](https://business-science.github.io/tidyquant/reference/quandl_api_key.md)
      is a wrapper for
      [`Quandl::Quandl.api_key()`](https://rdrr.io/pkg/Quandl/man/Quandl.api_key.html).
    - `quandl_search` is a wrapper for
      [`Quandl::Quandl.search()`](https://rdrr.io/pkg/Quandl/man/Quandl.search.html).
  - Yahoo Japan Integration: `tq_get(get = "stock.prices.japan")` is a
    wrapper for `quantmod::getSymbols(src = "yahooj")` that enables
    getting stocks from Yahoo Finance Japan.
- Improvements and Fixes:
  - [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    now accept non-OHLC data through the `select` argument. They also
    now work with `rollapply`.
  - [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    now accept `PerformanceAnalytics` functions that work to clean and
    transform asset returns.
  - Deprecated the `ohlc_fun` argument to instead use `select` in
    [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and `tq_transmute`
  - `.Deprecated` -\> `.Defunct` for
    [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
    and
    [`tq_transform_xy()`](https://business-science.github.io/tidyquant/reference/deprecated.md).
    Use
    [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transmute_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md).
    Move the sign post functions to deprecated.R
  - Remove the previously deprecated argument, `transform_fun` from
    [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md).
    Use `mutate_fun` instead.
  - Fix issue with `tq_mutate` returning rows incorrectly sorted
  - Fix issue with `tq_get` returning data frames as nested
  - Fix `tq_get` error to return full error when issues are present.

## tidyquant 0.4.0

CRAN release: 2017-03-03

- New Features:
  - [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    replaces
    [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
    for consistency with `dplyr`.
  - [`tq_performance()`](https://business-science.github.io/tidyquant/reference/tq_performance.md)
    which integrates the performance analysis functions of
    `PerformanceAnalytics`.
  - [`tq_portfolio()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)
    which enables aggregating portfolios from individual stock returns.
  - `tq_tranform()`: Added the NA-handling functions from `zoo` to the
    list of compatible, which provide a number of useful methods for
    handling `NA` values in data sets. Added `Return.calculate` and
    `Return.excess` for calculating returns and returns in excess of the
    risk-free rate, respectively.
- Documentation:
  - [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    help pages have been combined.
  - Split introduction into four separate vignettes, which improves flow
    and enables readers to more easily get to needed documentation. Now
    five docs total covering the primary needs of `tidyquant` users!
- New data:
  - [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
    gets the stock list for NASDAQ, NYSE, and AMEX exchanges. Use
    [`tq_exchange_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
    to exchange options.
  - `FANG` data set
- New visualizations that integrate with `ggplot2`:
  - `palette_()` functions used to create scales are exported.
  - [`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md)
    creates light, dark, and green themes for tidyquant visualizations.
  - [`scale_color_tq()`](https://business-science.github.io/tidyquant/reference/scale_manual.md)
    and
    [`scale_fill_tq()`](https://business-science.github.io/tidyquant/reference/scale_manual.md)
    add color/fill scales for the data used in tidyquant visualizations.
- Improvements and Fixes:
  - The `transform_fun` argument of
    [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    has been replaced with `mutate_fun` for consistency with
    [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md).
  - Core functions are now generics to allow for extendability.
  - Issue
    [\#11](https://github.com/business-science/tidyquant/issues/11):
    Part 2. Fix multiple stocks that only return 110 lines. Handle
    stocks that return csv with “We’re sorry” message.
  - Issue
    [\#11](https://github.com/business-science/tidyquant/issues/11):
    Part 1. Fix instability with `get = key.ratios` failing with HTTP
    500 error on download. Use httr RETRY in case of failure.
  - Fixed issue with `get = "key.ratios"` where stocks listed on AMEX
    exchange were not able to return key ratios.
  - Issue [\#9](https://github.com/business-science/tidyquant/issues/9):
    Fix problem with `get = "key.stats"` where NA’s in multiple `x`
    (e.g. `c("AAPL", "GOOG")`) cause call to fail during coercion.
  - Issue [\#8](https://github.com/business-science/tidyquant/issues/8),
    Part 2: Enable compound gets
    (e.g. `tq_get("AAPL", get = c("stock.prices", "financials"))`).
  - Issue [\#8](https://github.com/business-science/tidyquant/issues/8),
    Part 1: Create
    [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
    function to return a stock index. `tq_get(get = "stock.index")` is
    deprecated and will be removed during the next version after 0.4.0.
    Use
    [`tq_index_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
    for index options.
  - Issue [\#7](https://github.com/business-science/tidyquant/issues/7):
    Fixed issue with date column inadvertently being coerced to `dttm`.

## tidyquant 0.3.0

CRAN release: 2017-01-21

- New data:
  - New `tq_get` option `get = "key.stats"`, which retrieves the current
    key statistics (55 total) from www.finance.yahoo.com/. These include
    various current data such as Ask, Bid, Day’s High, Day’s Low, Last
    Trade Price, current P/E Ratio, EPS, Current Market Cap, EPS
    Projected Current Year, EPS Projected Next Year and many more.
    Example: `tq_get("AAPL", get = "key.stats")`.
- New visualizations that integrate with `ggplot2`:
  - Chart geoms: Bar charts (`geom_barchart`) and candlestick charts
    (`geom_candlestick`) can be quickly created with the new geoms.
  - Moving Averages: Seven moving averages can be quickly visualized /
    prototyped using `geom_ma`. The geom wraps the
    [`TTR::SMA`](https://rdrr.io/pkg/TTR/man/MovingAverages.html)
    functions.
  - Bollinger bands can be visualized with `geom_bbands`. The same seven
    moving averages are compatible with the geom.
  - Zooming Into Chart Sections: Two functions (`coord_x_date` and
    `coord_x_datetime`) were added to enable zooming into chart sections
    using dates with no out-of-bounds data loss (e.g. out-of-bounds data
    loss with the `scale_x_` functions).
- New Vignette: Covers “Charting with tidyquant”.
- Fixes:
  - Issue [\#5](https://github.com/business-science/tidyquant/issues/5):
    `tq_get` can now accept character vectors and data frames for the
    `x` arg, in addition to a single character input. This streamlines
    the getting of data for multiple inputs (e.g. stock symbols, stock
    indexes, etc).
  - Issue [\#4](https://github.com/business-science/tidyquant/issues/4):
    Added `col_rename` arg to `tq_mutate` and `tq_transform`, which
    enables fast and easy renaming during the operation.
  - Issue [\#3](https://github.com/business-science/tidyquant/issues/3):
    Integrated
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
    with
    [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md).
    The transform and mutate functions now work properly with grouped
    data frames.
  - Issue [\#2](https://github.com/business-science/tidyquant/issues/2):
    Fixed bug with
    [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md),
    `get = "key.ratios"`, where key ratios for stocks from the NYSE
    returned `NA`.
- Removed support for deprecated arguments: `x_fun`, `.x`, and `.y` in
  the respective transform and mutate functions.

## tidyquant 0.2.0

CRAN release: 2017-01-08

- New `get = "key.ratios"` option for
  [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md),
  which retrieves 10-years of key performance ratios (89 total) from
  www.morningstar.com. These include various historical measures of
  financial performance including profitability, growth, cash flow,
  financial health, efficiency, and valuation ratios. Example:
  `tq_get("AAPL", get = "key.ratios")`.
- Added `zoo` `rollapply()` functions to list of compatible / integrated
  functions used with
  [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
  and
  [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md).
  See `tq_transform_fun_options()` for the full list.
- Changed
  [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md),
  [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md),
  [`tq_mutate_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  and
  [`tq_transform_xy()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
  arguments to be more obvious:
  - `x_fun` is now `ohlc_fun` for
    [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
  - `.x` is now `x` and `.y` is now `y` for
    [`tq_mutate_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
    and
    [`tq_transform_xy()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
- Fixed duplication of column names during `tq_mutate`. Names are now
  sequentially indexed with duplicate names starting at `.1` suffix.

## tidyquant 0.1.0

CRAN release: 2016-12-31

- Initial release of `tidyquant`, for seamless quantitative financial
  analysis (`xts`, `quantmod`, `TTR`) package integration with the
  `tidyverse`.
