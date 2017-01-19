# tidyquant 0.3.0

* New visualizations that integrate with `ggplot2`:
    * Chart geoms: Bar charts (`tq_geom_barchart`) and candlestick charts (`tq_geom_candlestick`) can be quickly created with the new geoms.
    * Moving Averages: Seven moving averages can be quickly visualized / prototyped using `tq_geom_ma`. The geom wraps the `TTR::SMA` functions.
    * Bollinger bands can be visualized with `tq_geom_bbands`. The same seven moving averages are compatible with the geom.
    * Zooming Into Chart Sections: Two functions (`coord_x_date` and `coord_x_datetime`) were added to enable zooming into chart sections using dates with no out-of-bounds data loss (e.g. out-of-bounds data loss with the `scale_x_` functions). 
* Fixes:
    * Issue #5: `tq_get` can now accept character vectors and data frames for the `x` arg, in addition to a single character input. This streamlines the getting of data for multiple inputs (e.g. stock symbols, stock indexes, etc).
    * Issue #4: Added `col_rename` arg to `tq_mutate` and `tq_transform`, which enables fast and easy renaming during the operation.
    * Issue #3: Integrated `dplyr::group_by()` with `tq_mutate()` and `tq_transform()`. The transform and mutate functions now work properly with grouped data frames.
    * Issue #2: Fixed bug with `tq_get()`, `get = "key.ratios"`, where key ratios for stocks from the NYSE returned `NA`.
* Removed support for deprecated arguments: `x_fun`, `.x`, and `.y` in the respective transform and mutate functions.

# tidyquant 0.2.0

* Added `get = "key.ratios"` option for `tq_get()`, which retrieves 10-years of key performance ratios (89 total) from [www.morningstar.com](https://www.morningstar.com). These include various measures of financial performance including profitability, growth, cash flow, financial health, efficiency, and valuation ratios. Example: `tq_get("AAPL", get = "key.ratios")`.
* Added `zoo` `rollapply()` functions to list of compatible / integrated functions used with `tq_transform()` and `tq_mutate()`. See `tq_transform_fun_options()` for the full list.
* Changed `tq_mutate()`, `tq_transform()`, `tq_mutate_xy()` and `tq_transform_xy()` arguments to be more obvious:
    * `x_fun` is now `ohlc_fun` for `tq_mutate()` and `tq_transform()`
    * `.x` is now `x` and `.y` is now `y` for `tq_mutate_xy()` and `tq_transform_xy()`
* Fixed duplication of column names during `tq_mutate`. Names are now sequentually indexed with duplicate names starting at `.1` suffix.  


# tidyquant 0.1.0 

* Initial release of `tidyquant`, for seamless quantitative financial analysis (`xts`, `quantmod`, `TTR`) package integration with the `tidyverse`.
