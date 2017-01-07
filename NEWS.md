# tidyquant 0.1.0.9010 - Development Version

* Added `get = "key.ratios"` option for `tq_get()`, which retrieves 10-years of key performance ratios (89 total) from [www.morningstar.com](https://www.morningstar.com). These include various measures of financial performance including profitability, growth, cash flow, financial health, efficiency, and valuation ratios. Example: `tq_get("AAPL", get = "key.ratios")`.
* Added `zoo` `rollapply()` functions to list of compatible / integrated functions used with `tq_transform()` and `tq_mutate()`. See `tq_transform_fun_options()` for the full list.
* Changed `tq_mutate()`, `tq_transform()`, `tq_mutate_xy()` and `tq_transform_xy()` arguments to be more obvious:
    * `x_fun` is now `ohlc_fun` for `tq_mutate()` and `tq_transform()`
    * `.x` is now `x` and `.y` is now `y` for `tq_mutate_xy()` and `tq_transform_xy()`
* Fixed duplication of column names during `tq_mutate`. Names are now sequentually indexed with duplicate names starting at `.1` suffix.  

# tidyquant 0.1.0 

* Initial release of `tidyquant`, for seamless quantitative financial analysis (`xts`, `quantmod`, `TTR`) package integration with the `tidyverse`.
