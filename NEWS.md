# tidyquant 0.1.0.9000 - Development Version

* Added `zoo` `rollapply()` functions to list of compatible functions with `tq_transform()` and `tq_mutate()`. See `tq_transform_fun_options()`.
* Added `get = "key.ratios"` option for `tq_get()`. Key ratios are downloaded from www.morningstar.com. These include various measures of financial performance including profitability, growth, cash flow, financial health, and efficiency ratios. Example: `tq_get("AAPL", get = "key.ratios")`.
* Changed `tq_mutate()`, `tq_transform()`, `tq_mutate_xy()` and `tq_transform_xy()` arguments to be more obvious:
    * `x_fun` is now `ohlc_fun` for `tq_mutate()` and `tq_transform()`
    * `.x` is now `x` and `.y` is now `y` for `tq_mutate_xy()` and `tq_transform_xy()`
* Fixed duplication of column names during `tq_mutate`. Names are now sequentually indexed with duplicate names starting at `.1` suffix.  

# tidyquant 0.1.0 

* Initial release of `tidyquant`, for seamless quantitative financial analysis (`xts`, `quantmod`, `TTR`) package integration with the `tidyverse`.
