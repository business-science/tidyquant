# Package index

## Financial APIs

Functions for getting financial data from various API sources.

### Get Data from APIs

- [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
  [`tq_get_options()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
  :

  Get quantitative data in `tibble` format

- [`tq_index()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  [`tq_index_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  [`tq_exchange()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  [`tq_exchange_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  [`tq_fund_holdings()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  [`tq_fund_source_options()`](https://business-science.github.io/tidyquant/reference/tq_index.md)
  :

  Get all stocks in a stock index or stock exchange in `tibble` format

### API Keys

- [`quandl_api_key()`](https://business-science.github.io/tidyquant/reference/quandl_api_key.md)
  : Query or set Quandl API Key
- [`quandl_search()`](https://business-science.github.io/tidyquant/reference/quandl_search.md)
  : Search the Quandl database
- [`av_api_key()`](https://business-science.github.io/tidyquant/reference/av_api_key.md)
  : Set Alpha Vantage API Key
- [`tiingo_api_key()`](https://business-science.github.io/tidyquant/reference/tiingo_api_key.md)
  : Set Tiingo API Key

## Financial Analysis

Functions for performing various financial analyses.

### Quantitative Transformations

Functions for mutating quantitative data.

- [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_mutate_()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_mutate_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_mutate_xy_()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_mutate_fun_options()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_transmute_()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_transmute_xy()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_transmute_xy_()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  [`tq_transmute_fun_options()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  : Mutates quantitative data

### Portfolio Aggregation

Functions for aggregating 1 or more portfolios.

- [`tq_portfolio()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)
  [`tq_portfolio_()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)
  [`tq_repeat_df()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)
  : Aggregates a group of returns by asset into portfolio returns

### Performance Analytics

Functions for performing various financial performance analyses.

- [`tq_performance()`](https://business-science.github.io/tidyquant/reference/tq_performance.md)
  [`tq_performance_()`](https://business-science.github.io/tidyquant/reference/tq_performance.md)
  [`tq_performance_fun_options()`](https://business-science.github.io/tidyquant/reference/tq_performance.md)
  : Computes a wide variety of summary performance metrics from stock or
  portfolio returns

### Charting with ggplot2

Functions for creating `ggplots` for financial charting.

- [`geom_bbands()`](https://business-science.github.io/tidyquant/reference/geom_bbands.md)
  [`geom_bbands_()`](https://business-science.github.io/tidyquant/reference/geom_bbands.md)
  : Plot Bollinger Bands using Moving Averages
- [`geom_barchart()`](https://business-science.github.io/tidyquant/reference/geom_chart.md)
  [`geom_candlestick()`](https://business-science.github.io/tidyquant/reference/geom_chart.md)
  : Plot Financial Charts in ggplot2
- [`geom_ma()`](https://business-science.github.io/tidyquant/reference/geom_ma.md)
  [`geom_ma_()`](https://business-science.github.io/tidyquant/reference/geom_ma.md)
  : Plot moving averages
- [`theme_tq()`](https://business-science.github.io/tidyquant/reference/theme_tq.md)
  [`theme_tq_dark()`](https://business-science.github.io/tidyquant/reference/theme_tq.md)
  [`theme_tq_green()`](https://business-science.github.io/tidyquant/reference/theme_tq.md)
  : tidyquant themes for ggplot2.
- [`palette_light()`](https://business-science.github.io/tidyquant/reference/palette_tq.md)
  [`palette_dark()`](https://business-science.github.io/tidyquant/reference/palette_tq.md)
  [`palette_green()`](https://business-science.github.io/tidyquant/reference/palette_tq.md)
  : tidyquant palettes for use with scales
- [`scale_color_tq()`](https://business-science.github.io/tidyquant/reference/scale_manual.md)
  [`scale_colour_tq()`](https://business-science.github.io/tidyquant/reference/scale_manual.md)
  [`scale_fill_tq()`](https://business-science.github.io/tidyquant/reference/scale_manual.md)
  : tidyquant colors and fills for ggplot2.
- [`coord_x_date()`](https://business-science.github.io/tidyquant/reference/coord_x_date.md)
  [`coord_x_datetime()`](https://business-science.github.io/tidyquant/reference/coord_x_date.md)
  : Zoom in on plot regions using date ranges or date-time ranges

## Excel

Functions providing Excel-like functionality.

### Pivot Table, VLOOKUP, & Sum-Ifs

Common functions for Excel power-users.

- [`pivot_table()`](https://business-science.github.io/tidyquant/reference/excel_pivot_table.md)
  : Excel Pivot Table
- [`VLOOKUP()`](https://business-science.github.io/tidyquant/reference/excel_ref_functions.md)
  : Excel Reference Functions
- [`SUM_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  [`COUNT_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  [`AVERAGE_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  [`MEDIAN_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  [`MIN_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  [`MAX_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  [`CREATE_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md)
  : Excel Summarising "If" Functions

### Summary Functions

Functions returning a single value from a vector of values.

- [`SUM()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`AVERAGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`MEDIAN()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`MIN()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`MAX()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`COUNT()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`COUNT_UNIQUE()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`STDEV()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`VAR()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`COR()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`COV()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`FIRST()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`LAST()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`NTH()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`CHANGE_FIRSTLAST()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  [`PCT_CHANGE_FIRSTLAST()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md)
  : Excel Statistical Summary Functions

### Mutation Functions

Functions returning multiple values from a vector.

- [`ABS()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`SQRT()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`LOG()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`EXP()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`RETURN()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`PCT_CHANGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CHANGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`LAG()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`LEAD()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CUMULATIVE_SUM()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CUMULATIVE_PRODUCT()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CUMULATIVE_MAX()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CUMULATIVE_MIN()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CUMULATIVE_MEAN()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  [`CUMULATIVE_MEDIAN()`](https://business-science.github.io/tidyquant/reference/excel_stat_mutation_functions.md)
  : Excel Statistical Mutation Functions

### Date & Time Functions

Functions providing date and time functionality.

- [`AS_DATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`AS_DATETIME()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DATEVALUE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YMD()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MDY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DMY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YMD_HMS()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MDY_HMS()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DMY_HMS()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YMD_HM()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MDY_HM()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DMY_HM()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YMD_H()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MDY_H()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DMY_H()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`WEEKDAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`WDAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DOW()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MONTHDAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MDAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DOM()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`QUARTERDAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`QDAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`WEEKNUM()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`WEEK()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`WEEKNUM_ISO()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MONTH()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`QUARTER()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YEAR_ISO()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DATE_TO_NUMERIC()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DATE_TO_DECIMAL()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`SECOND()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`MINUTE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`HOUR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`NOW()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`TODAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`EOMONTH()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`EDATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`NET_WORKDAYS()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`COUNT_DAYS()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`YEARFRAC()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`DATE_SEQUENCE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`WORKDAY_SEQUENCE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`HOLIDAY_SEQUENCE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`HOLIDAY_TABLE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`FLOOR_DATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`FLOOR_DAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`FLOOR_WEEK()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`FLOOR_MONTH()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`FLOOR_QUARTER()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`FLOOR_YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`CEILING_DATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`CEILING_DAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`CEILING_WEEK()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`CEILING_MONTH()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`CEILING_QUARTER()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`CEILING_YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`ROUND_DATE()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`ROUND_DAY()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`ROUND_WEEK()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`ROUND_MONTH()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`ROUND_QUARTER()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  [`ROUND_YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)
  : Excel Date and Time Functions

### Financial Functions

Common financial calculations from Excel.

- [`NPV()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)
  [`IRR()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)
  [`FV()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)
  [`PV()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)
  [`PMT()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)
  [`RATE()`](https://business-science.github.io/tidyquant/reference/excel_financial_math_functions.md)
  : Excel Financial Math Functions

## Data Sets

- [`FANG`](https://business-science.github.io/tidyquant/reference/FANG.md)
  : Stock prices for the "FANG" stocks.

## Tidyquant

Package-level information.

- [`tidyquant`](https://business-science.github.io/tidyquant/reference/tidyquant-package.md)
  [`tidyquant-package`](https://business-science.github.io/tidyquant/reference/tidyquant-package.md)
  : tidyquant: Integrating quantitative financial analysis tools with
  the tidyverse
- [`tidyquant_conflicts()`](https://business-science.github.io/tidyquant/reference/tidyquant_conflicts.md)
  : Conflicts between the tidyquant and other packages

## Deprecated

- [`tq_transform()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
  [`tq_transform_xy()`](https://business-science.github.io/tidyquant/reference/deprecated.md)
  : Deprecated functions
