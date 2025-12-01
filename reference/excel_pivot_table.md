# Excel Pivot Table

**The Pivot Table** is one of Excel's most powerful features, and now
it's available in `R`! A pivot table is a table of statistics that
summarizes the data of a more extensive table (such as from a database,
spreadsheet, or business intelligence program).

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names are similar to Excel function names

- Functionality replicates Excel

## Usage

``` r
pivot_table(
  .data,
  .rows,
  .columns,
  .values,
  .filters = NULL,
  .sort = NULL,
  fill_na = NA
)
```

## Arguments

- .data:

  A `data.frame` or `tibble` that contains data to summarize with a
  pivot table

- .rows:

  Enter one or more groups to assess as expressions (e.g.
  `~ MONTH(date_column)`)

- .columns:

  Enter one or more groups to assess expressions (e.g.
  `~ YEAR(date_column)`)

- .values:

  Numeric only. Enter one or more summarization expression(s) (e.g.
  `~ SUM(value_column)`)

- .filters:

  This argument is not yet in use

- .sort:

  This argument is not yet in use

- fill_na:

  A value to replace missing values with. Default is `NA`

## Value

Returns a tibble that has been pivoted to summarize information by
column and row groupings

## Details

This summary might include sums, averages, or other statistics, which
the pivot table groups together in a meaningful way.

The key parameters are:

- `.rows` - These are groups that will appear as row-wise headings for
  the summarization, You can modify these groups by applying collapsing
  functions (e.g.
  ([`YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)).

- `.columns` - These are groups that will appear as column headings for
  the summarization. You can modify these groups by applying collapsing
  functions (e.g.
  ([`YEAR()`](https://business-science.github.io/tidyquant/reference/excel_date_functions.md)).

- `.values` - These are numeric data that are summarized using a summary
  function (e.g.
  [`SUM()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
  [`AVERAGE()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
  [`COUNT()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
  [`FIRST()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
  [`LAST()`](https://business-science.github.io/tidyquant/reference/excel_stat_summary_functions.md),
  [`SUM_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md),
  [`AVERAGE_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md),
  [`COUNT_IFS()`](https://business-science.github.io/tidyquant/reference/excel_if_functions.md))

**R implementation details.**

- The `pivot_table()` function is powered by the `tidyverse`, an
  ecosystem of packages designed to manipulate data.

- All of the key parameters can be expressed using a functional form:

  - Rows and Column Groupings can be collapsed. Example:
    `.columns = ~ YEAR(order_date)`

  - Values can be summarized provided a single value is returned.
    Example:
    `.values = ~ SUM_IFS(order_volume >= quantile(order_volume, probs = 0.75))`

  - Summarizations and Row/Column Groupings can be stacked (combined)
    with [`c()`](https://rdrr.io/r/base/c.html). Example:
    `.rows = c(~ YEAR(order_date), company)`

  - Bare columns (e.g. `company`) don not need to be prefixed with the
    `~`.

  - **All grouping and summarizing functions MUST BE prefixed with
    `~`**. Example: `.rows = ~ YEAR(order_date)`

## Examples

``` r
# PIVOT TABLE ----
# Calculate returns by year/quarter
FANG %>%
    pivot_table(
        .rows       = c(symbol, ~ QUARTER(date)),
        .columns    = ~ YEAR(date),
        .values     = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )
#> # A tibble: 16 × 6
#>    symbol `QUARTER(date)`  `2013`   `2014`  `2015`    `2016`
#>    <chr>            <int>   <dbl>    <dbl>   <dbl>     <dbl>
#>  1 AMZN                 1  0.0357 -0.155    0.206  -0.0681  
#>  2 AMZN                 2  0.0615 -0.0531   0.172   0.196   
#>  3 AMZN                 3  0.108  -0.0299   0.170   0.154   
#>  4 AMZN                 4  0.243  -0.0224   0.298  -0.104   
#>  5 GOOG                 1  0.0981  0.00174  0.0442  0.00419 
#>  6 GOOG                 2  0.0988  0.0143  -0.0406 -0.0771  
#>  7 GOOG                 3 -0.0135 -0.00911  0.166   0.112   
#>  8 GOOG                 4  0.263  -0.0737   0.241  -0.000958
#>  9 META                 1 -0.0864  0.101    0.0481  0.116   
#> 10 META                 2 -0.0255  0.0746   0.0502 -0.0153  
#> 11 META                 3  1.02    0.161    0.0344  0.123   
#> 12 META                 4  0.0839  0.0192   0.151  -0.107   
#> 13 NFLX                 1  1.06   -0.0297   0.194  -0.0703  
#> 14 NFLX                 2  0.157   0.208    0.590  -0.135   
#> 15 NFLX                 3  0.379  -0.0463   0.103   0.0194  
#> 16 NFLX                 4  0.134  -0.221    0.0793  0.206   
```
