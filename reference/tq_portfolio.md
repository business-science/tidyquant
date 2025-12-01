# Aggregates a group of returns by asset into portfolio returns

Aggregates a group of returns by asset into portfolio returns

## Usage

``` r
tq_portfolio(
  data,
  assets_col,
  returns_col,
  weights = NULL,
  col_rename = NULL,
  ...
)

tq_portfolio_(
  data,
  assets_col,
  returns_col,
  weights = NULL,
  col_rename = NULL,
  ...
)

tq_repeat_df(data, n, index_col_name = "portfolio")
```

## Arguments

- data:

  A `tibble` (tidy data frame) of returns in tidy format (i.e long
  format).

- assets_col:

  The column with assets (securities)

- returns_col:

  The column with returns

- weights:

  Optional parameter for the asset weights, which can be passed as a
  numeric vector the length of the number of assets or a two column
  tibble with asset names in first column and weights in second column.

- col_rename:

  A string or character vector containing names that can be used to
  quickly rename columns.

- ...:

  Additional parameters passed to
  [`PerformanceAnalytics::Return.portfolio`](https://rdrr.io/pkg/PerformanceAnalytics/man/Return.portfolio.html)

- n:

  Number of times to repeat a data frame row-wise.

- index_col_name:

  A renaming function for the "index" column, used when repeating data
  frames.

## Value

Returns data in the form of a `tibble` object.

## Details

`tq_portfolio` is a wrapper for
[`PerformanceAnalytics::Return.portfolio`](https://rdrr.io/pkg/PerformanceAnalytics/man/Return.portfolio.html).
The main advantage is the results are returned as a `tibble` and the
function can be used with the `tidyverse`.

`assets_col` and `returns_col` are columns within `data` that are used
to compute returns for a portfolio. The columns should be in "long"
format (or "tidy" format) meaning there is only one column containing
all of the assets and one column containing all of the return values
(i.e. not in "wide" format with returns spread by asset).

`weights` are the weights to be applied to the asset returns. Weights
can be input in one of three options:

- Single Portfolio: A numeric vector of weights that is the same length
  as unique number of assets. The weights are applied in the order of
  the assets.

- Single Portfolio: A two column tibble with assets in the first column
  and weights in the second column. The advantage to this method is the
  weights are mapped to the assets and any unlisted assets default to a
  weight of zero.

- Multiple Portfolios: A three column tibble with portfolio index in the
  first column, assets in the second column, and weights in the third
  column. The tibble must be grouped by portfolio index.

`tq_repeat_df` is a simple function that repeats a data frame `n` times
row-wise (long-wise), and adds a new column for a portfolio index. The
function is used to assist in Multiple Portfolio analyses, and is a
useful precursor to `tq_portfolio`.

## See also

- [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  which can be used to get period returns.

- [`PerformanceAnalytics::Return.portfolio()`](https://rdrr.io/pkg/PerformanceAnalytics/man/Return.portfolio.html)
  which is the underlying function that specifies which parameters can
  be passed via `...`

## Examples

``` r
# Load libraries
library(dplyr)

# Use FANG data set

# Get returns for individual stock components
monthly_returns_stocks <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly")

##### Portfolio Aggregation Methods #####

# Method 1: Use tq_portfolio with numeric vector of weights

weights <- c(0.50, 0.25, 0.25, 0)
tq_portfolio(data = monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights,
             col_rename = NULL,
             wealth.index = FALSE)
#> # A tibble: 48 × 2
#>    date       portfolio.returns
#>    <date>                 <dbl>
#>  1 2013-01-31           0.260  
#>  2 2013-02-28          -0.00456
#>  3 2013-03-28          -0.0195 
#>  4 2013-04-30           0.0810 
#>  5 2013-05-31          -0.0139 
#>  6 2013-06-28          -0.0179 
#>  7 2013-07-31           0.254  
#>  8 2013-08-30           0.104  
#>  9 2013-09-30           0.145  
#> 10 2013-10-31           0.0420 
#> # ℹ 38 more rows

# Method 2: Use tq_portfolio with two column tibble and map weights

# Note that GOOG's weighting is zero in Method 1. In Method 2,
# GOOG is not added and same result is achieved.
weights_df <- tibble(symbol = c("META", "AMZN", "NFLX"),
                     weights = c(0.50, 0.25, 0.25))
tq_portfolio(data = monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_df,
             col_rename = NULL,
             wealth.index = FALSE)
#> # A tibble: 48 × 2
#>    date       portfolio.returns
#>    <date>                 <dbl>
#>  1 2013-01-31           0.260  
#>  2 2013-02-28          -0.00456
#>  3 2013-03-28          -0.0195 
#>  4 2013-04-30           0.0810 
#>  5 2013-05-31          -0.0139 
#>  6 2013-06-28          -0.0179 
#>  7 2013-07-31           0.254  
#>  8 2013-08-30           0.104  
#>  9 2013-09-30           0.145  
#> 10 2013-10-31           0.0420 
#> # ℹ 38 more rows

# Method 3: Working with multiple portfolios

# 3A: Duplicate monthly_returns_stocks multiple times
mult_monthly_returns_stocks <- tq_repeat_df(monthly_returns_stocks, n = 4)
#> Ungrouping data frame groups: symbol

# 3B: Create weights table grouped by portfolio id
weights <- c(0.50, 0.25, 0.25, 0.00,
             0.00, 0.50, 0.25, 0.25,
             0.25, 0.00, 0.50, 0.25,
             0.25, 0.25, 0.00, 0.50)
stocks <- c("META", "AMZN", "NFLX", "GOOG")
weights_table <- tibble(stocks) %>%
    tq_repeat_df(n = 4) %>%
    bind_cols(tibble(weights)) %>%
    group_by(portfolio)

# 3C: Scale to multiple portfolios
tq_portfolio(data = mult_monthly_returns_stocks,
             assets_col = symbol,
             returns_col = monthly.returns,
             weights = weights_table,
             col_rename = NULL,
             wealth.index = FALSE)
#> # A tibble: 192 × 3
#> # Groups:   portfolio [4]
#>    portfolio date       portfolio.returns
#>        <int> <date>                 <dbl>
#>  1         1 2013-01-31           0.260  
#>  2         1 2013-02-28          -0.00456
#>  3         1 2013-03-28          -0.0195 
#>  4         1 2013-04-30           0.0810 
#>  5         1 2013-05-31          -0.0139 
#>  6         1 2013-06-28          -0.0179 
#>  7         1 2013-07-31           0.254  
#>  8         1 2013-08-30           0.104  
#>  9         1 2013-09-30           0.145  
#> 10         1 2013-10-31           0.0420 
#> # ℹ 182 more rows
```
