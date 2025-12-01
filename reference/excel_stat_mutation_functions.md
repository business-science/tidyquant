# Excel Statistical Mutation Functions

15+ common statistical functions familiar to users of Excel (e.g.
`ABS()`, `SQRT()`) that **modify / transform** a series of values (i.e.
a vector of the same length of the input is returned).

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names in most cases match Excel function names

- Functionality replicates Excel

- By default, missing values are ignored (same as in Excel)

## Usage

``` r
ABS(x)

SQRT(x)

LOG(x)

EXP(x)

RETURN(x, n = 1, fill_na = NA)

PCT_CHANGE(x, n = 1, fill_na = NA)

CHANGE(x, n = 1, fill_na = NA)

LAG(x, n = 1, fill_na = NA)

LEAD(x, n = 1, fill_na = NA)

CUMULATIVE_SUM(x)

CUMULATIVE_PRODUCT(x)

CUMULATIVE_MAX(x)

CUMULATIVE_MIN(x)

CUMULATIVE_MEAN(x)

CUMULATIVE_MEDIAN(x)
```

## Arguments

- x:

  A vector. Most functions are designed for numeric data.

- n:

  Values to offset. Used in functions like `LAG()`, `LEAD()`, and
  `PCT_CHANGE()`

- fill_na:

  Fill missing (`NA`) values with a different value. Used in offsetting
  functions.

## Value

- **Mutation functions** return a mutated / transformed version of the
  vector

## Useful functions

**Mutation Functions** - Transforms a vector

- Transformation: `ABS()`, `SQRT()`, `LOG()`, `EXP()`

- Lags & Change (Offsetting Functions): `CHANGE()`, `PCT_CHANGE()`,
  `LAG()`, `LEAD()`

- Cumulative Totals: `CUMULATIVE_SUM()`, `CUMULATIVE_PRODUCT()`

## Examples

``` r
# Libraries
library(timetk, exclude = "FANG")
library(dplyr)

# --- Basic Usage ----

CUMULATIVE_SUM(1:10)
#>  [1]  1  3  6 10 15 21 28 36 45 55

PCT_CHANGE(c(21, 24, 22, 25), fill_na = 0)
#> [1]  0.00000000  0.14285714 -0.08333333  0.13636364

# --- Usage with tidyverse ---

# Go from daily to monthly periodicity,
# then calculate returns and growth of $1 USD
FANG %>%
    mutate(symbol = forcats::as_factor(symbol)) %>%
    group_by(symbol) %>%

    # Summarization - Collapse from daily to FIRST value by month
    summarise_by_time(
        .date_var  = date,
        .by        = "month",
        adjusted   = FIRST(adjusted)
    ) %>%

    # Mutation - Calculate monthly returns and cumulative growth of $1 USD
    group_by(symbol) %>%
    mutate(
        returns = PCT_CHANGE(adjusted, fill_na = 0),
        growth  = CUMULATIVE_SUM(returns) + 1
    )
#> # A tibble: 192 × 5
#> # Groups:   symbol [4]
#>    symbol date       adjusted returns growth
#>    <fct>  <date>        <dbl>   <dbl>  <dbl>
#>  1 META   2013-01-01     28    0       1    
#>  2 META   2013-02-01     29.7  0.0618  1.06 
#>  3 META   2013-03-01     27.8 -0.0656  0.996
#>  4 META   2013-04-01     25.5 -0.0810  0.915
#>  5 META   2013-05-01     27.4  0.0744  0.990
#>  6 META   2013-06-01     23.8 -0.131   0.859
#>  7 META   2013-07-01     24.8  0.0403  0.899
#>  8 META   2013-08-01     37.5  0.511   1.41 
#>  9 META   2013-09-01     41.9  0.117   1.53 
#> 10 META   2013-10-01     50.4  0.204   1.73 
#> # ℹ 182 more rows
```
