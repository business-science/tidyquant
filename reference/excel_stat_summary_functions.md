# Excel Statistical Summary Functions

15+ common statistical functions familiar to users of Excel (e.g.
`SUM()`, `AVERAGE()`). These functions return a **single value** (i.e. a
vector of length 1).

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names in most cases match Excel function names

- Functionality replicates Excel

- By default, missing values are ignored (same as in Excel)

## Usage

``` r
SUM(x)

AVERAGE(x)

MEDIAN(x)

MIN(x)

MAX(x)

COUNT(x)

COUNT_UNIQUE(x)

STDEV(x)

VAR(x)

COR(x, y)

COV(x, y)

FIRST(x)

LAST(x)

NTH(x, n = 1)

CHANGE_FIRSTLAST(x)

PCT_CHANGE_FIRSTLAST(x)
```

## Arguments

- x:

  A vector. Most functions are designed for numeric data. Some functions
  like `COUNT()` handle multiple data types.

- y:

  A vector. Used in functions requiring 2 inputs.

- n:

  A single value used in `NTH()` to select a specific element location
  to return.

## Value

- **Summary functions** return a single value

## Details

**Summary Functions**

- All functions remove missing values (`NA`). This is the same behavior
  as in Excel and most commonly what is desired.

## Useful functions

**Summary Functions** - Return a single value from a vector

- Sum: `SUM()`

- Center: `AVERAGE()`, `MEDIAN()`

- Spread: `STDEV()`, `VAR()`

- Range: `MIN()`, `MAX()`

- Count: `COUNT()`, `COUNT_UNIQUE()`

- Position: `FIRST()`, `LAST()`, `NTH()`

- Change (Summary): `CHANGE_FIRSTLAST()`, `PCT_CHANGE_FIRSTLAST()`

- Correlation: `COR()`, `COV()`

## Examples

``` r
# Libraries
library(timetk, exclude = "FANG")
library(forcats)
library(dplyr)

# --- Basic Usage ----

SUM(1:10)
#> [1] 55

PCT_CHANGE_FIRSTLAST(c(21, 24, 22, 25))
#> [1] 0.1904762

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
    )
#> # A tibble: 192 × 3
#> # Groups:   symbol [4]
#>    symbol date       adjusted
#>    <fct>  <date>        <dbl>
#>  1 META   2013-01-01     28  
#>  2 META   2013-02-01     29.7
#>  3 META   2013-03-01     27.8
#>  4 META   2013-04-01     25.5
#>  5 META   2013-05-01     27.4
#>  6 META   2013-06-01     23.8
#>  7 META   2013-07-01     24.8
#>  8 META   2013-08-01     37.5
#>  9 META   2013-09-01     41.9
#> 10 META   2013-10-01     50.4
#> # ℹ 182 more rows
```
