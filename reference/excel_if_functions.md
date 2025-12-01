# Excel Summarising "If" Functions

**"IFS" functions** are filtering versions of their summarization
counterparts. Simply add "cases" that filter if a condition is true.
Multiple cases are evaluated as "AND" filtering operations. A single
case with `|` ("OR") bars can be created to accomplish an "OR". See
details below.

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names are similar to Excel function names

- By default, missing values are ignored (same as in Excel)

## Usage

``` r
SUM_IFS(x, ...)

COUNT_IFS(x, ...)

AVERAGE_IFS(x, ...)

MEDIAN_IFS(x, ...)

MIN_IFS(x, ...)

MAX_IFS(x, ...)

CREATE_IFS(.f, ...)
```

## Arguments

- x:

  A vector. Most functions are designed for numeric data. Some functions
  like `COUNT_IFS()` handle multiple data types.

- ...:

  Add cases to evaluate. See Details.

- .f:

  A function to convert to an "IFS" function. Use `...` in this case to
  provide parameters to the `.f` like `na.rm = TRUE`.

## Value

- **Summary functions** return a single value

## Details

**"AND" Filtering:** Multiple cases are evaluated as "AND" filtering
operations.

**"OR" Filtering:** Compound single cases with `|` ("OR") bars can be
created to accomplish an "OR". Simply use a statement like
`x > 10 | x < -10` to perform an "OR" if-statement.

**Creating New "Summarizing IFS" Functions:** Users can create new "IFS"
functions using the `CREATE_IFS()` function factory. The only
requirement is that the output of your function (`.f`) must be a single
value (scalar). See examples below.

## Useful Functions

**Summary Functions** - Return a single value from a vector

- Sum: `SUM_IFS()`

- Center: `AVERAGE_IFS()`, `MEDIAN_IFS()`

- Count: `COUNT_IFS()`

- Range: `MIN_IFS()`, `MAX_IFS()`

**Create your own summary "IFS" function**

- `CREATE_IFS()`: This is a function factory that generates summary
  "\_IFS" functions.

## Examples

``` r
library(dplyr)
library(timetk, exclude = "FANG")
library(stringr)
library(lubridate)

# --- Basic Usage ---

SUM_IFS(x = 1:10, x > 5)
#> [1] 40

COUNT_IFS(x = letters, str_detect(x, "a|b|c"))
#> [1] 3

SUM_IFS(-10:10, x > 8 | x < -5)
#> [1] -21

# Create your own IFS function (Mind blowingly simple)!
Q75_IFS <- CREATE_IFS(.f = quantile, probs = 0.75, na.rm = TRUE)
Q75_IFS(1:10, x > 5)
#> 75% 
#>   9 

# --- Usage with tidyverse ---

# Using multiple cases IFS cases to count the frequency of days with
# high trade volume in a given year
FANG %>%
    group_by(symbol) %>%
    summarise(
        high_volume_in_2015 = COUNT_IFS(volume,
                                        year(date) == 2015,
                                        volume > quantile(volume, 0.75))
    )
#> # A tibble: 4 × 2
#>   symbol high_volume_in_2015
#>   <chr>                <int>
#> 1 AMZN                    62
#> 2 GOOG                    19
#> 3 META                    15
#> 4 NFLX                    54

# Count negative returns by month
FANG %>%
    mutate(symbol = forcats::as_factor(symbol)) %>%
    group_by(symbol) %>%

    # Collapse from daily to FIRST value by month
    summarise_by_time(
        .date_var  = date,
        .by        = "month",
        adjusted   = FIRST(adjusted)
    ) %>%

    # Calculate monthly returns
    group_by(symbol) %>%
    mutate(
        returns = PCT_CHANGE(adjusted, fill_na = 0)
    ) %>%

    # Find returns less than zero and count the frequency
    summarise(
        negative_monthly_returns = COUNT_IFS(returns, returns < 0)
    )
#> # A tibble: 4 × 2
#>   symbol negative_monthly_returns
#>   <fct>                     <int>
#> 1 META                         16
#> 2 AMZN                         16
#> 3 NFLX                         16
#> 4 GOOG                         20
```
