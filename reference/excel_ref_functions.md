# Excel Reference Functions

Excel reference functions are used to efficiently lookup values from a
data source. The most popular lookup function is "VLOOKUP", which has
been implemented in R.

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names are similar to Excel function names

- Functionality replicates Excel

## Usage

``` r
VLOOKUP(.lookup_values, .data, .lookup_column, .return_column)
```

## Arguments

- .lookup_values:

  One or more lookup values.

- .data:

  A `data.frame` or `tibble` that contains values to evaluate and return

- .lookup_column:

  The column in `.data` containing exact matching values of the
  `.lookup_values`

- .return_column:

  The column in `.data` containing the values to return if a match is
  found

## Value

Returns a vector the length of the input lookup values

## Details

**`VLOOKUP()` Details**

- Performs exact matching only. Fuzzy matching is not implemented.

- Can only return values from one column only. Use
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  to perform table joining.

## Examples

``` r
library(dplyr)

lookup_table <- tibble(
    stock   = c("META", "AMZN", "NFLX", "GOOG"),
    company = c("Facebook", "Amazon", "Netflix", "Google")
)

# --- Basic Usage ---

VLOOKUP("NFLX",
        .data = lookup_table,
        .lookup_column = stock,
        .return_column = company)
#> [1] "Netflix"

# --- Usage with tidyverse ---

# Add company names to the stock data
FANG %>%
    mutate(company = VLOOKUP(symbol, lookup_table, stock, company))
#> # A tibble: 4,032 × 9
#>    symbol date        open  high   low close    volume adjusted company 
#>    <chr>  <date>     <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl> <chr>   
#>  1 META   2013-01-02  27.4  28.2  27.4  28    69846400     28   Facebook
#>  2 META   2013-01-03  27.9  28.5  27.6  27.8  63140600     27.8 Facebook
#>  3 META   2013-01-04  28.0  28.9  27.8  28.8  72715400     28.8 Facebook
#>  4 META   2013-01-07  28.7  29.8  28.6  29.4  83781800     29.4 Facebook
#>  5 META   2013-01-08  29.5  29.6  28.9  29.1  45871300     29.1 Facebook
#>  6 META   2013-01-09  29.7  30.6  29.5  30.6 104787700     30.6 Facebook
#>  7 META   2013-01-10  30.6  31.5  30.3  31.3  95316400     31.3 Facebook
#>  8 META   2013-01-11  31.3  32.0  31.1  31.7  89598000     31.7 Facebook
#>  9 META   2013-01-14  32.1  32.2  30.6  31.0  98892800     31.0 Facebook
#> 10 META   2013-01-15  30.6  31.7  29.9  30.1 173242600     30.1 Facebook
#> # ℹ 4,022 more rows
```
