# Search the Quandl database

Search the Quandl database

## Usage

``` r
quandl_search(query, silent = FALSE, per_page = 10, ...)
```

## Arguments

- query:

  Search terms

- silent:

  Prints the results when FALSE.

- per_page:

  Number of results returned per page.

- ...:

  Additional named values that are interpretted as Quandl API
  parameters.

## Value

Returns a tibble with search results.

## Details

A wrapper for
[`Quandl::Quandl.search()`](https://rdrr.io/pkg/Quandl/man/Quandl.search.html)

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
`get = "quandl"`

## Examples

``` r
if (FALSE) { # \dontrun{
quandl_search(query = "oil")
} # }
```
