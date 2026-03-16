# Search the Nasdaq Data Link database

Search the Nasdaq Data Link database

## Usage

``` r
quandl_search(query, silent = FALSE, per_page = 10, ...)
```

## Arguments

- query:

  A character string giving the search term passed to the Nasdaq Data
  Link dataset search endpoint.

- silent:

  A logical indicating whether to suppress console output for matching
  datasets.

- per_page:

  An integer specifying how many search results to return per page.

- ...:

  Additional arguments passed to the Nasdaq Data Link search API.

## Value

Returns a tibble with search results.

## Details

A wrapper around the Nasdaq Data Link dataset search endpoint.

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
`get = "quandl"`

## Examples

``` r
if (FALSE) { # \dontrun{
quandl_search(query = "oil")
} # }
```
