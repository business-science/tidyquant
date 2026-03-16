# Search the Quandl database

Search the Quandl database

## Usage

``` r
quandl_search(query, silent = FALSE, per_page = 10, ...)
```

## Arguments

- query:

  A character string giving the search term passed to
  [`Quandl::Quandl.search()`](https://rdrr.io/pkg/Quandl/man/Quandl.search.html).

- silent:

  A logical indicating whether to suppress messages from
  [`Quandl::Quandl.search()`](https://rdrr.io/pkg/Quandl/man/Quandl.search.html).

- per_page:

  An integer specifying how many search results to return per page.

- ...:

  Additional arguments passed to
  [`Quandl::Quandl.search()`](https://rdrr.io/pkg/Quandl/man/Quandl.search.html).

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
