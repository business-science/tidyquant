# Query or set Quandl API Key

Query or set Quandl API Key

## Usage

``` r
quandl_api_key(api_key)
```

## Arguments

- api_key:

  Optionally passed parameter to set Quandl `api_key`.

## Value

Returns invisibly the currently set `api_key`

## Details

A wrapper for
[`Quandl::Quandl.api_key()`](https://rdrr.io/pkg/Quandl/man/Quandl.api_key.html)

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
`get = "quandl"`

## Examples

``` r
if (FALSE) { # \dontrun{
if (rlang::is_installed("Quandl")) {
quandl_api_key(api_key = "foobar")
}
} # }
```
