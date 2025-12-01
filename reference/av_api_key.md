# Set Alpha Vantage API Key

Requires the alphavantager packager to use.

## Usage

``` r
av_api_key(api_key)
```

## Arguments

- api_key:

  Optionally passed parameter to set Alpha Vantage `api_key`.

## Value

Returns invisibly the currently set `api_key`

## Details

A wrapper for
[`alphavantager::av_api_key()`](https://rdrr.io/pkg/alphavantager/man/av_api_key.html)

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
`get = "alphavantager"`

## Examples

``` r
if (FALSE) { # \dontrun{
if (rlang::is_installed("alphavantager")) {
av_api_key(api_key = "foobar")
}
} # }
```
