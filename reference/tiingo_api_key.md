# Set Tiingo API Key

Requires the riingo package to be installed.

## Usage

``` r
tiingo_api_key(api_key)
```

## Arguments

- api_key:

  Optionally passed parameter to set Tiingo `api_key`.

## Value

Returns invisibly the currently set `api_key`

## Details

A wrapper for `riingo::ringo_set_token()`

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
`get = "tiingo"`

## Examples

``` r
if (FALSE) { # \dontrun{
  tiingo_api_key(api_key = "foobar")
} # }
```
