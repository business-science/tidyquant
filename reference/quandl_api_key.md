# Query or set Nasdaq Data Link API Key

Query or set Nasdaq Data Link API Key

## Usage

``` r
quandl_api_key(api_key)
```

## Arguments

- api_key:

  Optionally passed parameter to set the Nasdaq Data Link `api_key`.

## Value

Returns invisibly the currently set `api_key`.

## Details

`quandl_api_key()` stores the API key used by tidyquant's built-in
Nasdaq Data Link client. The option name remains `Quandl.api_key` for
backward compatibility.

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
`get = "quandl"`

## Examples

``` r
if (FALSE) { # \dontrun{
quandl_api_key(api_key = "foobar")
} # }
```
