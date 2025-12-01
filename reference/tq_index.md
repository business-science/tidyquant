# Get all stocks in a stock index or stock exchange in `tibble` format

Get all stocks in a stock index or stock exchange in `tibble` format

## Usage

``` r
tq_index(x, use_fallback = FALSE)

tq_index_options()

tq_exchange(x)

tq_exchange_options()

tq_fund_holdings(x, source = "SSGA")

tq_fund_source_options()
```

## Arguments

- x:

  A single character string, a character vector or tibble representing a
  single stock index or multiple stock indexes.

- use_fallback:

  A boolean that can be used to return a fallback data set last
  downloaded when the package was updated. Useful if the website is
  down. Set to `FALSE` by default.

- source:

  The API source to use.

## Value

Returns data in the form of a `tibble` object.

## Details

`tq_index()` returns the stock symbol, company name, weight, and sector
of every stock in an index.

`tq_index_options()` returns a list of stock indexes you can choose
from.

`tq_exchange()` returns the stock symbol, company, last sale price,
market capitalization, sector and industry of every stock in an
exchange. Three stock exchanges are available (AMEX, NASDAQ, and NYSE).

`tq_exchange_options()` returns a list of stock exchanges you can choose
from. The options are AMEX, NASDAQ and NYSE.

`tq_fund_holdings()` returns the the stock symbol, company name, weight,
and sector of every stock in an fund. The `source` parameter specifies
which investment management company to use. Example: `source = "SSGA"`
connects to State Street Global Advisors (SSGA). If `x = "SPY"`, then
SPDR SPY ETF holdings will be returned.

`tq_fund_source_options()`: returns the options that can be used for the
`source` API for `tq_fund_holdings()`.

## See also

[`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)
to get stock prices, financials, key stats, etc using the stock symbols.

## Examples

``` r
# Stock Indexes:

# Get the list of stock index options
tq_index_options()
#> [1] "DOW"       "DOWGLOBAL" "SP400"     "SP500"     "SP600"    

# Get all stock symbols in a stock index
if (FALSE) { # \dontrun{
tq_index("DOW")
} # }

# Stock Exchanges:

# Get the list of stock exchange options
tq_exchange_options()
#> [1] "AMEX"   "NASDAQ" "NYSE"  

# Get all stocks in a stock exchange
if (FALSE) { # \dontrun{
tq_exchange("NYSE")
} # }

# Mutual Funds and ETFs:

# Get the list of stock exchange options
tq_fund_source_options()
#> [1] "SSGA"

# Get all stocks in a fund
if (FALSE) { # \dontrun{
tq_fund_holdings("SPY", source = "SSGA")
} # }
```
