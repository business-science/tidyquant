# Stock prices for the "FANG" stocks.

A dataset containing the daily historical stock prices for the "FANG"
tech stocks, "META", "AMZN", "NFLX", and "GOOG", spanning from the
beginning of 2013 through the end of 2016.

## Usage

``` r
FANG
```

## Format

A "tibble" ("tidy" data frame) with 4,032 rows and 8 variables:

- symbol:

  stock ticker symbol

- date:

  trade date

- open:

  stock price at the open of trading, in USD

- high:

  stock price at the highest point during trading, in USD

- low:

  stock price at the lowest point during trading, in USD

- close:

  stock price at the close of trading, in USD

- volume:

  number of shares traded

- adjusted:

  stock price at the close of trading adjusted for stock splits, in USD

## Source

<https://www.investopedia.com/terms/f/fang-stocks-fb-amzn.asp>
