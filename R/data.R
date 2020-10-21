#' Stock prices for the "FANG" stocks.
#'
#' A dataset containing the daily historical stock prices for the "FANG" tech stocks,
#' "FB", "AMZN", "NFLX", and "GOOG", spanning from the beginning of
#' 2013 through the end of 2016.
#'
#' @format A "tibble" ("tidy" data frame) with 4,032 rows and 8 variables:
#' \describe{
#'   \item{symbol}{stock ticker symbol}
#'   \item{date}{trade date}
#'   \item{open}{stock price at the open of trading, in USD}
#'   \item{high}{stock price at the highest point during trading, in USD}
#'   \item{low}{stock price at the lowest point during trading, in USD}
#'   \item{close}{stock price at the close of trading, in USD}
#'   \item{volume}{number of shares traded}
#'   \item{adjusted}{stock price at the close of trading adjusted for stock splits, in USD}
#' }
#' @source \url{https://www.investopedia.com/terms/f/fang-stocks-fb-amzn.asp}
"FANG"
