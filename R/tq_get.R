#' Get quantitative data in \code{tibble} format
#'
#' @param x A character string representing a single stock index,
#' stock symbol, metal symbol, currency combination, FRED code, etc.
#' @param get A character string representing the type of data to get
#' for \code{x}. Options include:
#' \itemize{
#'   \item \code{"stock.index"}: Get all stocks in an index or exchange
#'   from \href{http://www.marketvolume.com/indexes_exchanges/}{marketvolume.com}.
#'   \item \code{"stock.prices"}: Get the stock prices for a stock symbol from
#'   \href{https://finance.yahoo.com/}{Yahoo Finance}.
#'   \item \code{"dividends"}: Get the dividends for a stock symbol
#'   from \href{https://finance.yahoo.com/}{Yahoo Finance}.
#'   \item \code{"splits"}: Get the splits for a stock symbol
#'   from \href{https://finance.yahoo.com/}{Yahoo Finance}.
#'   \item \code{"financials"}: Get the income, balance sheet, and cash flow
#'   financial statements for a stock symbol from
#'   \href{https://www.google.com/finance}{Google Finance}.
#'   \item \code{"key.ratios"}: Get the growth, profitablity, financial health,
#'   and efficiency ratios for a stock symbol from
#'   \href{https://www.morningstar.com}{morningstar.com}.
#'   \item \code{"economic.data"}: Get economic data from
#'   \href{https://fred.stlouisfed.org/}{FRED}.
#'   \item \code{"metal.prices"}: Get the metal prices from
#'   \href{https://www.oanda.com/}{Oanda}.
#'   \item \code{"exchange.rates"}: Get exchange rates from
#'   \href{https://www.oanda.com/currency/converter/}{Oanda}.
#' }
#' @param ... Additional parameters passed to the appropriate \code{quantmod}
#' function. Common optional parameters include:
#' \itemize{
#'   \item \code{from}: Optional. A character string representing a start date in
#'   YYYY-MM-DD format. No effect on \code{get = "stock.index"},
#'   \code{"financials"}, or \code{"key.ratios"}.
#'   \item \code{to}: A character string representing a end date in
#'   YYYY-MM-DD format. No effect on \code{get = "stock.index"},
#'   \code{get = "financials"}, or \code{"key.ratios"}.
#'   \item \code{use_fallback}: Used with \code{get = "stock.index"} only.
#'   Set to \code{FALSE} by default. A boolean
#'   representing whether to use the fall back data set for a stock index. Useful
#'   if the data cannot be fetched from the website. The fallback data returned is
#'   accurate as of the date the package was last updated.
#' }
#'
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details
#' \code{tq_get()} is a consolidated function that gets data from various
#' web sources. The function is a wrapper for several \code{quantmod}
#' functions. The results are always returned as a \code{tibble}. The advantages
#' are (1) only one function is needed for all data sources and (2) the function
#' can be seemlessly used with the tidyverse: \code{purrr}, \code{tidyr}, and
#' \code{dplyr} verbs.
#'
#' \code{tq_get_options()} returns a list of valid `get` options you can
#' choose from.
#'
#' \code{tq_get_stock_index_options()} returns a list of stock indexes you can
#' choose from. Alternatively \code{tq_get("options", get = "stock.index")}
#' can be used.
#'
#' @rdname tq_get
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyverse)
#' library(tidyquant)
#'
#' ##### Basic Functionality
#'
#' # Get the list of `get` options
#' tq_get_options()
#'
#' # Get all stocks in a stock index from www.marketvolume.com
#' tq_get("SP500", get = "stock.index")
#'
#' # Get the list of stock index options that can be used with tq_get(get = "stock.index")
#' tq_get_stock_index_options()
#'
#' # Get stock prices for a stock from Yahoo
#' aapl_stock_prices <- tq_get("AAPL")
#' cvx_stock_prices  <- tq_get("CVX", get = "stock.prices",
#'                             from = "2014-01-01", to = "2015-01-01")
#'
#' # Get dividends and splits for a stock from Yahoo
#' tq_get("AAPL", get = "dividends", from = "1990-01-01")
#' tq_get("AAPL", get = "splits", from = "1990-01-01")
#'
#' # Get financial statement data for a stock from Google
#' appl_financials <- tq_get("AAPL", get = "financials")
#'
#' # Get key ratios for a stock from Morningstar
#' appl_key_ratios <- tq_get("AAPL", get = "key.ratios")
#'
#' # Get FRED economic data for a commodity code
#' tq_get("DCOILWTICO", get = "economic.data") # WTI crude oil spot prices
#'
#' # Get exchange rate data from Oanda
#' eur_usd <- tq_get("EUR/USD", get = "exchange.rates")
#'
#' # Get metal prices from Oanda
#' plat_price_usd <- tq_get("plat", get = "metal.prices")
#' gold_price_eur <- tq_get("gold", get = "metal.prices",
#'                                  base.currency = "EUR")
#'
#' ##### Tidyverse functionality
#'
#' # Get a historical stock prices from multiple stocks
#' FANG <- tibble(symbol = c("FB", "AMZN", "NFLX", "GOOGL")) %>%
#'     mutate(stock.prices = map(.x = symbol,
#'                               ~ tq_get(.x, get = "stock.prices"))) %>%
#'     unnest()
#'

# PRIMARY FUNCTIONS ----

tq_get <- function(x, get = "stock.prices", ...) {

    # Check get
    get <- stringr::str_to_lower(get) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "") %>%
        stringr::str_replace_all("s$", "")

    get_list <- tq_get_options() %>%
        stringr::str_replace_all("[[:punct:]]", "") %>%
        stringr::str_replace_all("s$", "")
    if (!(get %in% get_list)) {
        stop("Error: `get` must be a valid entry")
    }

    # Check x
    if (length(x) != 1) {
        stop("Error: Enter one value of x per request.
             Use purrr::map() to iterate a tibble.")
    }

    # Setup switches based on get
    ret <- switch(get,
                  stockprice   = tq_get_util_2(x, get, ...),
                  dividend     = tq_get_util_2(x, get, ...),
                  split        = tq_get_util_2(x, get, ...),
                  financial    = tq_get_util_2(x, get, ...),
                  keyratio     = tq_get_util_1(x, get, ...),
                  metalprice   = tq_get_util_2(x, get, ...),
                  exchangerate = tq_get_util_2(x, get, ...),
                  economicdata = tq_get_util_2(x, get, ...),
                  stockindex   = tq_get_util_3(x, get, ...)
                  )

    ret

}

#' @rdname tq_get
#' @export
tq_get_options <- function() {
    c("stock.prices",
      "stock.index",
      "dividends",
      "splits",
      "financials",
      "key.ratios",
      "economic.data",
      "exchange.rates",
      "metal.prices"
      )
}


#' @rdname tq_get
#' @export
tq_get_stock_index_options <- function() {
    c(  "DOWJONES",
        "DJI",
        "DJT",
        "DJU",
        "SP100",
        "SP400",
        "SP500",
        "SP600",
        "RUSSELL1000",
        "RUSSELL2000",
        "RUSSELL3000",
        "AMEX",
        "AMEXGOLD",
        "AMEXOIL",
        "NASDAQ",
        "NASDAQ100",
        "NYSE",
        "SOX"
    )
}

# UTILITY FUNCTIONS ----

# NOT USED: ISSUE WITH getYahooData
# Util 1: Used for tq_get() `get` options:
#     key.ratios -> From www.morningstar.com
tq_get_util_1 <- function(x, get, ...) {

    # Convert x to uppercase
    x <- stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "")

    # Check x
    if (!is.character(x)) {
        err <- "Error: x must be a character string in the form of a valid stock symbol."
        stop(err)
    }

    tryCatch({

        # Download file
        tmp <- tempfile()
        url_base_1 <- 'http://financials.morningstar.com/finan/ajax/exportKR2CSV.html?&callback=?&t=XNAS:'
        url_base_2 <- '&region=usa&culture=en-US&cur=&order=asc'
        url <- paste0(url_base_1, x, url_base_2)

        download.file(url, destfile = tmp, quiet = TRUE)

        # Setup Tibble Part 1
        key_ratios_1 <- tibble::tibble(
            section            = c(rep("Financials", 15),
                                   rep("Profitability", 17),
                                   rep("Growth", 16),
                                   rep("Cash Flow", 5),
                                   rep("Financial Health", 24),
                                   rep("Efficiency Ratios", 8)),
            sub.section        = c(rep("Financials", 15),
                                   rep("Margin of Sales %", 9),
                                   rep("Profitability", 8),
                                   rep("Revenue %", 4),
                                   rep("Operating Income %", 4),
                                   rep("Net Income %", 4),
                                   rep("EPS %", 4),
                                   rep("Cash Flow Ratios", 5),
                                   rep("Balance Sheet Items (in %)", 20),
                                   rep("Liquidty/Financial Health", 4),
                                   rep("Efficiency", 8))
        )

        # Setup Tibble Part 2

        # Read lines
        skip_rows <- c(1:2, 19:21, 31:32, 41:44, 49, 54, 59, 64:66, 72:74, 95:96, 101:103)
        text <- readr::read_lines(tmp)[-skip_rows]

        # Unlink tmp
        unlink(tmp)

        # Parse text
        key_ratios_2 <-
            suppressMessages(
                suppressWarnings(
                    utils::read.csv(text = text, na.strings=c("","NA")) %>%
                        tibble::as_tibble() %>%
                        dplyr::mutate_all(as.character)
                )
            )


        # Combine tibble parts into raw data
        key_ratios_raw <- dplyr::bind_cols(key_ratios_1, key_ratios_2)

        # Cleanup raw data
        key_ratios <- key_ratios_raw %>%
            dplyr::select(-TTM) %>%
            dplyr::rename(category = X) %>%
            tibble::rownames_to_column(var = "group") %>%
            dplyr::mutate(group = as.numeric(group)) %>%
            tidyr::gather(key = date, value = value, -c(group, section, sub.section, category)) %>%
            dplyr::arrange(group) %>%
            dplyr::mutate(date = stringr::str_sub(date, start = 2, end = length(date))) %>%
            dplyr::mutate(date = stringr::str_replace(date, "\\.", "-")) %>%
            dplyr::mutate(date = lubridate::ymd(date, truncated = 2)) %>%
            dplyr::mutate(value = stringr::str_replace(value, ",", "")) %>%
            dplyr::mutate(value = as.double(value)) %>%
            dplyr::select(section, sub.section, group, category, date, value) %>%
            dplyr::group_by(section) %>%
            tidyr::nest()

        return(key_ratios)

    }, error = function(e) {

        warn <- paste0("Error at ", x, " during call to get = key.ratios")
        warning(warn)
        return(NA) # Return NA on error

    })

}



# Util 2: Used for tq_get() `get` options:
#     financials      -> from quantmod::getFinancials()
#     metal.prices    -> from quantmod::getMetals()
#     exchange.rates  -> from quantmod::getFX()
#     economic.data   -> from quantmod::getSymbols.FRED()
tq_get_util_2 <-
    function(x,
             get,
             from = as.character(paste0(lubridate::year(lubridate::today()) - 10, "-01-01")),
             to   = as.character(lubridate::today()),
             ...) {

    # Setup switches based on get
    vars <- switch(get,
                   stockprice   = list(chr_x      = "stock symbol",
                                       fun        = quantmod::getSymbols,
                                       chr_fun    = "quantmod::getSymbols",
                                       list_names = c("open", "high", "low", "close", "volume", "adjusted"),
                                       source     = "yahoo"),
                   dividend     = list(chr_x      = "stock symbol",
                                       fun        = quantmod::getDividends,
                                       chr_fun    = "quantmod::getDividends",
                                       list_names = "dividends",
                                       source     = "yahoo"),
                   split        = list(chr_x      = "stock symbol",
                                       fun        = quantmod::getSplits,
                                       chr_fun    = "quantmod::getSplits",
                                       list_names = "splits",
                                       source     = "yahoo"),
                   financial    = list(chr_x      = "stock symbol",
                                       fun        = quantmod::getFinancials,
                                       chr_fun    = "quantmod::getFinancials",
                                       source     = "google"),
                   metalprice   = list(chr_x      = "metal symbol",
                                       fun        = quantmod::getMetals,
                                       chr_fun    = "quantmod::getMetals",
                                       list_names = "price",
                                       source     = "oanda"),
                   exchangerate = list(chr_x      = "exchange rate combination",
                                       fun        = quantmod::getFX,
                                       chr_fun    = "quantmod::getFX",
                                       list_names = "exchange.rate",
                                       source     = "oanda"),
                   economicdata = list(chr_x      = "economic symbol",
                                       fun        = quantmod::getSymbols,
                                       chr_fun    = "quantmod::getSymbols.FRED",
                                       list_names = "price",
                                       source     = "FRED")
    )

    # Check x
    if (!is.character(x)) {
        err <- paste0("Error: x must be a character string in the form of a valid ",
                      vars$chr_x)
        stop(err)
    }

    # Get data; Handle errors
    ret <- tryCatch({

        suppressWarnings(
            suppressMessages(
                vars$fun(x, src = vars$source, auto.assign = FALSE, from = from, to = to, ...)
            )
        )

    }, error = function(e) {

        warn <- paste0("Error at ", vars$chr_x, " ", x,
                       " during call to ", vars$chr_fun, ".")
        warning(warn)
        return(NA) # Return NA on error

    })

    # coerce financials to tibble
    if (identical(get, "financial") && class(ret) == "financials") {

        # Tidy a single financial statement
        tidy_fin <- function(x) {

            group <- 1:nrow(x)

            df <- dplyr::bind_cols(tibble::tibble(group),
                                   tidyquant::as_tibble(x, preserve_row_names = TRUE)) %>%
                dplyr::rename(category = row.names) %>%
                tidyr::gather(date, value, -c(category, group)) %>%
                dplyr::mutate(date = lubridate::ymd(date)) %>%
                dplyr::arrange(group)

            df

        }

        # Setup tibble and map tidy_fin function
        ret <- tibble::tibble(
            type = c("IS", "IS", "BS", "BS", "CF", "CF"),
            period = rep(c("A", "Q"), 3)) %>%
            dplyr::mutate(retrieve = paste0("ret$", type, "$", period)) %>%
            dplyr::mutate(data = purrr::map(retrieve, ~ eval(parse(text = .x)))) %>%
            dplyr::mutate(data = purrr::map(data, tidy_fin)) %>%
            dplyr::select(-retrieve) %>%
            tidyr::spread(key = period, value = data) %>%
            dplyr::rename(annual = A, quarter = Q)

    }

    # Coerce any xts to tibble
    if (xts::is.xts(ret)) {
        names(ret) <- vars$list_names
        ret <- ret %>%
            tidyquant::as_tibble(preserve_row_names = TRUE) %>%
            dplyr::rename(date = row.names) %>%
            dplyr::mutate(date = lubridate::ymd(date))

        # Filter economic data by date
        if (identical(get, "economicdata")) {
            ret <- ret %>%
                dplyr::filter(date >= lubridate::ymd(from) & date <= lubridate::ymd(to))
        }
    }

    ret

}


# Util 3: Used for tq_get() `get` options:
#     stock.index     -> web scraped from www.marketvolume.com
tq_get_util_3 <-
    function(x, get, use_fallback = FALSE, ...) {

    x <- stringr::str_to_upper(x) %>%
         stringr::str_trim(side = "both") %>%
         stringr::str_replace_all("[[:punct:]]", "")

    # Check if x is an appropriate index
    index_list <- tq_get_stock_index_options()
    if (!(x %in% c(index_list, "OPTIONS"))) {
        err <- paste0("Error: x must be a character string in the form of a valid index.",
                      " The following are valid options:\n",
                      stringr::str_c(index_list, collapse = ", ")
        )
        stop(err)
    }

    # Show options or collect data
    if (x == "OPTIONS") {

        ret <- index_list

    } else {

        # Setup switches based on `get`
        vars <- switch(x,
                       DJI              = list(chr_x      = "index",
                                               a          = "dji_components",
                                               b          = "DJI",
                                               max_page   = 1),
                       DJT              = list(chr_x      = "index",
                                               a          = "djt_components",
                                               b          = "DJT",
                                               max_page   = 1),
                       DJU              = list(chr_x      = "index",
                                               a          = "dju_components",
                                               b          = "DJU",
                                               max_page   = 1),
                       DOWJONES         = list(chr_x      = "index",
                                               a          = "dja_components",
                                               b          = "DJA",
                                               max_page   = 1),
                       NASDAQ100        = list(chr_x      = "index",
                                               a          = "n100_components",
                                               b          = "SPX",
                                               max_page   = 2),
                       SP100            = list(chr_x      = "index",
                                               a          = "sp100_components",
                                               b          = "OEX",
                                               max_page   = 1),
                       SP400            = list(chr_x      = "index",
                                               a          = "sp400_components",
                                               b          = "SP400",
                                               max_page   = 2),
                       SP500            = list(chr_x      = "index",
                                               a          = "sp500_components",
                                               b          = "SPX",
                                               max_page   = 2),
                       SP600            = list(chr_x      = "index",
                                               a          = "sp600_components",
                                               b          = "SP600",
                                               max_page   = 3),
                       RUSSELL1000      = list(chr_x      = "index",
                                               a          = "r1000_components",
                                               b          = "RUI",
                                               max_page   = 4),
                       RUSSELL2000      = list(chr_x      = "index",
                                               a          = "r2000_components",
                                               b          = "RUT",
                                               max_page   = 9),
                       RUSSELL3000      = list(chr_x      = "index",
                                               a          = "r3000_components",
                                               b          = "RUA",
                                               max_page   = 13),
                       AMEX             = list(chr_x      = "exchange",
                                               a          = "amex_components",
                                               b          = "XAX",
                                               max_page   = 2),
                       NASDAQ           = list(chr_x      = "exchange",
                                               a          = "nasdaq_components",
                                               b          = "COMP",
                                               max_page   = 9),
                       NYSE             = list(chr_x      = "index",
                                               a          = "nyse_components",
                                               b          = "NYA",
                                               max_page   = 11),
                       AMEXGOLD         = list(chr_x      = "index",
                                               a          = "gold_components",
                                               b          = "HUI",
                                               max_page   = 1),
                       AMEXOIL          = list(chr_x      = "index",
                                               a          = "oil_components",
                                               b          = "XOI",
                                               max_page   = 1),
                       SOX              = list(chr_x      = "index",
                                               a          = "sem_components",
                                               b          = "SOX",
                                               max_page   = 1)

        )

        if (!use_fallback) {

            # Base path and page rows from www.marketvolume.com
            base_path_1 <- "http://www.marketvolume.com/indexes_exchanges/"
            base_path_2 <- ".asp?s="
            base_path_3 <- "&row="
            base_path <- paste0(base_path_1, vars$a, base_path_2, vars$b, base_path_3)

            # Get page numbers; Add 1 to max page to adjust for 250 stock list expansion.
            row_num <- seq(from = 0, by = 250, length.out = vars$max_page + 1)

            message("Getting data...\n")

            # Function to map
            get_stock_index <- function(base_path, row_num) {
                path <- paste0(base_path, row_num)
                # rvest functions: Get table of stocks
                stock_table <- xml2::read_html(curl::curl(path, handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>%
                    rvest::html_node("table") %>%
                    rvest::html_table()
                # Format table
                stock_table <- stock_table[-1, 1:2] %>%
                    tibble::as_tibble() %>%
                    dplyr::rename(symbol = X1, company = X2)
                stock_table
            }

            # Map function; return stockindex
            stock_index <- tryCatch({

                tibble::tibble(row_num) %>%
                    dplyr::mutate(
                        stock_table = purrr::map(row_num,
                                                 function(.x) get_stock_index(base_path = base_path, row_num = .x)
                        )
                    ) %>%
                    tidyr::unnest() %>%
                    dplyr::select(-row_num) %>%
                    dplyr::mutate_all(function(x) stringr::str_trim(x, side = 'both') %>%
                                          stringr::str_to_upper()) %>%
                    dplyr::distinct()

            }, error = function(e) {

                path <- paste0(base_path, 0)
                con <- pipe(path)
                close(con)
                warning(paste0("Could not access ", path, ". ",
                                "If problem persists, try setting `use_fallback = TRUE` ",
                                "to return the last dowloaded data set."))

                NA

            })

        } else {

            # load("R/sysdata.rda")
            date.dload <- stock_indexes %>%
                dplyr::filter(index.option == x) %>%
                dplyr::select(date.downloaded)
            message(paste0("Using fallback dataset last downloaded ",
                           date.dload[[1]]), ".")
            stock_index <- stock_indexes %>%
                dplyr::filter(index.option == x) %>%
                dplyr::select(index.components) %>%
                tidyr::unnest()

        }


        ret <- stock_index

    }

    ret

}
