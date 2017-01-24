#' Get quantitative data in \code{tibble} format
#'
#' @param x A single character string, a character vector or tibble representing a single (or multiple)
#' stock symbol, metal symbol, currency combination, FRED code, etc.
#' @param get A character string representing the type of data to get
#' for \code{x}. Options include:
#' \itemize{
#'   \item \code{"stock.prices"}: Get the open, high, low, close, volume and adjusted
#'   stock prices for a stock symbol from
#'   \href{https://finance.yahoo.com/}{Yahoo Finance}.
#'   \item \code{"financials"}: Get the income, balance sheet, and cash flow
#'   financial statements for a stock symbol from
#'   \href{https://www.google.com/finance}{Google Finance}.
#'   \item \code{"key.ratios"}: These are key historical ratios. Get \strong{89 historical growth, profitablity, financial health,
#'   efficiency, and valuation ratios that span 10-years} from
#'   \href{https://www.morningstar.com}{Morningstar}.
#'   \item \code{"key.stats"}: These are key current statistics. Get \strong{55 current key statistics} such as
#'   Ask, Bid, Day's High, Day's Low, Last Trade Price, current P/E Ratio, EPS,
#'   Market Cap, EPS Projected Current Year, EPS Projected Next Year and many more from
#'   \href{https://finance.yahoo.com/}{Yahoo Finance}.
#'   \item \code{"dividends"}: Get the dividends for a stock symbol
#'   from \href{https://finance.yahoo.com/}{Yahoo Finance}.
#'   \item \code{"splits"}: Get the splits for a stock symbol
#'   from \href{https://finance.yahoo.com/}{Yahoo Finance}.
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
#'   YYYY-MM-DD format. No effect on
#'   \code{"financials"}, \code{"key.ratios"}, or \code{"key.stats"}.
#'   \item \code{to}: A character string representing a end date in
#'   YYYY-MM-DD format. No effect on
#'   \code{get = "financials"},  \code{"key.ratios"}, or \code{"key.stats"}.
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
#' \code{tq_get_stock_index_options()} Is deprecated and will be removed in the
#' next version. Please use \code{tq_index_options()} instead.
#'
#' @seealso \code{\link{tq_index}} to get a ful list of stocks in an index.
#'
#' @rdname tq_get
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' # Get the list of `get` options
#' tq_get_options()
#'
#' # Get stock prices for a stock from Yahoo
#' aapl_stock_prices <- tq_get("AAPL")
#'
#' # Get stock prices for multiple stocks
#' mult_stocks <- tq_get(c("FB", "AMZN"),
#'                       get  = "stock.prices",
#'                       from = "2016-01-01",
#'                       to   = "2017-01-01")




# PRIMARY FUNCTIONS ----

tq_get <- function(x, get = "stock.prices", ...) {

    # Deprecated, remove next version
    if ("stock.index" %in% get) {
        warning("The 'stock.index' option is deprecated and will be removed in the next version. Please use tq_index() instead.")
    }

    # Handle x
    if (inherits(x, "data.frame")) {

        # Prevent issues with grouped_df's
        if (inherits(x, "grouped_df")) {
            warning("Ungrouping grouped data.frame")
            x <- dplyr::ungroup(x)
        }

        col_name <- colnames(x)[[1]]

        names(x)[[1]] <- "symbol.."

        x <- x %>%
            tibble::as_tibble()

        ret <- tq_get_multiple(x = x, get = get, ...)

        names(ret)[[1]] <- col_name

    } else if (is.character(x) && length(x) > 1) {

        col_name <- names(x)

        if (is.null(col_name)) col_name <- "symbol.x"

        x <- tibble::tibble(symbol.. = x)

        ret <- tq_get_multiple(x = x, get = get, ...)

        names(ret)[[1]] <- col_name

    } else if (is.character(x) && length(x) == 1) {

        ret <- tq_get_base(x = x, get = get, ...)

    } else {

        stop("x must be a single character, list of characters, or data frame of characters with the first column being the object to pass to tq_get.")

    }

    ret

}

tq_get_multiple <- function(x, get, ...) {

    # Map tq_get_base
    ret <- x %>%
        dplyr::mutate(data.. = purrr::map(.x = symbol..,
                                         ~ tq_get_base(x = .x,
                                                       get = get,
                                                       ...)),
                      class.. = purrr::map_chr(.x = data.., ~ class(.x)[[1]])) %>%
        dplyr::filter(class.. != "logical") %>%
        dplyr::select(-class..) %>%
        tidyr::unnest()

    ret

}

tq_get_base <- function(x, get, ...) {

    # Check get
    get <- stringr::str_to_lower(get) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "") %>%
        stringr::str_replace_all("s$", "")

    get_list <- tq_get_options() %>%
        stringr::str_replace_all("[[:punct:]]", "") %>%
        stringr::str_replace_all("s$", "")
    if (!(get %in% c(get_list, "stockindex"))) {
        stop("Error: `get` must be a valid entry")
    }

    # Check x
    if (length(x) != 1) {
        stop("Error: Enter one value of x per request.
             Use purrr::map() to iterate a tibble.")
    }

    # Setup switches based on get
    ret <- switch(get,
                  stockprice   = tq_get_util_1(x, get, ...),
                  dividend     = tq_get_util_1(x, get, ...),
                  split        = tq_get_util_1(x, get, ...),
                  financial    = tq_get_util_1(x, get, ...),
                  keystat      = tq_get_util_3(x, get, ...),
                  keyratio     = tq_get_util_2(x, get, ...),
                  metalprice   = tq_get_util_1(x, get, ...),
                  exchangerate = tq_get_util_1(x, get, ...),
                  economicdata = tq_get_util_1(x, get, ...),
                  stockindex   = tq_index(x, ...) # Deprecated, remove next version
                  )

    ret

}

#' @rdname tq_get
#' @export
tq_get_options <- function() {
    c("stock.prices",
      "financials",
      "key.stats",
      "key.ratios",
      "dividends",
      "splits",
      "economic.data",
      "exchange.rates",
      "metal.prices"
      )
}

# Deprecated, remove next version
#' @rdname tq_get
#' @export
tq_get_stock_index_options <- function() {
    warning("tq_get_stock_index_options() is deprecated and will be removed in the next version. Please use tq_index_options().")
    tq_index_options()
}


# UTILITY FUNCTIONS ----

# Util 1: stock.prices, financials, economic.data -----
tq_get_util_1 <-
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

# Util 2: key.ratios -----
tq_get_util_2 <- function(x, get, ...) {

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
        stock_exchange <- c("XNAS", "XNYS") # mornginstar gets from various exchanges
        url_base_1 <- 'http://financials.morningstar.com/finan/ajax/exportKR2CSV.html?&callback=?&t='
        url_base_2 <- '&region=usa&culture=en-US&cur=&order=asc'
        # Two URLs to try
        url <- paste0(url_base_1, stock_exchange, ":", x, url_base_2)

        # Try various stock exchanges
        download.file(url[1], destfile = tmp, quiet = TRUE)
        if (length(readLines(tmp)) == 0) {
            download.file(url[2], destfile = tmp, quiet = TRUE)
        }


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
        key_ratios_bind <- key_ratios_raw %>%
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
            dplyr::select(section, sub.section, group, category, date, value)

        # Calculate valuations

        # Get stock prices
        from = lubridate::today() - lubridate::years(12)
        valuations_2 <- tq_get(x, get = "stock.prices", from = from) %>%
            tq_transform_xy(adjusted, transform_fun = to.period, period = "years") %>%
            dplyr::mutate(year = lubridate::year(date)) %>%
            dplyr::select(year, date, adjusted)

        # Get key ratios
        valuations_1 <- key_ratios_bind %>%
            dplyr::filter(section == "Financials") %>%
            dplyr::filter(category %in% c("Revenue USD Mil",
                                          "Shares Mil",
                                          "Earnings Per Share USD",
                                          "Book Value Per Share * USD",
                                          "Operating Cash Flow USD Mil")) %>%
            dplyr::mutate(year = lubridate::year(date)) %>%
            dplyr::select(year, category, value) %>%
            tidyr::spread(key = category, value = value) %>%
            dplyr::mutate(`Revenue Per Share USD` = `Revenue USD Mil` / `Shares Mil`,
                          `Cash Flow Per Share USD` = `Operating Cash Flow USD Mil` / `Shares Mil`) %>%
            dplyr::select(year,
                          `Earnings Per Share USD`,
                          `Revenue Per Share USD`,
                          `Book Value Per Share * USD`,
                          `Cash Flow Per Share USD`)

        # Merge and calculate valuations
        valuation <- dplyr::left_join(valuations_1, valuations_2, by = "year") %>%
            dplyr::mutate(`Price to Earnings`  = adjusted / `Earnings Per Share USD`,
                          `Price to Sales`     = adjusted / `Revenue Per Share USD`,
                          `Price to Book`      = adjusted / `Book Value Per Share * USD`,
                          `Price to Cash Flow` = adjusted / `Cash Flow Per Share USD`) %>%
            dplyr::select(date,
                          `Price to Earnings`,
                          `Price to Sales`,
                          `Price to Book`,
                          `Price to Cash Flow`) %>%
            tidyr::gather(key = category, value = value, -date) %>%
            dplyr::select(category, date, value) %>%
            dplyr::mutate(date = lubridate::ymd(date))

        # Get last group number
        last_group_num <- key_ratios_bind$group %>% max()

        # Create valuation tibble and bind_rows
        valuation_bind <- dplyr::bind_cols(
            tibble::tibble(section = rep("Valuation Ratios", nrow(valuation))),
            tibble::tibble(sub.section = rep("Valuation Ratios", nrow(valuation))),
            tibble::tibble(group = rep(seq(last_group_num + 1, length.out = 4), each = 10)),
            valuation)

        key_ratios <- dplyr::bind_rows(key_ratios_bind, valuation_bind) %>%
            dplyr::group_by(section) %>%
            tidyr::nest()

        return(key_ratios)

    }, error = function(e) {

        warn <- paste0("Error at ", x, " during call to get = key.ratios")
        warning(warn)
        return(NA) # Return NA on error

    })

}

# Util 3: key.stats -----
tq_get_util_3 <- function(x, get, ...) {

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
        url_base_1 <- 'http://download.finance.yahoo.com/d/quotes.csv?s='
        url_base_2 <- '&f='
        url_base_3 <- '&e=.csv'
        yahoo_tag_list <- stringr::str_c(yahoo_tags$yahoo.tag, collapse = "")
        url <- paste0(url_base_1, x, url_base_2, yahoo_tag_list, url_base_3)

        # Try various stock exchanges
        download.file(url, destfile = tmp, quiet = TRUE)

        # Read data
        key_stats_raw <- suppressMessages(
            suppressWarnings(
                readr::read_csv(tmp, col_names = FALSE, na = c("", "NA", "N/A", "<NA>"))
            )
        )

        # Unlink tmp
        unlink(tmp)

        # Format tidy data frame ----

        # Names
        key_stat_names <- yahoo_tags$yahoo.tag.desc %>%
            make.names()
        names(key_stats_raw) <- key_stat_names

        # Formatting Functions
        convert_to_numeric <- function(x) {
            # x = character such as "23.4B"
            units <- stringr::str_sub(x, -1, -1)

            if (units %in% c("T", "B", "M")) {
                value <- stringr::str_sub(x, 1, -2) %>%
                    as.numeric()

                if (units == "T") {
                    value <- value * 1e12
                } else if (units == "B") {
                    value <- value * 1e9
                } else if (units == "M") {
                    value <- value * 1e6
                }
            } else (
                value <- as.numeric(value)
            )

            value
        }

        convert_to_percent <- function(x) {
            # x = character such as "-0.6104%"
            units <- stringr::str_sub(x, -1, -1)

            if (units %in% c("%")) {
                value <- stringr::str_sub(x, 1, -2) %>%
                    as.numeric()

                value <- value * 1 / 100
            }

            value
        }

        # Main formatting script
        suppressWarnings(
            key_stats <- key_stats_raw %>%
                dplyr::mutate(Last.Trade.Date = lubridate::mdy(Last.Trade.Date),
                              Market.Capitalization = convert_to_numeric(Market.Capitalization),
                              EBITDA = convert_to_numeric(EBITDA),
                              Percent.Change.From.52.week.Low = convert_to_percent(Percent.Change.From.52.week.Low),
                              Percent.Change.From.50.day.Moving.Average = convert_to_percent(Percent.Change.From.50.day.Moving.Average),
                              Percent.Change.From.200.day.Moving.Average = convert_to_percent(Percent.Change.From.200.day.Moving.Average),
                              Change.in.Percent = convert_to_percent(Change.in.Percent),
                              Ex.Dividend.Date = lubridate::mdy(Ex.Dividend.Date),
                              Dividend.Pay.Date = lubridate::mdy(Dividend.Pay.Date),
                              Revenue = convert_to_numeric(Revenue)
                )
        )

        # Sort by column name
        key_stats_sorted <- key_stats[, order(names(key_stats))]

        return(key_stats_sorted)

    }, error = function(e) {

        warn <- paste0("Error at ", x, " during call to get = key.stats")
        warning(warn)
        return(NA) # Return NA on error

    })

}
