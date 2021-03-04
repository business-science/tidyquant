#' Get quantitative data in `tibble` format
#'
#' @param x A single character string, a character vector or tibble representing a single (or multiple)
#' stock symbol, metal symbol, currency combination, FRED code, etc.
#' @param get A character string representing the type of data to get
#' for `x`. Options include:
#' \itemize{
#'   \item `"stock.prices"`: Get the open, high, low, close, volume and adjusted
#'   stock prices for a stock symbol from
#'   \href{https://finance.yahoo.com/}{Yahoo Finance}. Wrapper for `quantmod::getSymbols()`.
#'   \item `"dividends"`: Get the dividends for a stock symbol from
#'   \href{https://finance.yahoo.com/}{Yahoo Finance}. Wrapper for `quantmod::getDividends()`.
#'   \item `"splits"`: Get the split ratio for a stock symbol from
#'   \href{https://finance.yahoo.com/}{Yahoo Finance}. Wrapper for `quantmod::getSplits()`.
#'   \item `"stock.prices.japan"`: Get the open, high, low, close, volume and adjusted
#'   stock prices for a stock symbol from
#'   \href{https://finance.yahoo.co.jp/}{Yahoo Finance Japan}. Wrapper for `quantmod::getSymbols.yahooj()`.
#'   \item `"economic.data"`: Get economic data from
#'   \href{https://fred.stlouisfed.org/}{FRED}. rapper for `quantmod::getSymbols.FRED()`.
#'   \item `"quandl"`: Get data sets from
#'   \href{https://www.quandl.com/}{Quandl}. Wrapper for `Quandl::Quandl()`.
#'   See also [quandl_api_key()].
#'   \item `"quandl.datatable"`: Get data tables from
#'   \href{https://www.quandl.com/}{Quandl}. Wrapper for `Quandl::Quandl.datatable()`.
#'   See also [quandl_api_key()].
#'   \item `"tiingo"`: Get data sets from
#'   \href{https://api.tiingo.com/}{Tiingo}. Wrapper for `riingo::riingo_prices()`.
#'   See also [tiingo_api_key()].
#'   \item `"tiingo.iex"`: Get data sets from
#'   \href{https://api.tiingo.com/}{Tiingo}. Wrapper for `riingo::riingo_iex_prices()`.
#'   See also [tiingo_api_key()].
#'   \item `"tiingo.crypto"`: Get data sets from
#'   \href{https://api.tiingo.com/}{Tiingo}. Wrapper for `riingo::riingo_crypto_prices()`.
#'   See also [tiingo_api_key()].
#'   \item `"alphavantager"`: Get data sets from
#'   \href{https://www.alphavantage.co/}{Alpha Vantage}. Wrapper for `alphavantager::av_get()`.
#'   See also [av_api_key()].
#'   \item `"rblpapi"`: Get data sets from
#'   \href{https://www.bloomberg.com/professional/solution/bloomberg-terminal/}{Bloomberg}. Wrapper for `Rblpapi`.
#'   See also [Rblpapi::blpConnect()] to connect to Bloomberg terminal (required).
#'   Use the argument `rblpapi_fun` to set the function such as "bdh" (default), "bds", or "bdp".
#' }
#' @param complete_cases Removes symbols that return an NA value due to an error with the get
#' call such as sending an incorrect symbol "XYZ" to get = "stock.prices". This is useful in
#' scaling so user does not need to
#' add an extra step to remove these rows. `TRUE` by default, and a warning
#' message is generated for any rows removed.
#' @param ... Additional parameters passed to the "wrapped"
#' function. Investigate underlying functions to see full list of arguments.
#' Common optional parameters include:
#' \itemize{
#'   \item `from`: Standardized for time series functions in `quantmod`, `quandl`, `tiingo`, `alphavantager` packages.
#'   A character string representing a start date in
#'   YYYY-MM-DD format.
#'   \item `to`: Standardized for time series functions in `quantmod`, `quandl`, `tiingo`, `alphavantager` packages.
#'   A character string representing a end date in
#'   YYYY-MM-DD format.
#' }
#'
#'
#' @return Returns data in the form of a `tibble` object.
#'
#' @details
#' `tq_get()` is a consolidated function that gets data from various
#' web sources. The function is a wrapper for several `quantmod`
#' functions, `Quandl` functions, and also gets data from websources unavailable
#' in other packages.
#' The results are always returned as a `tibble`. The advantages
#' are (1) only one function is needed for all data sources and (2) the function
#' can be seemlessly used with the tidyverse: `purrr`, `tidyr`, and
#' `dplyr` verbs.
#'
#' `tq_get_options()` returns a list of valid `get` options you can
#' choose from.
#'
#' `tq_get_stock_index_options()` Is deprecated and will be removed in the
#' next version. Please use `tq_index_options()` instead.
#'
#' @seealso
#' \itemize{
#'   \item [tq_index()] to get a ful list of stocks in an index.
#'   \item [tq_exchange()] to get a ful list of stocks in an exchange.
#'   \item [quandl_api_key()] to set the api key for collecting data via the `"quandl"`
#'   get option.
#'   \item [tiingo_api_key()] to set the api key for collecting data via the `"tiingo"`
#'   get option.
#'   \item [av_api_key()] to set the api key for collecting data via the `"alphavantage"`
#'   get option.
#' }
#'
#'
#' @rdname tq_get
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#' library(tidyverse)
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
#'
#'
#' \dontrun{
#'
#' # --- Quandl ---
#'
#' quandl_api_key('<your_api_key>')
#'
#' # Energy data from EIA
#' tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01")
#'
#'
#' # --- Tiingo ---
#'
#' tiingo_api_key('<your_api_key>')
#'
#' # Tiingo Prices (Free alternative to Yahoo Finance!)
#' tq_get(c("AAPL", "GOOG"), get = "tiingo", from = "2010-01-01")
#'
#' # Sub-daily prices from IEX ----
#' tq_get(c("AAPL", "GOOG"),
#'        get = "tiingo.iex",
#'        from   = "2020-01-01",
#'        to     = "2020-01-15",
#'        resample_frequency = "5min")
#'
#' # Tiingo Bitcoin Prices ----
#' tq_get(c("btcusd", "btceur"),
#'        get    = "tiingo.crypto",
#'        from   = "2020-01-01",
#'        to     = "2020-01-15",
#'        resample_frequency = "5min")
#'
#'
#' # --- Alpha Vantage ---
#'
#' av_api_key('<your_api_key>')
#'
#' # Daily Time Series
#' tq_get("AAPL",
#'        get        = "alphavantager",
#'        av_fun     = "TIME_SERIES_DAILY_ADJUSTED",
#'        outputsize = "full")
#'
#' # Intraday 15 Min Interval
#' tq_get("AAPL",
#'        get        = "alphavantage",
#'        av_fun     = "TIME_SERIES_INTRADAY",
#'        interval   = "15min",
#'        outputsize = "full")
#'
#' # FX DAILY
#' tq_get("USD/EUR", get = "alphavantage", av_fun = "FX_DAILY", outputsize = "full")
#'
#' # FX REAL-TIME QUOTE
#' tq_get("USD/EUR", get = "alphavantage", av_fun = "CURRENCY_EXCHANGE_RATE")
#'
#' }



# PRIMARY FUNCTIONS ----

tq_get <- function(x, get = "stock.prices", complete_cases = TRUE, ...) {

    # Validate compound gets
    # if (length(get) > 1) validate_compound_gets(get)
    if (length(get) > 1) stop(call. = FALSE, "tq_get(): Please use only one value for `get` source.")

    # Validate quandl api key
    if("quandl" %in% get) {
        if (is.null(quandl_api_key())) warning("No Quandl API key detected. Limited to 50 anonymous calls per day. Set key with 'quandl_api_key()'.",
                                               call. = FALSE)
    }

    # Validate Alpha Vantage api key
    if(stringr::str_detect("alphavantage", get)) {
        if (is.null(tidyquant::av_api_key())) stop("No Alpha Vantager API key detected. Set key with 'av_api_key()'.",
                                               call. = FALSE)
    }

    # Validate Alpha Vantage api key
    if(stringr::str_detect("tiingo", get)) {
        if (is.null(riingo::riingo_get_token())) stop("No Tiingo API key detected. Set key with 'tiingo_api_key()'.",
                                                   call. = FALSE)
    }

    # Setup Rblpapi
    if("Rblpapi" %in% get) {
        if(!requireNamespace("Rblpapi", quietly = TRUE)) {
            stop("Rblpapi must be installed to get data from Bloomberg.", call. = FALSE)
        }
        Rblpapi::blpConnect() #must have a valid blp session running
    }

    # Discontinue key.stats
    if("key.stats" %in% get) stop("Yahoo Key Statistics has been discontinued.", call. = FALSE)
    # if("dividends" %in% get) stop("Yahoo Dividends has been discontinued.", call. = FALSE)
    # if("splits" %in% get) stop("Yahoo Splits has been discontinued.", call. = FALSE)
    if("stock.prices.google" %in% get) stop("Google Finance has been discontinued.", call. = FALSE)

    # API SPECIAL CASES
    # TIINGO - Minimize API calls
    if (stringr::str_detect(get, "tiingo")) {
        ret <- tq_get_base(x, get, complete_cases = complete_cases, map = FALSE, ...)
        return(ret)
    }

    # Distribute operations based on x
    if (is.character(x) && length(x) == 1 && length(get) == 1) {

        # Expedite get
        ret <- tq_get_base(x, get, complete_cases = complete_cases, map = FALSE, ...)

        if (tibble::is_tibble(ret)) ret <- ret %>% tibble::add_column(symbol = x, .before = 1)

    } else if (is.character(x)) {

        col_name <- names(x)

        if (is.null(col_name)) col_name <- "symbol"

        x_tib <- tibble::tibble(symbol.. = x)

        ret <- tq_get_map(x = x_tib, get = get, complete_cases = complete_cases, ...)

        names(ret)[[1]] <- col_name[[1]]

    } else if (inherits(x, "data.frame")) {

        # Prevent issues with grouped_df's
        if (inherits(x, "grouped_df")) {
            warning("Ungrouping grouped data frame")
            x <- dplyr::ungroup(x)
        }

        col_name <- colnames(x)[[1]]

        names(x)[[1]] <- "symbol.."

        x_tib <- x %>%
            tibble::as_tibble()

        ret <- tq_get_map(x = x_tib, get = get, complete_cases = complete_cases, ...)

        names(ret)[[1]] <- col_name[[1]]

    } else {

        stop("x must be a single character, list of characters, or data frame of characters with the first column being the object to pass to tq_get.")

    }

    # Unnest if only 1 get option
    if (length(get) == 1 && (length(x) > 1 || is.data.frame(x))) {
        ret <- tryCatch({
            ret %>%
                tidyr::unnest(cols = dplyr::one_of(get))
        }, error = function(e) {
            warning("Returning as nested data frame.")
            ret
        })

    }

    return(ret)

}

tq_get_map <- function(x, get, complete_cases, ...) {

    ret <- x

    # Loop through each get option, mapping tq_get_base
    for (i in seq_along(get)) {

        if (complete_cases) {

            ret <- ret %>%
                dplyr::mutate(
                    data.. = purrr::map(
                        .x             = symbol..,
                        .f             = tq_get_base,
                        get            = get[[i]],
                        complete_cases = complete_cases,
                        map            = TRUE,
                        ...),
                    class.. = purrr::map_chr(
                        .x = data..,
                        .f = function(x) class(x)[[1]]
                        )
                    ) %>%
                dplyr::filter(class.. != "logical") %>%
                dplyr::select(-class..)

        } else {

            ret <- ret %>%
                dplyr::mutate(
                    data.. = purrr::map(
                        .x             = symbol..,
                        .f             = tq_get_base,
                        get            = get[[i]],
                        complete_cases = complete_cases,
                        map            = TRUE,
                        ...
                    )
                )

        }

        colnames(ret)[length(colnames(ret))] <- get[[i]]

    }

    ret

}

tq_get_base <- function(x, get, ...) {

    # Clean get
    get <- clean_get(get)

    # Validate get
    validate_get(get)

    # Setup switches based on get
    ret <- switch(get,
                  stockprice        = tq_get_util_1(x, get, ...),
                  # stockpricesgoogle = tq_get_util_1(x, get, ...),
                  stockpricesjapan  = tq_get_util_1(x, get, ...),
                  dividend          = tq_get_util_1(x, get, ...),
                  split             = tq_get_util_1(x, get, ...),
                  # financial         = tq_get_util_1(x, get, ...),
                  # keyratio          = tq_get_util_2(x, get, ...),
                  # metalprice        = tq_get_util_1(x, get, ...),
                  # exchangerate      = tq_get_util_1(x, get, ...),
                  economicdata      = tq_get_util_1(x, get, ...),
                  # stockindex        = tq_index(x), # Deprecated, remove next version
                  quandl            = tq_get_util_4(x, get, ...),
                  quandldatatable   = tq_get_util_5(x, get, ...),
                  alphavantage      = tq_get_util_6(x, get, ...),
                  alphavantager     = tq_get_util_6(x, get, ...),
                  tiingo            = tq_get_util_7(x, get, ...),
                  tiingoiex         = tq_get_util_7(x, get, ...),
                  tiingocrypto      = tq_get_util_7(x, get, ...),
                  rblpapi           = tq_get_rblpapi(x, get, ...)
                  )

    ret

}

#' @rdname tq_get
#' @export
tq_get_options <- function() {
    c("stock.prices",
      # "stock.prices.google",
      "stock.prices.japan",
      # "financials",
      # "key.ratios",
      "dividends",
      "splits",
      "economic.data",
      # "exchange.rates",
      # "metal.prices",
      "quandl",
      "quandl.datatable",
      "tiingo",
      "tiingo.iex",
      "tiingo.crypto",
      "alphavantager",
      "alphavantage",
      "rblpapi"
      )
}




# UTILITY FUNCTIONS ----

# Util 1: stock.prices, financials, economic.data -----
tq_get_util_1 <-
    function(x,
             get,
             complete_cases,
             map,
             from = as.character(paste0(lubridate::year(lubridate::today()) - 10, "-01-01")),
             to   = as.character(lubridate::today()),
             ...) {

    # google.stock.prices is now defunct (google no longer provides data)
    if(get == "stockpricesgoogle") {
      warning("Google Finance stopped providing data in March, 2018. \n",
              "Use get = 'stock.prices' instead to pull from Yahoo Finance.", call. = FALSE)
      return(NA)
    }

    # Google Financials is now defunct (google no longer provides data)
    if(get == "financial") {
      warning("Google Finance stopped providing data in March, 2018. \n",
              "We are currently looking for alternative financial data sources.", call. = FALSE)
      return(NA)
    }

    # Check x
    if (!is.character(x)) {
        stop("x must be a character string in the form of a valid symbol.")
    }

    # Handle 180 day Oanda limit
    if(get %in% c("exchangerate", "metalprice")) {  # If pulling Oanda
        if(from < Sys.Date() - lubridate::days(180)) {     # And some dates are past limit

            warning(paste0("Oanda only provides historical data for the past 180 days. Symbol: ", x),
                    call. = FALSE)

            # If completely outside range, stop. Otherwise there is some data to pull so continue
            if(to < Sys.Date() - lubridate::days(180)) {
                return(NA)
            }
        }
    }

    # Setup switches based on get
    vars <- switch(get,
                   stockprice            = list(chr_get    = "stock.prices",
                                                fun        = quantmod::getSymbols,
                                                chr_fun    = "quantmod::getSymbols",
                                                list_names = c("open", "high", "low", "close", "volume", "adjusted"),
                                                source     = "yahoo"),
                   # stockpricesgoogle     = list(chr_get    = "stock.prices",
                   #                              fun        = quantmod::getSymbols,
                   #                              chr_fun    = "quantmod::getSymbols.google",
                   #                              list_names = c("open", "high", "low", "close", "volume"),
                   #                              source     = "google"),
                   stockpricesjapan      = list(chr_get    = "stock.prices",
                                                fun        = quantmod::getSymbols,
                                                chr_fun    = "quantmod::getSymbols.yahooj",
                                                list_names = c("open", "high", "low", "close", "volume", "adjusted"),
                                                source     = "yahooj"),
                   dividend              = list(chr_get    = "dividends",
                                                fun        = quantmod::getDividends,
                                                chr_fun    = "quantmod::getDividends",
                                                list_names = "value",
                                                source     = "yahoo"),
                   split                 = list(chr_get    = "splits",
                                                fun        = quantmod::getSplits,
                                                chr_fun    = "quantmod::getSplits",
                                                list_names = "value",
                                                source     = "yahoo"),
                   # financial             = list(chr_get    = "financials",
                   #                              fun        = quantmod::getFinancials,
                   #                              chr_fun    = "quantmod::getFinancials",
                   #                              source     = "google"),
                   metalprice            = list(chr_get    = "metal.prices",
                                                fun        = quantmod::getMetals,
                                                chr_fun    = "quantmod::getMetals",
                                                list_names = "price",
                                       source     = "oanda"),
                   exchangerate          = list(chr_get    = "exchange.rates",
                                                fun        = quantmod::getFX,
                                                chr_fun    = "quantmod::getFX",
                                                list_names = "exchange.rate",
                                       source     = "oanda"),
                   economicdata          = list(chr_get    = "economic.data",
                                                fun        = quantmod::getSymbols,
                                                chr_fun    = "quantmod::getSymbols.FRED",
                                                list_names = "price",
                                                source     = "FRED")
    )

    # Get data; Handle errors
    ret <- tryCatch({

        suppressWarnings(
            suppressMessages(
                vars$fun(x, src = vars$source, auto.assign = FALSE, from = from, to = to, ...)
            )
        )

    }, error = function(e) {

        warn <- paste0("x = '", x, "', get = '", vars$chr_get, "': ", e)
        if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
        warning(warn, call. = FALSE)
        return(NA) # Return NA on error

    })

    # coerce financials to tibble
    if (identical(get, "financial") && class(ret) == "financials") {

        # Tidy a single financial statement
        tidy_fin <- function(x) {

            group <- 1:nrow(x)

            df <- dplyr::bind_cols(
                    tibble::tibble(group),
                    timetk::tk_tbl(x, preserve_index = TRUE, rename_index = "category", silent = TRUE)
                    ) %>%
                tidyr::gather(date, value, -c(category, group)) %>%
                dplyr::mutate(date = lubridate::as_date(date)) %>%
                dplyr::arrange(group)

            df

        }

        # Setup tibble and map tidy_fin function

        ret <- tibble::tibble(
            type = c("IS", "IS", "BS", "BS", "CF", "CF"),
            period = rep(c("A", "Q"), 3)) %>%
            dplyr::mutate(retrieve = paste0("ret$", type, "$", period)) %>%
            dplyr::mutate(data = purrr::map(.x = retrieve, .f = function(x) eval(parse(text = x)))) %>%
            dplyr::mutate(data = purrr::map(.x = data, .f = tidy_fin)) %>%
            dplyr::select(-retrieve) %>%
            tidyr::spread(key = period, value = data) %>%
            dplyr::rename(annual = A, quarter = Q)

    }

    # Coerce any xts to tibble
    if (xts::is.xts(ret)) {
        dimnames(ret)[[2]] <- vars$list_names
        ret <- ret %>%
            timetk::tk_tbl(preserve_index = TRUE, rename_index = "date", silent = TRUE) %>%
            dplyr::mutate(date = lubridate::as_date(date))

        # Filter economic data by date
        if (identical(get, "economicdata")) {
            ret <- ret %>%
                dplyr::filter(date >= lubridate::as_date(from) & date <= lubridate::as_date(to))
        }
    }

    ret

}





# Util 4: Quandl -----
tq_get_util_4 <- function(x, get, type = "raw",  meta = FALSE, order = "asc", complete_cases, map, ...) {

    # Check x
    if (!is.character(x)) {
        stop("x must be a character string in the form of a valid symbol.")
    }

    # Convert x to uppercase
    x <- stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both")

    # Check type
    if (type != "raw") {
        type = "raw"
        warning("tidyquant only supports the 'raw' return type. Returning 'raw' data.", call. = FALSE)
    }

    # Check meta
    if (meta == TRUE) {
        meta = FALSE
        warning("tidyquant does not support Quandl meta data. Setting `meta == FALSE`.", call. = FALSE)
    }

    # Check order
    if (order == "desc") {
        order = "asc"
        warning("For consistency, tidyquant does not return descending data. Returning ascending.", call. = FALSE)
    }

    # Repurpose from and to as start_date and end_date
    args <- list(code  = x,
                 type  = type,
                 meta  = meta,
                 order = order)
    args <- append(args, list(...))
    if (!is.null(args$from)) args$start_date <- args$from
    if (!is.null(args$to)) args$end_date <- args$to

    tryCatch({

      ret <- do.call("Quandl", args) %>% tibble::as_tibble()

    }, error = function(e) {

        warn <- paste0("x = '", x, "', get = 'quandl", "': ", e)
        if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
        warning(warn, call. = FALSE)
        return(NA) # Return NA on error

    })

    # Clean quandl column names to make easier
    if (!is.null(colnames(ret))) {
        colnames(ret) <- make.names(colnames(ret)) %>%
            stringr::str_replace_all(pattern = "\\.+", ".") %>%
            stringr::str_to_lower()
    }

    return(ret)

}

# Util 5: Quandl.datatable -----
tq_get_util_5 <- function(x, get, paginate = FALSE, complete_cases, map, ...) {

    # Check x
    if (!is.character(x)) {
        stop("x must be a character string in the form of a valid symbol.")
    }

    # Convert x to uppercase
    x <- stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both")

    ret <- tryCatch({

        Quandl.datatable(code = x, paginate = paginate, ...) %>%
            tibble::as_tibble()


    }, error = function(e) {

        warn <- paste0("x = '", x, "', get = 'quandl.datatable", "': ", e)
        if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
        warning(warn, call. = FALSE)
        return(NA) # Return NA on error

    })

    return(ret)

}

# Util 6: alphavantager -----
tq_get_util_6 <- function(x, get, av_fun, complete_cases, map, ...) {

    if(!requireNamespace("alphavantager", quietly = TRUE)) {
      stop("alphavantager must be installed to use this functionality.", call. = FALSE)
    }

    # Check x
    if (!is.character(x)) {
        stop("x must be a character string in the form of a valid symbol.")
    }

    # Check av_fun exists
    # dots <- list(...)
    # if (is.null(dots$av_fun)) {
    #     stop("`av_fun` argument must be present. See Alpha Van")
    # }

    # Convert x to uppercase
    x <- stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both")

    # Get av_fun
    # av_fun <- dots$av_fun
    # other_dots <- dots
    # other_dots$av_fun <- NULL

    ret <- tryCatch({

        alphavantager::av_get(symbol = x, av_fun = av_fun, ...)


    }, error = function(e) {

        warn <- paste0("x = '", x, "', get = 'alphavantager", "': ", e)
        if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
        warning(warn, call. = FALSE)
        return(NA) # Return NA on error

    })

    return(ret)

}

# Util : tiingo -----
tq_get_util_7 <- function(x, get, tiingo_fun, complete_cases, map, ...) {

  if(!requireNamespace("riingo", quietly = TRUE)) {
    stop("riingo must be installed to use this functionality.", call. = FALSE)
  }

  # Check x
  if (!is.character(x)) {
    stop("x must be a character string in the form of a valid symbol.")
  }

  .fun <- switch(get,
                 tiingo            = riingo::riingo_prices,
                 tiingoiex         = riingo::riingo_iex_prices,
                 tiingocrypto      = riingo::riingo_crypto_prices
  )

  # Convert x to uppercase
  x <- stringr::str_to_upper(x) %>% stringr::str_trim(side = "both")

  # Set from/to args to riingo params
  args <- list(ticker = x)
  args <- append(args, list(...))
  if (!is.null(args$from)) {
    args$start_date <- args$from
    args$from       <- NULL
  }
  if (!is.null(args$to)) {
    args$end_date <- args$to
    args$to       <- NULL
  }

  tryCatch({

    ret <- do.call(.fun, args)

    # Make columns consistent with Yahoo Finance!
    if (get == "tiingo") {
      ret <- ret %>%
        dplyr::select(ticker, date, open, high, low, close, volume, adjClose, dplyr::everything()) %>%
        dplyr::rename(adjusted = adjClose,
               symbol   = ticker)
    }

    if (get == "tiingoiex") {
      ret <- ret %>%
        dplyr::select(ticker, date, open, high, low, close, dplyr::everything()) %>%
        dplyr::rename(symbol = ticker)
    }

    if (get == "tiingocrypto") {
      ret <- ret %>%
        dplyr::rename(symbol = ticker)
    }

  }, error = function(e) {

    warn <- paste0("x = '", x, "', get = '", get, "': ", e)
    if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
    warning(warn, call. = FALSE)
    return(NA) # Return NA on error

  })

  return(ret)

}


# Util: rblpapi -----
tq_get_rblpapi <- function(x, get, rblpapi_fun = "bdh", complete_cases, map, ...) {

    # Check x
    if (!is.character(x)) {
        stop("x must be a character string in the form of a valid symbol.")
    }

    ret <- tryCatch({

        # Rblpapi argument setup
        if (rblpapi_fun == "bds") {
            args <- list(security = x)
        } else {
            args <- list(securities = x)
        }
        args <- append(args, list(...))

        # Repurpose from and to as start_date and end_date
        if (!is.null(args$from)) {
            args$start.date <- as.Date(args$from)
            args$from <- NULL
            }
        if (!is.null(args$to)) {
            args$end.date   <- as.Date(args$to)
            args$to <- NULL
        }

        # Call function
        do.call(getExportedValue("Rblpapi", rblpapi_fun), args) %>%
            tibble::as.tibble()

    }, error = function(e) {

        warn <- paste0("x = '", x, "', get = 'Rblpapi", "': ", e)
        if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
        warning(warn, call. = FALSE)
        return(NA) # Return NA on error

    })

    return(ret)

}

# Clean Get ----
clean_get <- function(get) {
    stringr::str_to_lower(get) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "") %>%
        stringr::str_replace_all("s$", "")
}

# Validate Gets -----
validate_get <- function(get) {

    get_options <- tq_get_options() %>%
        stringr::str_replace_all("[[:punct:]]", "") %>%
        stringr::str_replace_all("s$", "")

    # # Deprecated, remove "stockindex" in next version
    # if (!all(get %in% c(get_options, "stockindex"))) {
    #     stop("Get must be a valid entry. Use tq_get_options() to see valid options.")
    # }

    return(get)
}



# ---- DEPRECATED ----

# tq_get_stock_index_options ----
# tq_get_stock_index_options <- function() {
#   warning("tq_get_stock_index_options() is deprecated and will be removed in the next version. Please use tq_index_options().")
#   tq_index_options()
# }

# validate_compound_gets ----
# validate_compound_gets <- function(get) {
#
#     get <- stringr::str_to_lower(get) %>%
#         stringr::str_trim(side = "both") %>%
#         stringr::str_replace_all("[[:punct:]]", "") %>%
#         stringr::str_replace_all("s$", "")
#
#     # Only allowed to use first six options for compound gets because these use traditional stock symbols
#     # Update for "stock.prices.japan". Now subset is c(1, 3:7)
#     compound_get_options <- tq_get_options()[c(1, 3:7)] %>%
#         stringr::str_replace_all("[[:punct:]]", "") %>%
#         stringr::str_replace_all("s$", "")
#
#     if (!all(get %in% compound_get_options)) {
#         stop("Get options for compound get are not valid.")
#     }
# }

# Util 2: key.ratios [DEPRECATED] -----
# tq_get_util_2 <- function(x, get, complete_cases, map, ...) {
#
#   # # Morningstart Key Ratios is now defunct (breaking change to website)
#   # warning("Morningstar made a breaking change to the Key Ratios \n",
#   #         "We are currently looking for alternative financial data sources.", call. = FALSE)
#   # return(NA)
#
#
#   # Check x
#   if (!is.character(x)) {
#       stop("x must be a character string in the form of a valid symbol.")
#   }
#
#   # Convert x to uppercase
#   x <- stringr::str_to_upper(x) %>%
#       stringr::str_trim(side = "both")
#
#   # If the request has a ':', assume that it is in the form of EXCHANGE:SYMBOL
#   # Allows both forcing a specific exchange source and making requests from non-default exchanges
#   if ( stringr::str_detect(x,":") ) {
#       split_req <- stringr::str_split(x,":",2)
#       stock_exchange <- c(split_req[[1]][1])
#       x <- split_req[[1]][2]
#   } else {
#       stock_exchange <- c("XNAS", "XNYS", "XASE") # mornginstar gets from various exchanges
#   }
#
#   x <- stringr::str_replace_all(x,"[[:punct:]]", "")
#
#   tryCatch({
#
#       # Download file
#       url_base_1 <- 'http://financials.morningstar.com/finan/ajax/exportKR2CSV.html?&callback=?&t='
#       url_base_2 <- '&region=usa&culture=en-US&cur=&order=asc'
#       # Three URLs to try
#       url <- paste0(url_base_1, stock_exchange, ":", x, url_base_2)
#
#       # Try various stock exchanges
#       for(i in 1:length(url)) {
#           text <- httr::RETRY("GET", url[i], times = 5) %>%
#               httr::content(type = "text/html", encoding = "UTF-8")
#
#           if(!is.null(text)) {
#
#               # Test to see if file returned is just a message containing "We're sorry"
#               text_test <- text %>%
#                   xml2::as_list() %>%
#                   unlist() %>%
#                   stringr::str_detect("^We.re sorry")
#
#               # If text does not contain "We're sorry" message, break
#               if (!text_test) {
#                   break
#               }
#           }
#       }
#
#       # Read lines
#       text <- text %>%
#           xml2::as_list() %>%
#           unlist() %>%
#           strsplit(split = "\n") %>%
#           purrr::flatten_chr()
#
#       # Skip rows & setup key ratio categories
#
#       # Patch for stocks with only 110 lines, missing Free Cash Flow/Net Income (line 71)
#       if (length(text) == 111)  {
#           # 111 Lines is normal
#           skip_rows <- c(1:2, 19:21, 31:32, 41:44, 49, 54, 59, 64:66, 72:74, 95:96, 101:103)
#
#           key_ratios_1 <- tibble::tibble(
#               section            = c(rep("Financials", 15),
#                                      rep("Profitability", 17),
#                                      rep("Growth", 16),
#                                      rep("Cash Flow", 5),
#                                      rep("Financial Health", 24),
#                                      rep("Efficiency Ratios", 8)),
#               sub.section        = c(rep("Financials", 15),
#                                      rep("Margin of Sales %", 9),
#                                      rep("Profitability", 8),
#                                      rep("Revenue %", 4),
#                                      rep("Operating Income %", 4),
#                                      rep("Net Income %", 4),
#                                      rep("EPS %", 4),
#                                      rep("Cash Flow Ratios", 5),
#                                      rep("Balance Sheet Items (in %)", 20),
#                                      rep("Liquidty/Financial Health", 4),
#                                      rep("Efficiency", 8)),
#               group               = 1:85
#           )
#       } else {
#           # Patch for stocks with 110 lines
#           skip_rows <- c(1:2, 19:21, 31:32, 41:44, 49, 54, 59, 64:66, 71:73, 94:95, 100:102)
#
#           key_ratios_1 <- tibble::tibble(
#               section            = c(rep("Financials", 15),
#                                      rep("Profitability", 17),
#                                      rep("Growth", 16),
#                                      rep("Cash Flow", 4), # One less
#                                      rep("Financial Health", 24),
#                                      rep("Efficiency Ratios", 8)),
#               sub.section        = c(rep("Financials", 15),
#                                      rep("Margin of Sales %", 9),
#                                      rep("Profitability", 8),
#                                      rep("Revenue %", 4),
#                                      rep("Operating Income %", 4),
#                                      rep("Net Income %", 4),
#                                      rep("EPS %", 4),
#                                      rep("Cash Flow Ratios", 4), # One less
#                                      rep("Balance Sheet Items (in %)", 20),
#                                      rep("Liquidty/Financial Health", 4),
#                                      rep("Efficiency", 8)),
#               group              = c(1:52, 54:85)
#           )
#       }
#
#       text <- text[-skip_rows]
#
#       # Parse text
#       key_ratios_2 <-
#           suppressMessages(
#               suppressWarnings(
#                   utils::read.csv(text = text, na.strings=c("", "NA")) %>%
#                       tibble::as_tibble() %>%
#                       dplyr::mutate_all(as.character)
#               )
#           )
#
#
#       # Combine tibble parts into raw data
#       key_ratios_raw <- dplyr::bind_cols(key_ratios_1, key_ratios_2)
#
#       # Cleanup raw data
#       key_ratios_bind <- key_ratios_raw %>%
#           dplyr::select(-TTM) %>%
#           dplyr::rename(category = X) %>%
#           dplyr::mutate(group = as.numeric(group)) %>%
#           tidyr::gather(key = date, value = value, -c(group, section, sub.section, category)) %>%
#           dplyr::arrange(group) %>%
#           dplyr::mutate(value = ifelse(value == "false", NA, value)) %>%
#           dplyr::mutate(date = stringr::str_sub(date, start = 2, end = length(date))) %>%
#           dplyr::mutate(date = stringr::str_replace(date, "\\.", "-")) %>%
#           dplyr::mutate(date = lubridate::ymd(date, truncated = 2)) %>%
#           dplyr::mutate(value = stringr::str_replace(value, ",", "")) %>%
#           dplyr::mutate(value = as.double(value)) %>%
#           dplyr::select(section, sub.section, group, category, date, value)
#
#       key_ratios <- key_ratios_bind %>%
#           dplyr::group_by(section) %>%
#           tidyr::nest()
#
#   }, warning = function(w) {
#
#       warn <- w
#       if (map == TRUE) warn <- paste0(x, ": ", w)
#       warning(warn, call. = FALSE)
#       return(key_ratios)
#
#   }, error = function(e) {
#
#       warn <- paste0("x = '", x, "', get = 'key.ratios", "': ", e)
#       if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
#       warning(warn, call. = FALSE)
#       return(NA) # Return NA on error
#
#   })
#
#
#   # Calculate valuations
#
#   tryCatch({
#
#       # Get stock prices
#       from = lubridate::today() - lubridate::years(12)
#       valuations_2 <- tq_get(x, get = "stock.prices", from = from) %>%
#           tq_transmute_xy(adjusted, mutate_fun = to.period, period = "years") %>%
#           dplyr::mutate(year = lubridate::year(date)) %>%
#           dplyr::select(year, date, adjusted)
#
#       # Get categories
#       categories <- key_ratios_bind %>%
#           dplyr::filter(section == "Financials") %>%
#           dplyr::pull(category) %>%
#           unique()
#
#       revenue_name      <- categories[[1]]
#       shares_name       <- categories[[9]]
#       eps_name          <- categories[[6]]
#       book_val_name     <- categories[[10]]
#       op_cash_flow_name <- categories[[11]]
#
#       revenue_expr      <- rlang::sym(revenue_name)
#       shares_expr       <- rlang::sym(shares_name)
#       eps_expr          <- rlang::sym(eps_name)
#       book_val_expr     <- rlang::sym(book_val_name)
#       op_cash_flow_expr <- rlang::sym(op_cash_flow_name)
#
#       units <- stringr::str_split(revenue_name, pattern = " ")[[1]][[2]]
#
#       if (units != "USD") {
#           warning("Units of Key Ratios Not Compatible for Valuation Ratios.
#                   Returning key ratios without Valuation Ratios.")
#           return(key_ratios)
#       }
#
#       revenue_per_share_name   <- paste0("Revenue Per Share ", units)
#       cash_flow_per_share_name <- paste0("Cash Flow Per Share ", units)
#
#       revenue_per_share_expr   <- rlang::sym(revenue_per_share_name)
#       cash_flow_per_share_expr <- rlang::sym(cash_flow_per_share_name)
#
#       # Get key ratios
#       valuations_1 <- key_ratios_bind %>%
#           dplyr::filter(section == "Financials") %>%
#           # dplyr::filter(category %in% c("Revenue USD Mil",
#           #                               "Shares Mil",
#           #                               "Earnings Per Share USD",
#           #                               "Book Value Per Share * USD",
#           #                               "Operating Cash Flow USD Mil")) %>%
#           dplyr::filter(category %in% c(
#               revenue_name, shares_name, eps_name, book_val_name, op_cash_flow_name
#           )) %>%
#           dplyr::mutate(year = lubridate::year(date)) %>%
#           dplyr::select(year, category, value) %>%
#           tidyr::spread(key = category, value = value) %>%
#           # dplyr::mutate(`Revenue Per Share USD` = `Revenue USD Mil` / `Shares Mil`,
#           #               `Cash Flow Per Share USD` = `Operating Cash Flow USD Mil` / `Shares Mil`) %>%
#           dplyr::mutate(
#               !! revenue_per_share_name   := (!! revenue_expr) / (!! shares_expr),
#               !! cash_flow_per_share_name := (!! op_cash_flow_expr) / (!! shares_expr)
#               ) %>%
#           # dplyr::select(year,
#           #               `Earnings Per Share USD`,
#           #               `Revenue Per Share USD`,
#           #               `Book Value Per Share * USD`,
#           #               `Cash Flow Per Share USD`)
#           dplyr::select(
#               year, !! eps_expr, !! revenue_per_share_expr, !! book_val_expr, !! cash_flow_per_share_expr
#           )
#
#       # Merge and calculate valuations
#       valuation <- dplyr::left_join(valuations_1, valuations_2, by = "year") %>%
#           # dplyr::mutate(`Price to Earnings`  = adjusted / `Earnings Per Share USD`,
#           #               `Price to Sales`     = adjusted / `Revenue Per Share USD`,
#           #               `Price to Book`      = adjusted / `Book Value Per Share * USD`,
#           #               `Price to Cash Flow` = adjusted / `Cash Flow Per Share USD`) %>%
#           dplyr::mutate(`Price to Earnings`  = adjusted / !! eps_expr,
#                         `Price to Sales`     = adjusted / !! revenue_per_share_expr,
#                         `Price to Book`      = adjusted / !! book_val_expr,
#                         `Price to Cash Flow` = adjusted / !! cash_flow_per_share_expr) %>%
#           dplyr::select(date,
#                         `Price to Earnings`,
#                         `Price to Sales`,
#                         `Price to Book`,
#                         `Price to Cash Flow`) %>%
#           tidyr::gather(key = category, value = value, -date) %>%
#           dplyr::select(category, date, value) %>%
#           dplyr::mutate(date = lubridate::as_date(date))
#
#       # Get last group number
#       last_group_num <- key_ratios_bind$group %>% max()
#
#       # Create valuation tibble and bind_rows
#       valuation_bind <- dplyr::bind_cols(
#           tibble::tibble(section = rep("Valuation Ratios", nrow(valuation))),
#           tibble::tibble(sub.section = rep("Valuation Ratios", nrow(valuation))),
#           tibble::tibble(group = rep(seq(last_group_num + 1, length.out = 4), each = 10)),
#           valuation)
#
#       key_ratios <- dplyr::bind_rows(key_ratios_bind, valuation_bind) %>%
#           dplyr::group_by(section) %>%
#           tidyr::nest()
#
#       return(key_ratios)
#
#   }, warning = function(w) {
#
#       warn <- w
#       if (map == TRUE) warn <- paste0(x, ": ", w)
#       warning(warn, call. = FALSE)
#       return(key_ratios)
#
#   }, error = function(e) {
#
#       warn <- paste0("x = '", x, "', get = 'key.ratios", "': ", e)
#       if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
#       warning(warn, call. = FALSE)
#       return(NA) # Return NA on error
#
#   })
#
# }

# YAHOO FINANCE NO LONGER SUPPORTS
# Util 3: key.stats -----
# tq_get_util_3 <- function(x, get, complete_cases, map, ...) {
#
#     # Check x
#     if (!is.character(x)) {
#         stop("x must be a character string in the form of a valid symbol.")
#     }
#
#     # Convert x to uppercase
#     x <- stringr::str_to_upper(x) %>%
#         stringr::str_trim(side = "both") %>%
#         stringr::str_replace_all("[[:punct:]]", "")
#
#     tryCatch({
#
#         # Download file
#         tmp <- tempfile()
#         url_base_1 <- 'http://download.finance.yahoo.com/d/quotes.csv?s='
#         url_base_2 <- '&f='
#         url_base_3 <- '&e=.csv'
#         yahoo_tag_list <- stringr::str_c(yahoo_tags$yahoo.tag, collapse = "")
#         url <- paste0(url_base_1, x, url_base_2, yahoo_tag_list, url_base_3)
#
#         # Try various stock exchanges
#         download.file(url, destfile = tmp, quiet = TRUE)
#
#         # Read data
#         key_stats_raw <- suppressMessages(
#             suppressWarnings(
#                 readr::read_csv(tmp, col_names = FALSE, na = c("", "NA", "N/A", "<NA>"))
#             )
#         )
#
#         # Unlink tmp
#         unlink(tmp)
#
#         # Format tidy data frame ----
#
#         # Names
#         key_stat_names <- yahoo_tags$yahoo.tag.desc %>%
#             make.names()
#         names(key_stats_raw) <- key_stat_names
#
#         # Main formatting script
#         suppressWarnings(
#             key_stats <- key_stats_raw %>%
#                 dplyr::mutate(Ask = as.numeric(Ask),
#                               Ask.Size = as.numeric(Ask.Size),
#                               Average.Daily.Volume = as.numeric(Average.Daily.Volume),
#                               Bid = as.numeric(Bid),
#                               Bid.Size = as.numeric(Bid.Size),
#                               Book.Value = as.numeric(Book.Value),
#                               Change = as.numeric(Change),
#                               Change.From.200.day.Moving.Average = as.numeric(Change.From.200.day.Moving.Average),
#                               Change.From.50.day.Moving.Average = as.numeric(Change.From.50.day.Moving.Average),
#                               Change.From.52.week.High = as.numeric(Change.From.52.week.High),
#                               Change.From.52.week.Low = as.numeric(Change.From.52.week.Low),
#                               Change.in.Percent = convert_to_percent(Change.in.Percent),
#                               Currency = as.character(Currency),
#                               Days.High = as.numeric(Days.High),
#                               Days.Low = as.numeric(Days.Low),
#                               Days.Range = as.character(Days.Range),
#                               Dividend.Pay.Date = lubridate::mdy(Dividend.Pay.Date),
#                               Dividend.per.Share = as.numeric(Dividend.per.Share),
#                               Dividend.Yield = as.numeric(Dividend.Yield),
#                               EBITDA = convert_to_numeric(EBITDA),
#                               EPS = as.numeric(EPS),
#                               EPS.Estimate.Current.Year = as.numeric(EPS.Estimate.Current.Year),
#                               EPS.Estimate.Next.Quarter = as.numeric(EPS.Estimate.Next.Quarter),
#                               EPS.Estimate.Next.Year = as.numeric(EPS.Estimate.Next.Year),
#                               Ex.Dividend.Date = lubridate::mdy(Ex.Dividend.Date),
#                               Float.Shares = as.numeric(Float.Shares),
#                               High.52.week = as.numeric(High.52.week),
#                               Last.Trade.Date = lubridate::mdy(Last.Trade.Date),
#                               Last.Trade.Price.Only = as.numeric(Last.Trade.Price.Only),
#                               Last.Trade.Size = as.numeric(Last.Trade.Size),
#                               Last.Trade.With.Time = as.character(Last.Trade.With.Time),
#                               Low.52.week = as.numeric(Low.52.week),
#                               Market.Capitalization = convert_to_numeric(Market.Capitalization),
#                               Moving.Average.200.day = as.numeric(Moving.Average.200.day),
#                               Moving.Average.50.day = as.numeric(Moving.Average.50.day),
#                               Name = as.character(Name),
#                               Open = as.numeric(Open),
#                               PE.Ratio = as.numeric(PE.Ratio),
#                               PEG.Ratio = as.numeric(PEG.Ratio),
#                               Percent.Change.From.200.day.Moving.Average = convert_to_percent(Percent.Change.From.200.day.Moving.Average),
#                               Percent.Change.From.50.day.Moving.Average = convert_to_percent(Percent.Change.From.50.day.Moving.Average),
#                               Percent.Change.From.52.week.High = convert_to_percent(Percent.Change.From.52.week.High),
#                               Percent.Change.From.52.week.Low = convert_to_percent(Percent.Change.From.52.week.Low),
#                               Previous.Close = as.numeric(Previous.Close),
#                               Price.to.Book = as.numeric(Price.to.Book),
#                               Price.to.EPS.Estimate.Current.Year = as.numeric(Price.to.EPS.Estimate.Current.Year),
#                               Price.to.EPS.Estimate.Next.Year = as.numeric(Price.to.EPS.Estimate.Next.Year),
#                               Price.to.Sales = as.numeric(Price.to.Sales),
#                               Range.52.week = as.character(Range.52.week),
#                               Revenue = convert_to_numeric(Revenue),
#                               Shares.Outstanding = as.numeric(Shares.Outstanding),
#                               Short.Ratio = as.numeric(Short.Ratio),
#                               Stock.Exchange = as.character(Stock.Exchange),
#                               Target.Price.1.yr. = as.numeric(Target.Price.1.yr.),
#                               Volume = as.numeric(Volume)
#                 )
#         )
#
#         # Sort by column name
#         key_stats_sorted <- key_stats[, order(names(key_stats))]
#
#         # Handling for all NAs
#         if (all(is.na(key_stats_sorted))) {
#             warn <- paste0("x = '", x, "', get = 'key.stats", "': Value is not available.")
#             if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
#             warning(warn, call. = FALSE)
#             return(NA) # Return NA on error
#         }
#
#         return(key_stats_sorted)
#
#     }, error = function(e) {
#
#         warn <- paste0("x = '", x, "', get = 'key.stats", "': ", e)
#         if (map == TRUE && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
#         warning(warn, call. = FALSE)
#         return(NA) # Return NA on error
#
#     })
#
# }
