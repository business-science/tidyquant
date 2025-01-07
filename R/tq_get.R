#' Get quantitative data in `tibble` format
#'
#' @param x A single character string, a character vector or tibble representing a single (or multiple)
#' stock symbol, metal symbol, currency combination, FRED code, etc.
#' @param get A character string representing the type of data to get
#' for `x`. Options include:
#' \itemize{
#'   \item `"stock.prices"`: Get the open, high, low, close, volume and adjusted
#'   stock prices for a stock symbol from
#'   Yahoo Finance (https://finance.yahoo.com/). Wrapper for `quantmod::getSymbols()`.
#'   \item `"dividends"`: Get the dividends for a stock symbol from
#'   Yahoo Finance (https://finance.yahoo.com/). Wrapper for `quantmod::getDividends()`.
#'   \item `"splits"`: Get the split ratio for a stock symbol from
#'   Yahoo Finance (https://finance.yahoo.com/). Wrapper for `quantmod::getSplits()`.
#'   \item `"stock.prices.japan"`: Get the open, high, low, close, volume and adjusted
#'   stock prices for a stock symbol from
#'   Yahoo Finance Japan. Wrapper for `quantmod::getSymbols.yahooj()`.
#'   \item `"economic.data"`: Get economic data from
#'   \href{https://fred.stlouisfed.org/}{FRED}. rapper for `quantmod::getSymbols.FRED()`.
#'   \item `"quandl"`: Get data sets from
#'   \href{https://data.nasdaq.com/}{Quandl}. Wrapper for `Quandl::Quandl()`.
#'   See also [quandl_api_key()].
#'   \item `"quandl.datatable"`: Get data tables from
#'   \href{https://data.nasdaq.com/}{Quandl}. Wrapper for `Quandl::Quandl.datatable()`.
#'   See also [quandl_api_key()].
#'   \item `"tiingo"`: Get data sets from
#'   Tingo (https://www.tiingo.com/). Wrapper for `riingo::riingo_prices()`.
#'   See also [tiingo_api_key()].
#'   \item `"tiingo.iex"`: Get data sets from
#'   Tingo (https://www.tiingo.com/). Wrapper for `riingo::riingo_iex_prices()`.
#'   See also [tiingo_api_key()].
#'   \item `"tiingo.crypto"`: Get data sets from
#'   Tingo (https://www.tiingo.com/). Wrapper for `riingo::riingo_crypto_prices()`.
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
#'
#' * `from`: Standardized for time series functions in `quantmod`, `quandl`, `tiingo`, `alphavantager` packages.
#'   A character string representing a start date in
#'   YYYY-MM-DD format.
#' * `to`: Standardized for time series functions in `quantmod`, `quandl`, `tiingo`, `alphavantager` packages.
#'   A character string representing a end date in
#'   YYYY-MM-DD format.
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
#' can be seamlessly used with the tidyverse: `purrr`, `tidyr`, and
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
#'
#' # Get the list of `get` options
#' tq_get_options()
#'
#' # Get stock prices for a stock from Yahoo
#' aapl_stock_prices <- tq_get("AAPL")
#'
#' # Get stock prices for multiple stocks
#' mult_stocks <- tq_get(c("META", "AMZN"),
#'                       get  = "stock.prices",
#'                       from = "2016-01-01",
#'                       to   = "2017-01-01")
#'
#'
#' \dontrun{
#'
#' # --- Quandl ---
#' if (rlang::is_installed("quandl")) {
#' quandl_api_key('<your_api_key>')
#' tq_get("EIA/PET_MTTIMUS1_M", get = "quandl", from = "2010-01-01")
#' }
#'
#'
#' # Energy data from EIA
#'
#'
#'
#' # --- Tiingo ---
#' if (rlang::is_installed("riingo")) {
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
#' }
#'
#' # --- Alpha Vantage ---
#'
#' if (rlang::is_installed("alphavantager")) {
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
#' # FX DAILY
#' tq_get("USD/EUR", get = "alphavantage", av_fun = "FX_DAILY", outputsize = "full")
#'
#' # FX REAL-TIME QUOTE
#' tq_get("USD/EUR", get = "alphavantage", av_fun = "CURRENCY_EXCHANGE_RATE")
#'
#' }
#' }

# PRIMARY FUNCTIONS ----

tq_get <- function(x, get = "stock.prices", complete_cases = TRUE, ...) {

    get <- stringr::str_to_lower(get)

    if (length(get) > 1) stop(call. = FALSE, "tq_get(): Please use only one value for `get` source.")

    if("quandl" %in% get) {
        rlang::check_installed("Quandl")
        if (is.null(quandl_api_key())) warning("No Quandl API key detected. Limited to 50 anonymous calls per day. Set key with 'quandl_api_key()'.", call. = FALSE)
    }

    if(stringr::str_detect("alphavantage", get)) {
        rlang::check_installed("alphavantager", "to access the alphavantage API.")
        if (is.null(tidyquant::av_api_key())) stop("No Alpha Vantager API key detected. Set key with 'av_api_key()'.", call. = FALSE)
    }

    if(stringr::str_detect("tiingo", get)) {
        rlang::check_installed("riingo", "to use the riingo API.")
        if (is.null(riingo::riingo_get_token())) stop("No Tiingo API key detected. Set key with 'tiingo_api_key()'.", call. = FALSE)
    }

    if("rblpapi" %in% get) {
        rlang::check_installed("Rblpapi")
        tryCatch({
            Rblpapi::blpConnect()
        }, error = function(e) {
            warning("Failed to connect to Bloomberg. Ensure you have a valid session.", call. = FALSE)
            return(NULL)
        })
    }

    if("key.stats" %in% get) {
        warning("Yahoo Key Statistics has been discontinued.", call. = FALSE)
        return(NULL)
    }
    if("stock.prices.google" %in% get) {
        warning("Google Finance has been discontinued.", call. = FALSE)
        return(NULL)
    }

    # Handle API special cases
    if (stringr::str_detect(get, "tiingo")) {
        ret <- tq_get_base(x, get, complete_cases = complete_cases, map = FALSE, ...)
        return(ret)
    }

    # Distribute operations based on x
    ret <- tryCatch({
        if (is.character(x) && length(x) == 1 && length(get) == 1) {
            ret <- tq_get_base(x, get, complete_cases = complete_cases, map = FALSE, ...)
            if (tibble::is_tibble(ret)) ret <- ret %>% tibble::add_column(symbol = x, .before = 1)
        } else if (is.character(x)) {
            col_name <- names(x)
            if (is.null(col_name)) col_name <- "symbol"
            x_tib <- tibble::tibble(symbol.. = x)
            ret <- tq_get_map(x = x_tib, get = get, complete_cases = complete_cases, ...)
            names(ret)[[1]] <- col_name[[1]]
        } else if (inherits(x, "data.frame")) {
            if (inherits(x, "grouped_df")) {
                warning("Ungrouping grouped data frame")
                x <- dplyr::ungroup(x)
            }
            col_name <- colnames(x)[[1]]
            names(x)[[1]] <- "symbol.."
            x_tib <- tibble::as_tibble(x)
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

    }, error = function(e) {
        warning(paste("Failed to retrieve data:", e$message), call. = FALSE)
        return(NULL)
    })

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
    if (identical(get, "financial") && inherits(ret, "financials")) {

        # Tidy a single financial statement
        tidy_fin <- function(x) {

            group <- seq_len(nrow(x))

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
            dplyr::mutate(data = purrr::map(.x = .data$data, .f = tidy_fin)) %>%
            dplyr::select(-retrieve) %>%
            tidyr::spread(key = period, value = "data") %>%
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
  rlang::check_installed("Quandl")
    # Check x
    if (!is.character(x)) {
        stop("x must be a character string in the form of a valid symbol.")
    }

    # Convert x to uppercase
    x <- stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both")

    ret <- tryCatch({

        Quandl::Quandl.datatable(code = x, paginate = paginate, ...) %>%
            tibble::as_tibble()


    }, error = function(e) {

        warn <- paste0("x = '", x, "', get = 'quandl.datatable", "': ", e)
        if (map && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
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

  rlang::check_installed("riingo", "to get the Tiingo API.")

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
        dplyr::relocate("ticker", "date", "open", "high", "low", "close", "volume", "adjClose") %>%
        dplyr::rename(adjusted = adjClose,
               symbol   = ticker)
    }

    if (get == "tiingoiex") {
      ret <- ret %>%
        dplyr::relocate(ticker, date, open, high, low, close) %>%
        dplyr::rename(symbol = ticker)
    }

    if (get == "tiingocrypto") {
      ret <- ret %>%
        dplyr::rename(symbol = ticker)
    }

  }, error = function(e) {

    warn <- paste0("x = '", x, "', get = '", get, "': ", e)
    if (map && complete_cases) warn <- paste0(warn, " Removing ", x, ".")
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

    return(get)
}


