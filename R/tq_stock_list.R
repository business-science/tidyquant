#' Get all stocks in a stock index or stock exchange in `tibble` format
#'
#' @param x A single character string, a character vector or tibble representing a
#' single stock index or multiple stock indexes.
#' @param use_fallback A boolean that can be used to return a fallback data set
#' last downloaded when the package was updated. Useful if the website is down.
#' Set to `FALSE` by default.
#'
#' @return Returns data in the form of a `tibble` object.
#'
#' @details
#' `tq_index()` returns the stock symbol, company name, weight, and sector of every stock
#' in an index.
#'
#' `tq_index_options()` returns a list of stock indexes you can
#' choose from.
#'
#' `tq_exchange()` returns the stock symbol, company, last sale price,
#' market capitalization, sector and industry of every stock
#' in an exchange. Three stock exchanges are available (AMEX, NASDAQ, and NYSE).
#'
#' `tq_exchange_options()` returns a list of stock exchanges you can
#' choose from. The options are AMEX, NASDAQ and NYSE.
#'
#' `tq_fund_holdings()` returns the the stock symbol, company name, weight, and sector of every stock
#' in an fund. The `source` parameter specifies which investment management company to use.
#' Example: `source = "SSGA"` connects to State Street Global Advisors (SSGA).
#' If `x = "SPY"`, then SPDR SPY ETF holdings will be returned.
#'
#' `tq_fund_source_options()`: returns the options that can be used for the `source` API for `tq_fund_holdings()`.
#'
#' @seealso
#' [tq_get()] to get stock prices, financials, key stats, etc using the stock symbols.
#'
#'
#' @examples
#'
#' # Stock Indexes:
#'
#' # Get the list of stock index options
#' tq_index_options()
#'
#' # Get all stock symbols in a stock index
#' \dontrun{
#' tq_index("DOW")
#' }
#'
#' # Stock Exchanges:
#'
#' # Get the list of stock exchange options
#' tq_exchange_options()
#'
#' # Get all stocks in a stock exchange
#' \dontrun{
#' tq_exchange("NYSE")
#' }
#'
#' # Mutual Funds and ETFs:
#'
#' # Get the list of stock exchange options
#' tq_fund_source_options()
#'
#' # Get all stocks in a fund
#' \dontrun{
#' tq_fund_holdings("SPY", source = "SSGA")
#' }
#'
#' @name tq_index
#' @export

# tq_index -----

#' @rdname tq_index
#' @export
tq_index <- function(x, use_fallback = FALSE) {

    # Clean index name
    x <- clean_index(x)

    # Verify index
    verified <- tryCatch({
        verify_index(x)
    }, error = function(e) {
        warning(paste("Error verifying index:", e$message), call. = FALSE)
        return(NULL)
    })

    # If verification failed or not a verified index, return a warning and empty tibble
    if(is.null(verified) || !verified$is_verified) {
        warning(verified$err)
        return(tibble::tibble())
    }

    # Use fallback if requested
    if(use_fallback) {
        return(tryCatch({
            index_fallback(x)
        }, error = function(e) {
            warning(paste("Error using fallback for index:", e$message), call. = FALSE)
            return(tibble::tibble())
        }))
    }

    # Convert index name to SPDR ETF name
    x_spdr <- tryCatch({
        spdr_mapper(x)
    }, error = function(e) {
        warning(paste("Error mapping SPDR ETF name:", e$message), call. = FALSE)
        return(NULL)
    })

    # If SPDR mapping failed, return an empty tibble
    if(is.null(x_spdr)) {
        return(tibble::tibble())
    }

    # Download the index data
    dload <- tryCatch({
        ssga_download(x_spdr, index_name = x)
    }, error = function(e) {
        warning(paste("Error downloading index data:", e$message), call. = FALSE)
        return(NULL)
    })

    # If download failed, return a warning and empty tibble
    if(is.null(dload) || !is.null(dload$err)) {
        warning(dload$err)
        return(tibble::tibble())
    }

    # Clean holdings
    df <- tryCatch({
        clean_holdings(dload$df)
    }, error = function(e) {
        warning(paste("Error cleaning index holdings:", e$message), call. = FALSE)
        return(tibble::tibble())
    })

    df
}

#' @rdname tq_index
#' @export
tq_index_options <- function() {
    c(
        "DOW",
        "DOWGLOBAL",
        "SP400",
        "SP500",
        "SP600"
    )
}


# tq_exchange ----

#' @rdname tq_index
#' @export
tq_exchange <- function(x) {

    # Reformat x
    x <- stringr::str_to_lower(x) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "")

    # Check if x is a valid exchange
    exchange_list <- tq_exchange_options() %>%
        stringr::str_to_lower()

    if (!(x %in% c(exchange_list))) {
        err <- paste0("Error: x must be a character string in the form of a valid exchange.",
                      " The following are valid options:\n",
                      stringr::str_c(tq_exchange_options(), collapse = ", "))
        stop(err, call. = FALSE)
    }

    # Download data
    message("Getting data...\n")
    base_path_1 <- "https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange="
    base_path_2 <- "&download=true"
    url <- paste0(base_path_1, x, base_path_2)

    # Perform the HTTP request with error handling
    response <- tryCatch({
        httr2::request(url) %>%
            httr2::req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36") %>%
            httr2::req_perform()
    }, error = function(e) {
        warning(paste0("Failed to retrieve data for exchange '", x, "'. ", e$message), call. = FALSE)
        return(NULL)
    })

    # If the response is NULL (failed request), return NA
    if (is.null(response)) {
        return(NA)
    }

    # Evaluate the response
    if (response$status_code == 200) {
        # Collect JSON and process the data
        content <- httr2::resp_body_json(response)
        exchange_tbl <- tryCatch({
            do.call(rbind, lapply(content$data$rows, tibble::as_tibble))
        }, error = function(e) {
            warning("Failed to process data from the response.", call. = FALSE)
            return(NULL)
        })

        # If the processing failed, return NA
        if (is.null(exchange_tbl)) {
            return(NA)
        }

        # Post-process and clean the data
        exchange <- exchange_tbl %>%
            dplyr::rename(
                symbol = symbol,
                company = name,
                last.sale.price = lastsale,
                market.cap = marketCap,
                country = country,
                ipo.year = ipoyear,
                sector = sector,
                industry = industry
            ) %>%
            dplyr::mutate(
                symbol = as.character(symbol),
                company = as.character(company),
                last.sale.price = as.numeric(stringr::str_remove(last.sale.price, "\\$")),
                market.cap = as.numeric(market.cap),
                country = as.character(country),
                ipo.year = as.integer(ipo.year),
                sector = as.character(sector),
                industry = as.character(industry)
            ) %>%
            dplyr::select(symbol:industry) %>%
            dplyr::select(-c(netchange, pctchange, volume))

        return(exchange)

    } else {
        warn <- paste0("Error retrieving data for exchange '", x, "'. Status code: ", response$status_code)
        warning(warn, call. = FALSE)
        return(NA)
    }
}


#' @rdname tq_index
#' @export
tq_exchange_options <- function() {
    c("AMEX", "NASDAQ", "NYSE")
}

# tq_fund_holdings ----

#' @rdname tq_index
#' @param source The API source to use.
#' @export
tq_fund_holdings <- function(x, source = "SSGA") {

    # Verify index
    verified <- tryCatch({
        verify_fund_source(source)
    }, error = function(e) {
        warning(paste("Error verifying index:", e$message), call. = FALSE)
        return(NULL)
    })

    # If verification failed or not a verified index, return a warning and empty tibble
    if(is.null(verified) || !verified$is_verified) {
        warning(verified$err)
        return(tibble::tibble())
    }

    # Download the index data
    dload <- tryCatch({
        source <- stringr::str_to_upper(source)

        if (source == "SSGA") {
            ssga_download(x, index_name = x)
        } else {

        }


    }, error = function(e) {
        warning(paste("Error downloading index data:", e$message), call. = FALSE)
        return(NULL)
    })

    # If download failed, return a warning and empty tibble
    if(is.null(dload) || !is.null(dload$err)) {
        warning(dload$err)
        return(tibble::tibble())
    }

    # Clean holdings
    df <- tryCatch({
        clean_holdings(dload$df)
    }, error = function(e) {
        warning(paste("Error cleaning index holdings:", e$message), call. = FALSE)
        return(tibble::tibble())
    })

    df

}

#' @rdname tq_index
#' @export
tq_fund_source_options <- function() {
    c("SSGA")
}

# Utility ----------------------------------------------------------------------------------------------------

# Casing and punctuation
clean_index <- function(x) {

    # Capitalize, trim space, and remove any punctuation
    stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "")
}

# Verify index is of the right form
verify_index <- function(x) {

    # Setup with initial values
    verified <- list(is_verified = FALSE, err = "")

    if(!(x %in% tq_index_options())) {

        verified$err <- paste0(x, " must be a character string in the form of a valid index. ",
                               "The following are valid options:\n",
                               stringr::str_c(tq_index_options(), collapse = ", "))
    } else {
        verified$is_verified <- TRUE
    }

    verified
}

verify_fund_source <- function(x) {

    # Setup with initial values
    verified <- list(is_verified = FALSE, err = "")

    if(!(x %in% tq_fund_source_options())) {

        verified$err <- paste0(x, " must be a character string in the form of a valid Fund Source. ",
                               "The following are valid options:\n",
                               stringr::str_c(tq_fund_source_options(), collapse = ", "))
    } else {
        verified$is_verified <- TRUE
    }

    verified
}

# Map the index to the SPDR ETF name
spdr_mapper <- function(x) {

    switch(x,
           RUSSELL1000 = "ONEK",
           RUSSELL2000 = "TWOK",
           RUSSELL3000 = "THRK",
           DOW         = "DIA",
           DOWGLOBAL   = "DGT",
           SP400       = "MDY",
           SP500       = "SPY",
           # SLY seems broken.
           # Using SLYG for S&P 600
           # https://www.ssga.com/us/en/institutional/etfs/funds/spdr-sp-600-small-cap-growth-etf-slyg
           SP600       = "SLYG",
           SP1000      = "SMD"
           )
}

# Download the index data from SPDR
ssga_download <- function(x, index_name) {

    # Contruct download link
    #     OLD (< 2019-12-15): https://us.spdrs.com/site-content/xls/SPY_All_Holdings.xls
    #     NEW (> 2019-12-15): https://www.ssga.com/us/en/institutional/etfs/library-content/products/fund-data/etfs/us/holdings-daily-us-en-spy.xlsx
    #     NEW (> 2024-08-16): https://www.ssga.com/us/en/institutional/library-content/products/fund-data/etfs/us/holdings-daily-us-en-spy.xlsx
    # spdr_link <- paste0("https://us.spdrs.com/site-content/xls/", x, "_All_Holdings.xls")
    spdr_link <- paste0("https://www.ssga.com/us/en/institutional/library-content/products/fund-data/etfs/us/holdings-daily-us-en-", tolower(x), ".xlsx")

    # Results container
    res <- list(df = NULL, err = NULL)

    # Message
    message("Getting holdings for ", index_name)

    # Download quietly
    tryCatch({

        # Download to disk, force as a xlsx
        curl::curl_download(spdr_link, tf <- tempfile(fileext = ".xlsx"))

        # Read the xls file
        suppressMessages({
            res$df <- readxl::read_xlsx(tf, skip = 3)
        })

        # Release temp file
        unlink(tf)

        return(res)

    }, error = function(e) {

        # On error, catch it and return
        res$err <- paste0("Error at ", index_name, x, " during download. \n", e)

        return(res)

    })
}

# Series of commands to clean the results from SPDR
clean_holdings <- function(x) {

    # Identify the last row of data
    last_row <- which(is.na(x[["Weight"]]))[1] - 1

    ret <- x %>%

        # Subset rows with data
        dplyr::slice(1:last_row) %>%

        # Remove rows of cash
        dplyr::filter(Sector != "Unassigned") %>%

        # Type convert (bad data has been removed)
        # as.is = TRUE to prevent character -> factor
        purrr::modify(~type.convert(., as.is = TRUE)) %>%

        # Reweight
        dplyr::mutate(Weight = Weight / sum(Weight))

    # Rename
    names(ret)[1:2] <- c("company", "symbol")
    names(ret) <- names(ret) %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" ", "_")

    # Reorder
    ret <- ret %>% dplyr::select(symbol, company, dplyr::everything())

    # Fix for stocks like BRK.B -> BRK-B for Yahoo Finance
    ret <- ret %>%
        dplyr::mutate(symbol = stringr::str_replace(symbol, "\\.", "-"))


    return(ret)
}

# Return fallback data if necessary
index_fallback <- function(x) {

    # Date of last download
    date.dload <- stock_indexes %>%
        dplyr::filter(index.option == x) %>%
        dplyr::pull(date.downloaded) %>%
        utils::head(1)

    # Date message
    message(paste0("Using fallback dataset last downloaded ",
                   date.dload[[1]]), ".")

    # Unnest the specific index
    stock_index <- stock_indexes %>%
        dplyr::filter(index.option == x) %>%
        dplyr::select(index.components) %>%
        tidyr::unnest(cols = index.components)

    stock_index
}
