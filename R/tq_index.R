#' Get all stocks in a stock index in \code{tibble} format
#'
#' @param x A single character string, a character vector or tibble representing a
#' single stock index or multiple stock indexes.
#' @param use_fallback A boolean that can be used to return a fallback data set
#' last downloaded when the package was updated. Useful if the website is down.
#' Set to \code{FALSE} by default.
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details
#' \code{tq_index()} returns the stock symbol and company name of every stock
#' in an index. Eighteen stock indexes are available. The source is
#' \href{http://www.marketvolume.com/indexes_exchanges/}{www.marketvolume.com}.
#'
#' \code{tq_index_options()} returns a list of stock indexes you can
#' choose from.
#'
#' @seealso
#' \code{\link{tq_get}} to get stock prices, financials, key stats, etc using the stock symbols.
#'
#' @rdname tq_index
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' # Get the list of stock index options
#' tq_index_options()
#'
#' # Get all stock symbols in a stock index
#' tq_index("SP500")


#' @rdname tq_index
#' @export
tq_index <- function(x, use_fallback = FALSE) {

    # Reformat x
    x <- stringr::str_to_upper(x) %>%
        stringr::str_trim(side = "both") %>%
        stringr::str_replace_all("[[:punct:]]", "")

    # Check if x is an appropriate index
    index_list <- tq_index_options()
    if (!(x %in% c(index_list))) {
        err <- paste0("Error: x must be a character string in the form of a valid index.",
                      " The following are valid options:\n",
                      stringr::str_c(index_list, collapse = ", ")
        )
        stop(err)
    }

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

    ret

}


#' @rdname tq_index
#' @export
tq_index_options <- function() {
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


