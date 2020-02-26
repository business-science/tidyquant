#' Query or set Quandl API Key
#'
#' @param api_key Optionally passed parameter to set Quandl `api_key`.
#'
#' @return Returns invisibly the currently set `api_key`
#'
#' @details A wrapper for `Quandl::Quandl.api_key()`
#'
#' @seealso [tq_get()] `get = "quandl"`
#'
#' @name quandl_api_key
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' quandl_api_key(api_key = "foobar")
#' }
NULL

#' @rdname quandl_api_key
#' @export
quandl_api_key <- function(api_key) {

    if (!missing(api_key)) {
        options(Quandl.api_key = api_key)
    }
    invisible(getOption('Quandl.api_key'))

}


#' Search the Quandl database
#'
#' @inheritParams Quandl::Quandl.search
#'
#' @return Returns a tibble with search results.
#'
#' @details A wrapper for `Quandl::Quandl.search()`
#'
#' @seealso [tq_get()] `get = "quandl"`
#'
#' @name quandl_search
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' quandl_search(query = "oil")
#' }
NULL

#' @rdname quandl_search
#' @export
quandl_search <- function(query, silent = FALSE, per_page = 10, ...) {

    Quandl.search(query = query, silent = silent, per_page = per_page, ...) %>%
        tibble::as_tibble()

}
