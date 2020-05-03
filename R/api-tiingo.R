#' Set Tiingo API Key
#'
#' @param api_key Optionally passed parameter to set Tiingo `api_key`.
#'
#' @return Returns invisibly the currently set `api_key`
#'
#' @details A wrapper for `riingo::ringo_set_token()`
#'
#' @seealso [tq_get()] `get = "tiingo"`
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tiingo_api_key(api_key = "foobar")
#' }
#'
#' @name tiingo_api_key
#' @export
tiingo_api_key <- function(api_key) {
    if(!requireNamespace("riingo", quietly = TRUE)) {
        stop("riingo must be installed to use this functionality.", call. = FALSE)
    }

    if (missing(api_key)) return(riingo::riingo_get_token())

    riingo::riingo_set_token(api_key)

}


