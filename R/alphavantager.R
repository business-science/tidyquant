#' Set Alpha Vantage API Key
#'
#' @param api_key Optionally passed parameter to set Alpha Vantage `api_key`.
#'
#' @return Returns invisibly the currently set `api_key`
#'
#' @details A wrapper for `alphavantager::av_api_key()`
#'
#' @seealso [tq_get()] `get = "alphavantager"`
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' av_api_key(api_key = "foobar")
#' }
#'
#' @name av_api_key
#' @export
av_api_key <- function(api_key) {
    if(!requireNamespace("alphavantager", quietly = TRUE)) {
        stop("alphavantager must be installed to use this functionality.", call. = FALSE)
    }
    alphavantager::av_api_key(api_key)
}


