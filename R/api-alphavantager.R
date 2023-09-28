#' Set Alpha Vantage API Key
#'
#' Requires the alphavantager packager to use.
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
#' if (rlang::is_installed("alphavantager")) {
#' av_api_key(api_key = "foobar")
#' }
#' }
#'
#' @name av_api_key
#' @export
av_api_key <- function(api_key) {
    rlang::check_installed("alphavantager", "to use the alphavantager API.")
    alphavantager::av_api_key(api_key)
}


