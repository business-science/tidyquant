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
#' @name av_api_key
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
av_api_key <- alphavantager::av_api_key


