#' tidyquant: Integrating quantitative financial analysis tools with the tidyverse
#'
#' The main advantage of `tidyquant` is to
#' bridge the gap between the best quantitative resources for collecting and
#' manipulating quantitative data, `xts`, `quantmod` and `TTR`,
#' and the data modeling workflow and infrastructure of the `tidyverse`.
#'
#' @details
#' In this package, `tidyquant` functions and supporting data sets are
#' provided to seamlessly combine tidy tools with existing quantitative
#' analytics packages. The main advantage is being able to use tidy
#' functions with purrr for mapping and tidyr for nesting to extend modeling to
#' many stocks. See the tidyquant website for more information, documentation
#' and examples.
#'
#' Users will probably be interested in the following:
#' \itemize{
#'   \item \strong{Getting Data from the Web:} [tq_get()]
#'   \item \strong{Manipulating Data:} [tq_transmute()] and [tq_mutate()]
#'   \item \strong{Performance Analysis and Portfolio Aggregation:}
#'   [tq_performance()] and [tq_portfolio()]
#' }
#'
#' To learn more about tidyquant, start with the vignettes:
#'  `browseVignettes(package = "tidyquant")`
#'
#' @docType package
#' @name tidyquant
#'
#' @import quantmod
#' @import lubridate
#' @import PerformanceAnalytics
#' @importFrom rlang ":="
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%$%"
#' @importFrom utils "data" "download.file" "read.csv"
#' @importFrom TTR "SMA"
#' @importFrom xts "to.period"
#' @importFrom Quandl "Quandl" "Quandl.datatable" "Quandl.api_key" "Quandl.search"

NULL
