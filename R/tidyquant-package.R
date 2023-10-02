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
"_PACKAGE"

## usethis namespace: start
#' @import quantmod
#' @import lubridate
#' @import TTR
#' @import zoo
#' @import xts
#' @importFrom rlang := .data
#' @importFrom magrittr %$%
#' @importFrom utils download.file read.csv
#' @importFrom TTR SMA runSD MACD runCor BBands
#' @importFrom xts to.period xts to.monthly lag.xts
#' @importFrom zoo rollapply
## usethis namespace: end
NULL
