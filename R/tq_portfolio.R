#' Aggregates a group of returns by security into portfolio returns
#'
#' @param data A \code{tibble} (tidy data frame) of returns in tidy format (i.e long format).
#' @param assets_col The column with assets (securities)
#' @param returns_col The column with returns
#' @param weights Optional parameter for the security weights, which can be passed as a numeric vector the length of
#' the number of securities or a two column tibble with security names in first column
#' and weights in second column.
#' @param col_rename A string or character vector containing names that can be used
#' to quickly rename columns.
#' @param ... Additional parameters passed to \code{PerformanceAnalytics::Returns.portfolio}
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details \code{tq_portfolio} is a wrapper for \code{PerformanceAnalytics::Returns.portfolio},
#' The main advantage is the results are returned as a \code{tibble} and the
#' function can be used with the \code{tidyverse}.
#'
#' \code{assets_col} and \code{returns_col} are columns within \code{data} that are used
#' to compute returns for a portfolio. The columns should be in "long" format (or "tidy" format)
#' meaning there is only one column containing all of the securities (i.e. not in "wide" format
#' with returns spread by security).
#'
#' \code{weights} are the weights to be applied to the security returns.
#' Weights can be input in one of two options:
#' \itemize{
#'   \item A numeric vector of weights that is the same length as unique number of securities.
#'   The weights are applied in the order of the securities.
#'   \item A two column tibble with securities in the first column and weights in the second column.
#'   The advantage to this method is the weights are mapped to the securities and any unlisted
#'   securities default to a weight of zero.
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{tq_transform}} which can be used to get period returns.
#'   \item \code{\link[PerformanceAnalytics]{Return.portfolio}} which is the underlying function
#'   that specifies which parameters can be passed via \code{...}
#' }
#'
#'
#' @name tq_portfolio
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' # Get stock prices
#' stock_prices <- c("AAPL", "GOOG", "NFLX") %>%
#'     tq_get(get  = "stock.prices",
#'            from = "2010-01-01",
#'            to   = "2015-12-31")
#'
#' # Get returns for individual stock components
#' portfolio_monthly_returns <- stock_prices %>%
#'     group_by(symbol) %>%
#'     tq_transform(Ad, periodReturn, period = "monthly")
#'
#' # Method 1: Use tq_portfolio with numeric vector of weights
#' weights = c(0.5, 0, 0.5)
#' tq_portfolio(data = portfolio_monthly_returns,
#'              assets_col = symbol,
#'              returns_col = monthly.returns,
#'              weights = weights,
#'              col_rename = NULL,
#'              wealth.index = FALSE)
#'
#' # Method 2: Use tq_portfolio with two column tibble and map weights
#' # Note that GOOG's weighting is zero in Method 1. In Method 2,
#' # GOOG is not added and same result is achieved.
#' weights_df <- tibble(symbol = c("AAPL", "NFLX"),
#'                      weights = c(0.5, 0.5))
#' tq_portfolio(data = portfolio_monthly_returns,
#'              assets_col = symbol,
#'              returns_col = monthly.returns,
#'              weights = weights_df,
#'              col_rename = NULL,
#'              wealth.index = FALSE)




# PRIMARY FUNCTIONS ----

#' @rdname tq_portfolio
#' @export
tq_portfolio <- function(data, assets_col, returns_col, weights = NULL, col_rename = NULL, ...) {

    # Convert to NSE
    assets_col <- deparse(substitute(assets_col))
    returns_col <- deparse(substitute(returns_col))

    tq_portfolio_base_(data = data, assets_col = assets_col, returns_col = returns_col,
                       weights = weights, col_rename = col_rename, ...)

}

#' @rdname tq_portfolio
#' @export
tq_portfolio_ <- function(data, assets_col, returns_col, weights = NULL, col_rename = NULL, ...) {

    tq_portfolio_base_(data = data, assets_col = assets_col, returns_col = returns_col,
                       weights = weights, col_rename = col_rename, ...)

}

tq_portfolio_base_ <- function(data, assets_col, returns_col, weights, col_rename, ...) {

    # Check data
    check_data_is_data_frame(data)

    # Ungroup grouped data frames
    if (dplyr::is.grouped_df(data)) data <- dplyr::ungroup(data)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Get timezone
    time_zone <- get_time_zone(data, date_col_name)

    # Select date
    date_col <- dplyr::select_(data, date_col_name)

    # Re-map assets and returns column names
    returns_col_name <- returns_col
    assets_col_name <- assets_col
    returns_col <- dplyr::select_(data, returns_col)
    assets_col <- dplyr::select_(data, assets_col)


    # Handle weights
    check_weights(weights, assets_col)
    if (is.data.frame(weights)) weights <- map_weights(weights, assets_col)

    # Apply function
    ret <- data %>%
        tidyr::spread_(key_col = assets_col_name, value_col = returns_col_name) %>%
        as_xts_(date_col = date_col_name) %>%
        PerformanceAnalytics::Return.portfolio(weights = weights, verbose = FALSE, ...)

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
                                                  time_zone, col_rename)

    ret

}

# UTILITY FUNCTIONS -----

map_weights <- function(weights, assets_col) {

    y <- names(assets_col)[[1]]
    x <- names(weights)[[1]]

    ret <- dplyr::left_join(unique(assets_col), weights,
                            by = setNames(x, y)) %>%
        dplyr::rename_(weights = names(weights)[[2]]) %>%
        tidyr::replace_na(list(weights = 0)) %>%
        dplyr::select(weights) %>%
        unlist() %>%
        as.numeric()

    ret

}

check_weights <- function(weights, assets_col) {

    if (inherits(weights, "data.frame")) {

        unique_weights <- unique(weights[,1]) %>% unlist() %>% unname()
        unique_assets <- unique(assets_col) %>% unlist() %>% unname()

        if (all(unique_weights %in% unique_assets)) {

            if (ncol(weights) == 2) {

                if (sum(weights[,2]) == 1) {
                    return()
                } else {
                    stop("Sum of weights must be 1.")
                }

            } else {
                stop("Incorrect number of columns. Only two allowed.")
            }

        } else {

            stop("The assets in weights does not match the assets in data.")

        }


    } else if (is.numeric(weights)) {

        if (length(weights) == assets_col %>% unique() %>% nrow()) {
            if (sum(weights) == 1) {
                return()
            } else {
                stop("Sum of weights must be 1.")
            }
        } else {
            stop("The number of weights does not match the number of unique assets.")
        }

    } else if (is.null(weights)) {
        message("No portfolio weights supplied. Implementing equal weighting.")
        return()

    } else {

        stop("Incorrect data type for weights. Valid options are data frame, numeric vector, or NULL.")

    }


}
