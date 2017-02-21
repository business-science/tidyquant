#' Aggregates a group of returns by asset into portfolio returns
#'
#' @param data A \code{tibble} (tidy data frame) of returns in tidy format (i.e long format).
#' @param assets_col The column with assets (securities)
#' @param returns_col The column with returns
#' @param weights Optional parameter for the asset weights, which can be passed as a numeric vector the length of
#' the number of assets or a two column tibble with asset names in first column
#' and weights in second column.
#' @param col_rename A string or character vector containing names that can be used
#' to quickly rename columns.
#' @param n Number of times to repeat a data frame row-wise.
#' @param index_col_name A renaming function for the "index" column, used when repeating data frames.
#' @param ... Additional parameters passed to \code{PerformanceAnalytics::Returns.portfolio}
#'
#' @return Returns data in the form of a \code{tibble} object.
#'
#' @details \code{tq_portfolio} is a wrapper for \code{PerformanceAnalytics::Returns.portfolio}.
#' The main advantage is the results are returned as a \code{tibble} and the
#' function can be used with the \code{tidyverse}.
#'
#' \code{assets_col} and \code{returns_col} are columns within \code{data} that are used
#' to compute returns for a portfolio. The columns should be in "long" format (or "tidy" format)
#' meaning there is only one column containing all of the assets and one column containing
#' all of the return values (i.e. not in "wide" format with returns spread by asset).
#'
#' \code{weights} are the weights to be applied to the asset returns.
#' Weights can be input in one of three options:
#' \itemize{
#'   \item Single Portfolio: A numeric vector of weights that is the same length as unique number of assets.
#'   The weights are applied in the order of the assets.
#'   \item Single Portfolio: A two column tibble with assets in the first column and weights in the second column.
#'   The advantage to this method is the weights are mapped to the assets and any unlisted
#'   assets default to a weight of zero.
#'   \item Multiple Portfolios: A three column tibble with portfolio index in the first
#'   column, assets in the second column, and weights in the third column. The tibble
#'   must be grouped by portfolio index.
#' }
#'
#' \code{tq_repeat_df} is a simple function that repeats
#' a data frame \code{n} times row-wise (long-wise), and adds a new column for a portfolio index.
#' The function is used to assist in Multiple Portfolio analyses, and
#' is a useful precursor to \code{tq_portfolio}.
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
#' monthly_returns_stocks <- stock_prices %>%
#'     group_by(symbol) %>%
#'     tq_transform(Ad, periodReturn, period = "monthly")
#'
#' ##### Portfolio Aggregation Methods #####
#'
#' # Method 1: Use tq_portfolio with numeric vector of weights
#'
#' weights <- c(0.5, 0, 0.5)
#' tq_portfolio(data = monthly_returns_stocks,
#'              assets_col = symbol,
#'              returns_col = monthly.returns,
#'              weights = weights,
#'              col_rename = NULL,
#'              wealth.index = FALSE)
#'
#' # Method 2: Use tq_portfolio with two column tibble and map weights
#'
#' # Note that GOOG's weighting is zero in Method 1. In Method 2,
#' # GOOG is not added and same result is achieved.
#' weights_df <- tibble(symbol = c("AAPL", "NFLX"),
#'                      weights = c(0.5, 0.5))
#' tq_portfolio(data = monthly_returns_stocks,
#'              assets_col = symbol,
#'              returns_col = monthly.returns,
#'              weights = weights_df,
#'              col_rename = NULL,
#'              wealth.index = FALSE)
#'
#' # Method 3: Working with multiple portfolios
#'
#' # 3A: Duplicate monthly_returns_stocks multiple times
#' mult_monthly_returns_stocks <- tq_repeat_df(monthly_returns_stocks, n = 3)
#'
#' # 3B: Create weights table grouped by portfolio id
#' weights_table <- tribble(
#'     ~portfolio, ~stocks, ~weights,
#'     1,          "AAPL",  0.50,
#'     1,          "GOOG",  0.25,
#'     1,          "NFLX",  0.25,
#'     2,          "AAPL",  0.25,
#'     2,          "GOOG",  0.50,
#'     2,          "NFLX",  0.25,
#'     3,          "AAPL",  0.25,
#'     3,          "GOOG",  0.25,
#'     3,          "NFLX",  0.50
#'     ) %>%
#'     group_by(portfolio)
#'
#' # 3C: Scale to multiple portfolios
#' tq_portfolio(data = mult_monthly_returns_stocks,
#'              assets_col = symbol,
#'              returns_col = monthly.returns,
#'              weights = weights_table,
#'              col_rename = NULL,
#'              wealth.index = FALSE)




# PRIMARY FUNCTIONS ----

#' @rdname tq_portfolio
#' @export
tq_portfolio <- function(data, assets_col, returns_col, weights = NULL, col_rename = NULL, ...) {

    # NSE
    tq_portfolio_(
        data = data,
        assets_col = lazyeval::lazy_eval(assets_col),
        returns_col = lazyeval::lazy_eval(returns_col),
        weights = weights,
        col_rename = col_rename
    )
}

#' @rdname tq_portfolio
#' @export
tq_portfolio_ <- function(data, assets_col, returns_col, weights = NULL, col_rename = NULL, ...) {
    UseMethod("tq_portfolio_", data)
}


tq_portfolio_.grouped_df <- function(data, assets_col, returns_col, weights, col_rename, ...) {

    # Check weights and data compatibility
    check_data_weights_compatibility(data, weights)

    # Get groups
    group_names_data <- dplyr::groups(data)

    # Format data
    data_nested <- data %>%
        tidyr::nest() %>%
        dplyr::rename(returns.. = data)

    weights_nested <- weights %>%
        tidyr::nest() %>%
        dplyr::rename(weights.. = data)

    # Join data
    y <- names(data_nested)[[1]]
    x <- names(weights_nested)[[1]]
    data_weights <- left_join(data_nested, weights_nested, by = setNames(x, y))

    # Map data and weights to tq_portfolio_base_
    custom_function <- function(x, y, z) {
        tq_portfolio_(data = x,
                           weights = y,
                           x = z,
                           assets_col = assets_col,
                           returns_col = returns_col,
                           col_rename = col_rename,
                           map = TRUE,
                           ...)
    }

    data_weights %>%
        dplyr::mutate(
            portfolio.. = purrr::pmap(list(returns.., weights.., portfolio), custom_function),
            class.. = purrr::map_chr(.x = portfolio.., ~ class(.x)[[1]])
            ) %>%
        dplyr::filter(class.. != "logical") %>%
        dplyr::select(-c(returns.., weights.., class..)) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names_data)
}

tq_portfolio_.tbl_df <- function(data, assets_col, returns_col, weights, col_rename, map = FALSE, x = NULL, ...) {

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


    # Apply function
    ret <- tryCatch({
        # Handle weights
        check_weights(weights, assets_col, map, x)
        # Solve issue with spread re-ordering column names alphabetically
        # if (is.data.frame(weights)) weights <- map_weights(weights, assets_col)
        weights <- map_weights(weights, assets_col) # Note that weights are resorted to match spread reorder to alphabetical

        data %>%
            tidyr::spread_(key_col = assets_col_name, value_col = returns_col_name) %>%
            as_xts_(date_col = date_col_name) %>%
            PerformanceAnalytics::Return.portfolio(weights = weights, verbose = FALSE, ...)

    }, error = function(e) {

        warn <- e
        # if (map == TRUE) warn <- paste0(x, " had the following error: ", e, ". Removing portfolio ", x, ".")
        warning(warn)
        return(NA) # Return NA on error

    })

    # Coerce to tibble and convert date / datetime
    if (xts::is.xts(ret)) ret <- coerce_to_tibble(ret, date_col_name,
                                                  time_zone, col_rename)

    ret

}

#' @rdname tq_portfolio
#' @export
tq_repeat_df <- function(data, n, index_col_name = "portfolio") {

    if (!is.data.frame(data)) stop("data must be a tibble or data frame.")

    if (is.grouped_df(data)) {
        message(paste("Ungrouping data frame groups:", dplyr::groups(data)))
        data <- ungroup(data)
    }

    data_mult <- data[rep(seq_len(nrow(data)), n),]
    index     <- rep(1:n, each = nrow(data))

    ret <- dplyr::bind_cols(tibble(index), data_mult)

    colnames(ret)[[1]] <- index_col_name

    ret <- ret %>%
        group_by_(index_col_name)

    ret
}

# UTILITY FUNCTIONS -----

map_weights <- function(weights, assets_col) {

    if (is.data.frame(weights)) {

        # Get names
        y <- names(assets_col)[[1]]
        x <- names(weights)[[1]]

        # arrange added to sort in alphabetic order, which matches spread order
        ret <- dplyr::left_join(unique(assets_col), weights,
                                by = setNames(x, y)) %>%
            dplyr::rename_(weights = names(weights)[[2]]) %>%
            tidyr::replace_na(list(weights = 0)) %>%
            dplyr::arrange_(y) %>%
            dplyr::select(weights) %>%
            unlist() %>%
            as.numeric()

    } else {
        # must be numeric

        ret <- dplyr::bind_cols(unique(assets_col), tibble::tibble(weights))

        colnames(ret) <- c("assets", "weights")

        ret <- ret %>%
            dplyr::arrange(assets) %>%
            dplyr::select(weights) %>%
            unlist() %>%
            as.numeric()

    }

    ret

}

check_weights <- function(weights, assets_col, map, x) {

    if (inherits(weights, "data.frame")) {

        unique_weights <- unique(weights[,1]) %>% unlist() %>% unname()
        unique_assets <- unique(assets_col) %>% unlist() %>% unname()

        if (all(unique_weights %in% unique_assets)) {

            if (ncol(weights) == 2) {

                if (sum(weights[,2]) == 1) {
                    return()
                } else {
                    warn <- ""
                    if (map == TRUE) warn <- paste0("Portfolio ", x, ": ")
                    warning(paste0(warn, "Sum of weights does not equal 1."))
                }

            } else {
                warning("Incorrect number of columns. Only two allowed.")
            }

        } else {

            warning("The assets in weights does not match the assets in data.")

        }


    } else if (is.numeric(weights)) {

        if (length(weights) == assets_col %>% unique() %>% nrow()) {
            if (sum(weights) == 1) {
                return()
            } else {
                warning("Sum of weights must be 1.")
            }
        } else {
            warning("The number of weights does not match the number of unique assets.")
        }

    } else if (is.null(weights)) {
        message("No portfolio weights supplied. Implementing equal weighting.")
        return()

    } else {

        warning("Incorrect data type for weights. Valid options are data frame, numeric vector, or NULL.")

    }

}

check_data_weights_compatibility <- function(data, weights) {

    # Weights has one group
    if (length(dplyr::groups(weights)) != 1) {
        stop("weights must be grouped by portfolio index.")
    }

    # Weights has 3 columns
    if (ncol(weights) != 3) {
        stop("weights table must have three columns: portfolio index, assets, and weights to map.")
    }

    # Data has one group
    if (length(dplyr::groups(data)) != 1) {
        stop("data must be grouped by portfolio index.")
    }

    # Groups for data and weights match
    if (colnames(data)[[1]] != colnames(weights)[[1]]) {
        stop("First column (portfolio index) of data must match first column (portfolio index) of weights.")
    }

}


