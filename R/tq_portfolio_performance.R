#' Calculate portfolio level metrics that depend on component level interactions
#'
#' Calculations like portfolio standard deviation and value at risk are influenced
#' by the individual assets that make up the portfolio through their correlation
#' with each other. `tq_portfolio_performance()` allows the user to take those
#' correlations into account.
#'
#' @inheritParams tq_portfolio
#' @param weights Asset weights, which can be passed as a numeric vector the
#' length of the number of assets or a two column tibble with asset names in
#' first column and weights in second column. With 1 portfolio, this is optional,
#' with multiple portfolios, this is required.
#' @param performance_fun A portfolio performance metric function. See
#' `tq_portfolio_performance_fun_options()` for available options.
#' @param ... Additional parameters passed to the appropriate
#' `PerformanceAnalytics` function.
#'
#' @details
#'
#' `tq_portfolio_performance()` is a wrapper for `PerformanceAnalytics` functions
#' that calculate metrics taking into account the individual correlations
#' between assets. These functions are `PerformanceAnalytics::VaR()` (Value at Risk),
#' `PerformanceAnalytics::StdDev()` (Standard Deviation),
#' and `PerformanceAnalytics::ES()` (Expected Shortfall).
#'
#' Each of the three functions listed above take a number of arguments that
#' change the output and affect how the calculations are performed. Most important
#' are the `method` and `portfolio_method` arguments which can be passed
#' through the `...` argument. Read the documentation for each function to
#' know what methods are supported.
#'
#' For multiple portfolios, [tq_repeat_df()] is a helper function to put
#' your weight and asset return tibbles into the correct format. See the
#' examples for details.
#'
#' `assets_col` and `returns_col` are columns within `data` that are used
#' to compute returns for a portfolio. The columns should be in "long" format (or "tidy" format)
#' meaning there is only one column containing all of the assets and one column containing
#' all of the return values (i.e. not in "wide" format with returns spread by asset).
#'
#' `weights` are the weights to be applied to the asset returns.
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
#' @return
#'
#' Returns a summary `tibble` of the portfolio metric results.
#'
#' @seealso
#'
#' [tq_repeat_df()], and [tq_portfolio()]
#'
#' @examples
#'
#' # Load libraries
#' library(tidyquant)
#'
#' # Use FANG data set
#' data(FANG)
#'
#' # Get returns for individual stock components
#' monthly_returns_stocks <- FANG %>%
#'     group_by(symbol) %>%
#'     tq_transmute(adjusted, periodReturn, period = "monthly")
#'
#' ##### Portfolio Aggregation Methods #####
#'
#' # Method 1: Use tq_portfolio_performance with numeric vector of weights
#'
#' weights <- c(0.50, 0.25, 0.25, 0)
#'
#' # Calculating portfolio standard deviation
#' tq_portfolio_performance(data = monthly_returns_stocks,
#'                          assets_col = symbol,
#'                          returns_col = monthly.returns,
#'                          weights = weights,
#'                          performance_fun = StdDev)
#'
#' # Calculating portfolio standard deviation and asking for
#' # component level risk attribution
#' tq_portfolio_performance(data = monthly_returns_stocks,
#'                          assets_col = symbol,
#'                          returns_col = monthly.returns,
#'                          weights = weights,
#'                          performance_fun = StdDev,
#'                          portfolio_method = "component")
#'
#' # If no weights are specified, the default is to calculate the
#' # standard deviation of each asset separately
#' tq_portfolio_performance(data = monthly_returns_stocks,
#'                          assets_col = symbol,
#'                          returns_col = monthly.returns,
#'                          performance_fun = StdDev)
#'
#' # Method 2: Use tq_portfolio with two column tibble and map weights
#'
#' # Note that GOOG's weighting is zero in Method 1. In Method 2,
#' # GOOG is not added and same result is achieved.
#' weights_df <- tibble(symbol = c("FB", "AMZN", "NFLX"),
#'                      weights = c(0.50, 0.25, 0.25))
#'
#' # Calculate portfolio value at risk with risk contribution
#' tq_portfolio_performance(data = monthly_returns_stocks,
#'                          assets_col = symbol,
#'                          returns_col = monthly.returns,
#'                          weights = weights_df,
#'                          performance_fun = VaR,
#'                          portfolio_method = "component")
#'
#' # Unlike StdDev, VaR will produce an error if you attempt to
#' # calculate portfolio VaR with weights, but without specifying
#' # portfolio_method = "component". The default portfolio_method = "single"
#' # which would be used below fails in this case.
#' # tq_portfolio_performance(data = monthly_returns_stocks,
#' #              assets_col = symbol,
#' #              returns_col = monthly.returns,
#' #              weights = weights_df,
#' #              performance_fun = VaR)
#'
#' # Method 3: Working with multiple portfolios
#'
#' # 3A: Duplicate monthly_returns_stocks multiple times
#' mult_monthly_returns_stocks <- tq_repeat_df(monthly_returns_stocks, n = 4)
#'
#' # 3B: Create weights table grouped by portfolio id
#' weights <- c(0.50, 0.25, 0.25, 0.00,
#'              0.00, 0.50, 0.25, 0.25,
#'              0.25, 0.00, 0.50, 0.25,
#'              0.25, 0.25, 0.00, 0.50)
#' stocks <- c("FB", "AMZN", "NFLX", "GOOG")
#' weights_table <- tibble(stocks) %>%
#'     tq_repeat_df(n = 4) %>%
#'     bind_cols(tibble(weights)) %>%
#'     group_by(portfolio)
#'
#' # 3C: Scale to multiple portfolios
#'
#' # Expected Shortfall with risk attribution for each of the 4
#' # portfolios
#' tq_portfolio_performance(data = mult_monthly_returns_stocks,
#'                          assets_col = symbol,
#'                          returns_col = monthly.returns,
#'                          weights = weights_table,
#'                          performance_fun = ES,
#'                          portfolio_method = "component")
#'
#' # Like the VaR example, you must specify portfolio_method = "component"
#' # when supplying weights.
#'
#' # You must also always supply weights when using multiple portfolios
#'
#' @export
tq_portfolio_performance <- function(data, assets_col, returns_col, performance_fun, weights = NULL, ...) {
    UseMethod("tq_portfolio_performance")
}

#' @export
#'
tq_portfolio_performance.tbl_df <- function(data, assets_col, returns_col, performance_fun, weights = NULL, ...) {

    assets_col      <- rlang::enexpr(assets_col)
    returns_col     <- rlang::enexpr(returns_col)
    performance_fun <- rlang::enexpr(performance_fun)
    .dots           <- rlang::dots_list(...)

    # Check performance function
    check_portfolio_performance_fun_options(performance_fun)

    # Check returns_col and assets_col
    check_x_y_valid(data, as.character(returns_col), as.character(assets_col))

    # Capture portfolio_method option
    portfolio_method <- ifelse("portfolio_method" %in% names(.dots),
                               .dots[["portfolio_method"]],
                               "single")

    # Capture method option
    method <- ifelse("method" %in% names(.dots),
                     .dots[["method"]],
                     "none")

    # Apply the portfolio function
    result <- apply_portfolio_metric(data,
                                     assets_col = as.character(assets_col),
                                     returns_col = as.character(returns_col),
                                     weights = weights,
                                     performance_fun = !! performance_fun,
                                     ... = ...)

    # Coerce to data frame (not straight to tibble, may need row names)
    result <- result %>%
        as.data.frame()

    # Switch based on portfolio method
    if(portfolio_method == "single" | portfolio_method == "marginal") {

        result <- result %>%
            tidyr::gather(key = "asset", value = !! performance_fun) %>%
            tibble::as_tibble()

        # With single + weights specified + StdDev, 1 value is returned
        # for portfolio stddev. Add asset name
        if(!is.null(weights)) {
            result[["asset"]][[1]] <- "portfolio"
        }

    } else if(portfolio_method == "component") {

        result <- result %>%
            tibble::rownames_to_column(var = "asset") %>%
            tibble::as_tibble()

        # Strangely for historical + component + VaR, only 1 value is returned
        # without any rownames
        if(method == "historical") {
            result[["asset"]][[1]] <- "portfolio"
            colnames(result)[2] <- as.character(performance_fun)
        }

    }

    # Takes care of calling component with 1 asset. No rownames returned
    if(ncol(data) == 1) {
        result[["asset"]][[1]] <- colnames(data)
    }

    result
}

#' @export
#'
tq_portfolio_performance.grouped_df <- function(data, assets_col, returns_col, performance_fun, weights = NULL, ...) {

    assets_col      <- rlang::enexpr(assets_col)
    returns_col     <- rlang::enexpr(returns_col)
    performance_fun <- rlang::enexpr(performance_fun)

    # Single portfolio. Ungroup and use tq_portfolio_performance.tbl_df()
    if(colnames(data)[[1]] != "portfolio") {

        # Ungroup tibble
        data <- dplyr::ungroup(data)

        # Run for 1 portfolio
        ret <- tq_portfolio_performance(data            = data,
                                        assets_col      = !! assets_col,
                                        returns_col     = !! returns_col,
                                        performance_fun = !! performance_fun,
                                        weights         = weights,
                                        ...             = ...)

        # Return results
        return(ret)
    }

    # Otherwise, multiple portfolios
    check_data_weights_compatibility(data, weights)

    # Get groups
    group_names_data <- dplyr::groups(data)

    # Format data
    data_nested <- data %>%
        tidyr::nest() %>%
        dplyr::rename(returns.. = data)

    # Format weights
    weights_nested <- weights %>%
        tidyr::nest() %>%
        dplyr::rename(weights.. = data)

    # Join data and weights
    y <- names(data_nested)[[1]]
    x <- names(weights_nested)[[1]]
    data_weights <- left_join(data_nested, weights_nested, by = setNames(x, y))

    # Custom function for mapping
    custom_function <- function(x, y) {
        tq_portfolio_performance(data            = x,
                                 assets_col      = !! assets_col,
                                 returns_col     = !! returns_col,
                                 performance_fun = !! performance_fun,
                                 weights         = y,
                                 ...             = ...)
    }

    # Map data and weights to tq_portfolio_.tbl_df()
    data_weights %>%
        dplyr::mutate(
            portfolio.. = purrr::pmap(list(returns.., weights..), custom_function),
            class.. = purrr::map_chr(.x = portfolio.., .f = function(x) class(x)[[1]])
        ) %>%
        dplyr::filter(class.. != "logical") %>%
        dplyr::select(-c(returns.., weights.., class..)) %>%
        tidyr::unnest() %>%
        dplyr::group_by_(.dots = group_names_data)
}

tq_portfolio_performance_fun_options <- function() {
    c(
        "VaR",
        "StdDev",
        "ES",
        "CVaR",
        "ETL"
    )
}

# Utility ---------------------------------------------------------------------------------------------------

check_portfolio_performance_fun_options <- function(fun) {
    fun_options <- tq_portfolio_performance_fun_options() %>%
        unlist()
    if (!(as.character(fun) %in% fun_options)) {
        stop(paste0("fun = ", as.character(fun), " not a valid option."))
    }
}

apply_portfolio_metric <- function(data, assets_col, returns_col, weights, performance_fun, map = FALSE, x = NULL, ...) {

    performance_fun <- rlang::quo_name(rlang::enexpr(performance_fun))
    .dots           <- rlang::dots_list(...)

    # Find date or date-time col
    date_col_name <- get_col_name_date_or_date_time(data)

    # Re-map assets and returns column names
    returns_col_name <- returns_col
    assets_col_name  <- assets_col
    returns_col      <- dplyr::select_(data, returns_col)
    assets_col       <- dplyr::select_(data, assets_col)

    # Apply function
    ret <- tryCatch({
        # Handle weights
        # Don't want the equal weight message from check_weights, let
        # performance_fun return this along with other messages
        suppressMessages(check_weights(weights, assets_col, map, x))

        # Solve issue with spread re-ordering column names alphabetically
        # if (is.data.frame(weights)) weights <- map_weights(weights, assets_col)
        # Note that weights are resorted to match spread reorder to alphabetical
        weights <- map_weights(weights, assets_col)

        # Spread for xts form
        data <- data %>%
            dplyr::select_(date_col_name, assets_col_name, returns_col_name) %>%
            tidyr::spread_(key_col = assets_col_name, value_col = returns_col_name) %>%
            timetk::tk_xts(silent = TRUE)

        # Apply portfolio function. Pass dots through if user specified any
        if(length(.dots) > 0) {
            suppressWarnings(
                do.call(performance_fun, args = c(list(R = data, weights = weights), .dots))
            )
        } else {
            suppressWarnings(
                do.call(performance_fun, args = list(R = data, weights = weights))
            )
        }

    }, error = function(e) {

        warn <- e
        # if (map == TRUE) warn <- paste0(x, " had the following error: ", e, ". Removing portfolio ", x, ".")
        warning(warn)
        # Return NA on error
        return(NA)

    })

    ret
}
