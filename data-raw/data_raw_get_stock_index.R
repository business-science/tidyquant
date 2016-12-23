# Script to download latest stock indexes to R/sysdata.rda
# Data is used as a fail-safe if cannot be retrieved from
# www.marketvolume.com

re_run_fallback <- function() {

    index.option <- tq_get("options", get = "stock.index")
    group <- seq_along(index.option)
    date.downloaded <- rep(as.character(Sys.Date()), length.out = length(index.option))

    # Function to collect stock indexes from web
    get_stock_indexes <- function(group, index.option, date.downloaded) {
        stock_indexes <- tibble::tibble(
            group,
            index.option,
            date.downloaded
        ) %>%
            dplyr::mutate(index.components =
                              purrr::map(index.option,
                                         ~ tq_get(.x,
                                                  get = "stock.index",
                                                  use_fallback = FALSE))) %>%
            dplyr::select(group, index.option, index.components, date.downloaded)
        stock_indexes
    }

    if (!exists("stock_indexes")) {

        # if no stock_indexes, build the first instance
        stock_indexes <- get_stock_indexes(group, index.option, date.downloaded)
        devtools::use_data(stock_indexes, internal = TRUE)

    } else {

        # Update stock_indexes with new data that can be downloaded

        stock_indexes_old <- stock_indexes
        names(stock_indexes_old) <- stringr::str_c(names(stock_indexes), ".old", sep = "")

        stock_indexes_new <- get_stock_indexes(group, index.option, date.downloaded)

        stock_indexes_comp <- dplyr::bind_cols(stock_indexes_new, stock_indexes_old) %>%
            dplyr::mutate(len = purrr::map_int(index.components, length)) %>%
            dplyr::mutate(index.components = ifelse(len != 1, index.components, index.components.old),
                          date.downloaded = ifelse(len != 1, date.downloaded, date.downloaded.old))

        stock_indexes <- stock_indexes_comp %>%
            dplyr::select(group:date.downloaded)
        devtools::use_data(stock_indexes, internal = TRUE, overwrite = TRUE)

    }

}

