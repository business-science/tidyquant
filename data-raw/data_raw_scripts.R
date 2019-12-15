
# Functions ----

# Function to download latest stock indexes to R/sysdata.rda
# Data is used as a fail-safe if cannot be retrieved from www.marketvolume.com
re_run_fallback <- function() {

    index.option <- tq_index_options()
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
                                         ~ tq_index(.x, use_fallback = FALSE))) %>%
            dplyr::select(group, index.option, index.components, date.downloaded)
        stock_indexes
    }

    if (!exists("stock_indexes")) {

        # if no stock_indexes, build the first instance

        stock_indexes <- get_stock_indexes(group, index.option, date.downloaded)

    } else {

        # Update stock_indexes with new data that can be downloaded

        stock_indexes_old <- stock_indexes
        names(stock_indexes_old) <- stringr::str_c(names(stock_indexes), ".old", sep = "")

        stock_indexes_new <- get_stock_indexes(group, index.option, date.downloaded)

        # Compare new data to old. If new data could not be retrieved, use old
        stock_indexes_comp <- dplyr::bind_cols(stock_indexes_new, stock_indexes_old) %>%
            dplyr::mutate(len = purrr::map_int(index.components, length)) %>%
            dplyr::mutate(index.components = ifelse(len != 1, index.components, index.components.old),
                          date.downloaded = ifelse(len != 1, date.downloaded, date.downloaded.old))

        stock_indexes <- stock_indexes_comp %>%
            dplyr::select(group:date.downloaded)

    }

    stock_indexes

}

# Function to get yahoo key statistic codes
run_yahoo_finance_tags <- function() {
    require(readxl)
    yahoo_tags <- readxl::read_excel("../yahoo.key.statistics.xlsx") %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(as.character)
    yahoo_tags
}

# Script ----

stock_indexes <- re_run_fallback()
yahoo_tags <- run_yahoo_finance_tags()

usethis::use_data(stock_indexes, yahoo_tags, internal = TRUE, overwrite = TRUE)
