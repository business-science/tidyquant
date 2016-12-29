# GENERAL UTILITY FUNCTIONS

find_date_cols <- function(x) {

    # Functions
    is_date_class <- function(x) inherits(x, 'Date')

    is_char_date <- function(col) {
        check_na <- col %>%
            as.character() %>%
            #as.Date(format = "%y/%m/%d") %>%
            lubridate::ymd() %>%
            is.na()
        !all(check_na) # check for any na's
    }

    # Check for date class
    ret <- suppressWarnings(sapply(x, function(x) is_date_class(x)))

    # If none, check for character columns that can be converted
    if (sum(ret) == 0) {

        ret <- suppressWarnings(sapply(x, is_char_date))

    }

    ret

}

find_date_time_cols <- function(x) {

    # Functions
    is_datetime_class <- function(x) inherits(x, 'POSIXct')

    is_char_datetime <- function(col) {
        check_na <- col %>%
            as.character() %>%
            #as.Date(format = "%y/%m/%d") %>%
            lubridate::ymd_hms() %>%
            is.na()
        !all(check_na) # check for any na's
    }

    # Check for date class
    ret <- suppressWarnings(sapply(x, function(x) is_datetime_class(x)))

    # If none, check for character columns that can be converted
    if (sum(ret) == 0) {

        ret <- suppressWarnings(sapply(x, is_char_datetime))

    }

    ret

}

# Takes a dateframe and returns name of date / date-time column
get_col_name_date_or_date_time <- function(data) {
    date_cols <- find_date_cols(data)
    date_cols <- date_cols[date_cols == TRUE]
    if (length(date_cols) == 0) {
        date_cols <- find_date_time_cols(data)
        date_cols <- date_cols[date_cols == TRUE]
        if (length(date_cols) == 0) {
            stop("No date or POSIXct column found in `data`.")
        }
    }
    date_col_name <- names(date_cols)[[1]]
    date_col_name
}

# Detects date / date-time and converts
convert_date_cols <- function(data) {

    date_cols <- find_date_cols(data)
    date_cols <- date_cols[date_cols == TRUE]

    date_time_cols <- find_date_time_cols(data)
    date_time_cols <- date_time_cols[date_time_cols == TRUE]

    # Convert date column to date
    # TODO: This function needs to be reworked to improve handling
    if (length(date_cols) > 0) {
        ret <- dplyr::mutate(data, date = lubridate::ymd(date))
    } else if (length(date_time_cols) > 0) {
        ret <- dplyr::mutate(data, date = lubridate::ymd_hms(date))
    } else {
        ret <- data
    }

    ret
}
