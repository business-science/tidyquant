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
    ret <- sapply(x, function(x) is_date_class(x))

    # If none, check for character columns that can be converted
    if (sum(ret) == 0) {

        ret <- sapply(x, is_char_date)

    }

    ret

}
