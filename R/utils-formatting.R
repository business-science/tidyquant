# GENERAL UTILITY FUNCTIONS FOR FORMATTING NUMBERS

convert_to_numeric <- function(x) {

    # Error handling
    if (is.na(x) || is.null(x)) {
        return(NA)
    }

    # x = character such as "23.4B"
    units <- stringr::str_sub(x, -1, -1)

    if (units %in% c("T", "B", "M")) {
        value <- stringr::str_sub(x, 1, -2) %>%
            as.numeric()

        if (units == "T") {
            value <- value * 1e12
        } else if (units == "B") {
            value <- value * 1e9
        } else if (units == "M") {
            value <- value * 1e6
        }
    } else (
        value <- as.numeric(value)
    )

    return(value)
}

convert_to_percent <- function(x) {

    # Error handling
    if (is.na(x) || is.null(x)) {
        return(NA)
    }

    # x = character such as "-0.6104%"
    units <- stringr::str_sub(x, -1, -1)

    if (units %in% c("%")) {
        value <- stringr::str_sub(x, 1, -2) %>%
            as.numeric()

        value <- value * 1 / 100
    }

    return(value)
}
