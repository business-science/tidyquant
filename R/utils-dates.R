# GENERAL UTILITY FUNCTIONS FOR DATES

# Checks a data frame and returns true / false if date or date-time column exists
check_date_or_date_time_exists <- function(data) {

    date_cols <- timetk::tk_get_timeseries_variables(data)
    return(length(date_cols) > 0)

}

# Takes a dateframe and returns name of date / date-time column
get_col_name_date_or_date_time <- function(data) {

    date_cols <- timetk::tk_get_timeseries_variables(data)
    if (length(date_cols) == 0) {
        stop("No date or POSIXct column found in `data`.")
    }
    return(date_cols[[1]])

}

get_time_zone <- function(data, date_col_name) {

    date_col_string <- paste0(deparse(substitute(data)), "$", date_col_name)
    eval(parse(text = date_col_string)) %>%
        lubridate::tz()
}


