#' Excel Date and Time Functions
#'
#' @description
#' 50+ date and time functions familiar to users coming from an __Excel Background.__
#' The main benefits are:
#'
#' 1. Integration of the amazing `lubridate` package for handling dates and times
#' 2. Integration of Holidays from `timeDate` and Business Calendars
#' 3. New Date Math and Date Sequence Functions that factor in Business Calendars (e.g. [EOMONTH()], [NET_WORKDAYS()])
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names in most cases match Excel function names
#' - Functionality replicates Excel
#' - By default, missing values are ignored (same as in Excel)
#'
#' @details
#'
#' __Converters__ - Make date and date-time from text (character data)
#' * General String-to-Date Conversion: [AS_DATE()], [AS_DATETIME()]
#' * Format-Specific String-to-Date Conversion: [YMD()] (YYYY-MM-DD), [MDY()] (MM-DD-YYYY), [DMY()] (DD-MM-YYYY)
#' * Hour-Minute-Second Conversion: [YMD_HMS()], [YMD_HM()], and friends.
#'
#' __Extractors__ - Returns information from a time-stamp.
#' * Extractors: [SECOND()], [MINUTE()], [HOUR()], [DAY()], [WEEK()], [MONTH()], [QUARTER()], [YEAR()]
#'
#' __Current Time__ - Returns the current date/date-time based on your locale.
#' * [NOW()], [TODAY()]
#'
#' __Date Math__ - Perform popular Excel date calculations
#' * [EOMONTH()] - End of Month
#' * [NET_WORKDAYS()], [COUNT_DAYS()] - Return number of days between 2 dates factoring in working days and holidays
#' * [YEARFRAC()] - Return the fractional period of the year that has been completed between 2 dates.
#'
#' __Date Sequences__ - Return a vector of dates or a Holiday Table (`tibble`).
#' * [DATE_SEQUENCE()], [WORKDAY_SEQUENCE()], [HOLIDAY_SEQUENCE] - Return a sequence of dates between 2 dates that
#' factor in workdays and `timeDate` holiday calendars for popular business calendars including NYSE and London stock exchange.
#'
#' __Date Collapsers__ - Collapse a date sequence (useful in `dplyr::group_by()` and [pivot_table()])
#' * [FLOOR_DATE()], [FLOOR_DAY()], [FLOOR_WEEK()], [FLOOR_MONTH()], [FLOOR_QUARTER()], [FLOOR_YEAR()]
#' * Similar functions exist for CEILING and ROUND. These are wrappers for `lubridate` functions.
#'
#' @param x A vector of date or date-time objects
#' @param ... Parameters passed to underlying `lubridate` functions.
#' @param label A logical used for [MONTH()] and [WEEKDAY()] Date Extractors to decide whether or not to return names
#' (as ordered factors) or numeric values.
#' @param abbr A logical used for [MONTH()] and [WEEKDAY()]. If `label = TRUE`, used to determine if
#' full names (e.g. Wednesday) or abbreviated names (e.g. Wed) should be returned.
#' @param include_year A logicial value used in [QUARTER()]. Determines whether or not to return 2020 Q3 as `3` or `2020.3`.
#' @param fiscal_start A numeric value used in [QUARTER()]. Determines the fiscal-year starting quarter.
#' @param by Used to determine the gap in Date Sequence calculations and value to round to in Date Collapsing operations.
#' Acceptable values are: A character string, containing one of `"day"`, `"week"`, `"month"`, `"quarter"` or `"year"`.
#' @param start_date Used in Date Math and Date Sequence operations. The starting date in the calculation.
#' @param end_date Used in Date Math and Date Sequence operations. The ending date in the calculation.
#' @param remove_weekends A logical value used in Date Sequence and Date Math calculations.
#' Indicates whether or not weekends should be removed from the calculation.
#' @param holidays A vector of dates corresponding to holidays that should be removed from the calculation.
#' @param calendar The calendar to be used in Date Sequence calculations for Holidays from the `timeDate` package.
#' Acceptable values are: `"NYSE"`, `"LONDON"`, `"NERC"`, `"TSX"`, `"ZURICH"`
#' @param pattern Used to filter Holidays (e.g. `pattern = "Easter"`). A "regular expression" filtering pattern.
#' @param year Used in [DATE()]
#' @param month Used in [DATE()]
#' @param day Used in [DATE()]
#' @param months Used to offset months in [EOMONTH()] AND [EDATE()] Date Math calculations
#' @param years A numeric vector of years to return Holidays for in [HOLIDAY_TABLE()]
#'
#'
#'
#' @return
#' - __Converters__ - Date or date-time object the length of x
#' - __Extractors__ - Returns information from a time-stamp.
#' - __Current Time__ - Returns the current date/date-time based on your locale.
#' - __Date Math__ - Numeric values or Date Values depending on the calculation.
#' - __Date Sequences__ - Return a vector of dates or a Holiday Table (`tibble`).
#' - __Date Collapsers__ - Date or date-time object the length of x
#'
#'
#'
#' @examples
#' # Libraries
#' library(tidyquant)
#' library(tidyverse)
#' library(lubridate)
#'
#' # --- Basic Usage ----
#'
#' # Converters ---
#' AS_DATE("2011 Jan-01") # General
#' YMD("2011 Jan-01")     # Year, Month-Day Format
#' MDY("01-02-20")        # Month-Day, Year Format (January 2nd, 2020)
#' DMY("01-02-20")        # Day-Month, Year Format (February 1st, 2020)
#'
#' # Extractors ---
#' WEEKDAY("2020-01-01")                                  # Labelled Day
#' WEEKDAY("2020-01-01", label = FALSE)                   # Numeric Day
#' WEEKDAY("2020-01-01", label = FALSE, week_start = 1)   # Start at 1 (Monday) vs 7 (Sunday)
#' MONTH("2020-01-01")
#' QUARTER("2020-01-01")
#' YEAR("2020-01-01")
#'
#' # Current Date-Time ---
#' NOW()
#' TODAY()
#'
#' # Date Math ---
#' EOMONTH("2020-01-01")
#' EOMONTH("2020-01-01", months = 1)
#' NET_WORKDAYS("2020-01-01", "2020-07-01") # 131 Skipping Weekends
#' NET_WORKDAYS("2020-01-01", "2020-07-01",
#'              holidays = HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01",
#'                                          calendar = "NYSE")) # 126 Skipping 5 NYSE Holidays
#'
#' # Date Sequences ---
#' DATE_SEQUENCE("2020-01-01", "2020-07-01")
#' WORKDAY_SEQUENCE("2020-01-01", "2020-07-01")
#' HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01", calendar = "NYSE")
#' WORKDAY_SEQUENCE("2020-01-01", "2020-07-01",
#'                  holidays = HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01",
#'                                              calendar = "NYSE"))
#'
#' # Date Collapsers ---
#' FLOOR_DATE(AS_DATE("2020-01-15"), by = "month")
#' CEILING_DATE(AS_DATE("2020-01-15"), by = "month")
#' CEILING_DATE(AS_DATE("2020-01-15"), by = "month") - ddays(1) # EOMONTH using lubridate
#'
#' # --- Usage with tidyverse ---
#'
#' # Calculate returns by symbol/year/quarter
#' FANG %>%
#'     pivot_table(
#'         .rows       = c(symbol, ~ QUARTER(date)),
#'         .columns    = ~ YEAR(date),
#'         .values     = ~ PCT_CHANGE_FIRSTLAST(adjusted)
#'     )
#'
#' @name excel_date_functions


# CONVERTERS ----

#' @rdname excel_date_functions
#' @export
AS_DATE <- function(x, ...) {
    lubridate::as_date(x, ...)
}

#' @rdname excel_date_functions
#' @export
AS_DATETIME <- function(x, ...) {
    lubridate::as_datetime(x, ...)
}

#' @rdname excel_date_functions
#' @export
DATE <- function(year, month, day) {
    lubridate::ymd(stringr::str_c(year, month, day, sep = "-"))
}

#' @rdname excel_date_functions
#' @export
DATEVALUE <- AS_DATE

#' @rdname excel_date_functions
#' @export
YMD <- function(x, ...) {
    lubridate::ymd(x, ...)
}

#' @rdname excel_date_functions
#' @export
MDY <- function(x, ...) {
    lubridate::mdy(x, ...)
}

#' @rdname excel_date_functions
#' @export
DMY <- function(x, ...) {
    lubridate::dmy(x, ...)
}

#' @rdname excel_date_functions
#' @export
YMD_HMS <- function(x, ...) {
    lubridate::ymd_hms(x, ...)
}

#' @rdname excel_date_functions
#' @export
MDY_HMS <- function(x, ...) {
    lubridate::mdy_hms(x, ...)
}

#' @rdname excel_date_functions
#' @export
DMY_HMS <- function(x, ...) {
    lubridate::dmy_hms(x, ...)
}

#' @rdname excel_date_functions
#' @export
YMD_HM <- function(x, ...) {
    lubridate::ymd_hm(x, ...)
}

#' @rdname excel_date_functions
#' @export
MDY_HM <- function(x, ...) {
    lubridate::mdy_hm(x, ...)
}

#' @rdname excel_date_functions
#' @export
DMY_HM <- function(x, ...) {
    lubridate::dmy_hm(x, ...)
}

#' @rdname excel_date_functions
#' @export
YMD_H <- function(x, ...) {
    lubridate::ymd_h(x, ...)
}

#' @rdname excel_date_functions
#' @export
MDY_H <- function(x, ...) {
    lubridate::mdy_h(x, ...)
}

#' @rdname excel_date_functions
#' @export
DMY_H <- function(x, ...) {
    lubridate::dmy_h(x, ...)
}



# EXTRACTORS ----

#' @rdname excel_date_functions
#' @export
WEEKDAY <- function(x, ..., label = FALSE, abbr = TRUE) {
    lubridate::wday(x, ..., label = label, abbr = abbr)
}

#' @rdname excel_date_functions
#' @export
WDAY <- WEEKDAY

#' @rdname excel_date_functions
#' @export
DOW <- WEEKDAY

#' @rdname excel_date_functions
#' @export
MONTHDAY <- function(x, ...) {
    lubridate::mday(x, ...)
}

#' @rdname excel_date_functions
#' @export
MDAY <- MONTHDAY

#' @rdname excel_date_functions
#' @export
DOM <- MONTHDAY

#' @rdname excel_date_functions
#' @export
QUARTERDAY <- function(x, ...) {
    lubridate::qday(x, ...)
}

#' @rdname excel_date_functions
#' @export
QDAY <- QUARTERDAY

#' @rdname excel_date_functions
#' @export
DAY <- function(x, ...) {
    lubridate::day(x, ...)
}

#' @rdname excel_date_functions
#' @export
WEEKNUM <- function(x, ...) {
    lubridate::week(x, ...)
}

#' @rdname excel_date_functions
#' @export
WEEK <- WEEKNUM

#' @rdname excel_date_functions
#' @export
WEEKNUM_ISO <- function(x, ...) {
    lubridate::isoweek(x, ...)
}

#' @rdname excel_date_functions
#' @export
MONTH <- function(x, ..., label = FALSE, abbr = TRUE) {
    lubridate::month(x, ..., label = label, abbr = abbr)
}

#' @rdname excel_date_functions
#' @export
QUARTER <- function(x, ..., include_year = FALSE, fiscal_start = 1) {
    lubridate::quarter(x, ..., with_year = include_year, fiscal_start = fiscal_start)
}

#' @rdname excel_date_functions
#' @export
YEAR <- function(x, ...) {
    lubridate::year(x, ...)
}

#' @rdname excel_date_functions
#' @export
YEAR_ISO <- function(x, ...) {
    lubridate::isoyear(x, ...)
}

#' @rdname excel_date_functions
#' @export
DATE_TO_NUMERIC <- function(x, ...) {
    as.numeric(as.POSIXct(x, ...))
}

#' @rdname excel_date_functions
#' @export
DATE_TO_DECIMAL <- function(x, ...) {
    lubridate::decimal_date(x, ...)
}

#' @rdname excel_date_functions
#' @export
SECOND <- function(x, ...) {
    lubridate::second(x, ...)
}

#' @rdname excel_date_functions
#' @export
MINUTE <- function(x, ...) {
    lubridate::minute(x, ...)
}

#' @rdname excel_date_functions
#' @export
HOUR <- function(x, ...) {
    lubridate::hour(x, ...)
}

# CURRENT DATE-TIME ----
#' @rdname excel_date_functions
#' @export
NOW <- function(...) {
    lubridate::now(...)
}

#' @rdname excel_date_functions
#' @export
TODAY <- function(...) {
    lubridate::today(...)
}


# DATE MATH ----
#' @rdname excel_date_functions
#' @export
EOMONTH <- function(start_date, months = 0) {

    if (rlang::is_missing(start_date)) start_date <- TODAY()

    start_date <- lubridate::as_date(start_date)

    lubridate::month(start_date) <- lubridate::month(start_date) + months

    lubridate::ceiling_date(start_date, unit = "month") - lubridate::ddays(1)
}

#' @rdname excel_date_functions
#' @export
EDATE <- function(start_date, months = 0) {

    if (rlang::is_missing(start_date)) start_date <- TODAY()

    start_date <- AS_DATE(start_date)

    lubridate::month(start_date) <- lubridate::month(start_date) + months

    start_date
}

#' @rdname excel_date_functions
#' @export
NET_WORKDAYS <- function(start_date, end_date, remove_weekends = TRUE, holidays = NULL) {
    start_date <- AS_DATE(start_date)
    end_date   <- AS_DATE(end_date)

    WORKDAY_SEQUENCE(start_date = start_date, end_date = end_date,
                     remove_weekends = remove_weekends, holidays = holidays) %>%
        COUNT()
}

#' @rdname excel_date_functions
#' @export
COUNT_DAYS <- function(start_date, end_date) {
    start_date <- AS_DATE(start_date)
    end_date   <- AS_DATE(end_date)

    DATE_SEQUENCE(start_date = start_date, end_date = end_date, by = "day") %>% COUNT()
}

#' @rdname excel_date_functions
#' @export
YEARFRAC <- function(start_date, end_date) {
    start_date <- AS_DATE(start_date)
    end_date   <- AS_DATE(end_date)

    partial_year <- DATE_SEQUENCE(start_date = start_date, end_date = end_date, by = "day") %>% COUNT()
    full_year    <- DATE_SEQUENCE(start_date = start_date,
                                  end_date   = lubridate::ceiling_date(end_date, unit = "year") - lubridate::ddays(1),
                                  by         = "day") %>%
        COUNT()

    (partial_year - 1) / full_year
}

# DATE SEQUENCES AND HOLIDAYS ----
#' @rdname excel_date_functions
#' @export
DATE_SEQUENCE <- function(start_date, end_date, by = "day") {
    seq.Date(from = AS_DATE(start_date),
             to   = AS_DATE(end_date),
             by   = by)
}

#' @rdname excel_date_functions
#' @export
WORKDAY_SEQUENCE <- function(start_date, end_date, remove_weekends = TRUE, holidays = NULL) {

    day_sequence <-  DATE_SEQUENCE(start_date, end_date, by = "day")

    ret_tbl <- tibble::tibble(day_sequence = day_sequence) %>%
        dplyr::mutate(weekday = WEEKDAY(day_sequence, label = TRUE))

    if (remove_weekends) {
        ret_tbl <- ret_tbl %>%
            dplyr::filter(!(weekday == "Sat" | weekday == "Sun"))
    }

    if (!is.null(holidays)) {
        if (!is.Date(holidays)) stop("WORKDAY_SEQUENCE(): holidays must be a date sequence (vector of dates).", call. = FALSE)
        ret_tbl <- ret_tbl %>%
            dplyr::filter(!(day_sequence %in% holidays))
    }

    ret_tbl %>% dplyr::pull(day_sequence)

}

#' @rdname excel_date_functions
#' @export
HOLIDAY_SEQUENCE <- function(start_date, end_date,
                             calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH")) {
    fun <- switch(
        tolower(calendar[1]),
        "nyse"     = timeDate::holidayNYSE,
        "london"   = timeDate::holidayLONDON,
        "nerc"     = timeDate::holidayNERC,
        "tsx"      = timeDate::holidayTSX,
        "zurich"   = timeDate::holidayZURICH
    )

    date_seq <- DATE_SEQUENCE(start_date, end_date)
    years    <- date_seq %>% YEAR() %>% unique()
    holidays <- fun(year = years) %>% AS_DATE()

    return(holidays[holidays %in% date_seq])
}

#' @rdname excel_date_functions
#' @export
HOLIDAY_TABLE <- function(years, pattern = ".") {

    if (rlang::is_missing(years)) years = YEAR(TODAY())

    tibble::tibble(holidays = timeDate::listHolidays(pattern = pattern)) %>%
        dplyr::mutate(date = purrr::map(holidays, .f = function(holiday) {
            timeDate::holiday(years, Holiday = holiday) %>% AS_DATE()
        })
        ) %>%
        tidyr::unnest(date) %>%
        dplyr::mutate(year = YEAR(date))
}



# DATE COLLAPSERS ----

#' @rdname excel_date_functions
#' @export
FLOOR_DATE <- function(x, ..., by = "day") {
    lubridate::floor_date(x, ..., unit = by)
}

#' @rdname excel_date_functions
#' @export
FLOOR_DAY <- function(x, ...) {
    lubridate::floor_date(x, ..., unit = "day")
}

#' @rdname excel_date_functions
#' @export
FLOOR_WEEK<- function(x, ...) {
    lubridate::floor_date(x, ..., unit = "week")
}

#' @rdname excel_date_functions
#' @export
FLOOR_MONTH <- function(x, ...) {
    lubridate::floor_date(x, ..., unit = "month")
}

#' @rdname excel_date_functions
#' @export
FLOOR_QUARTER <- function(x, ...) {
    lubridate::floor_date(x, ..., unit = "quarter")
}

#' @rdname excel_date_functions
#' @export
FLOOR_YEAR <- function(x, ...) {
    lubridate::floor_date(x, ..., unit = "year")
}

#' @rdname excel_date_functions
#' @export
CEILING_DATE <- function(x, ..., by = "day") {
    lubridate::ceiling_date(x, ..., unit = by)
}

#' @rdname excel_date_functions
#' @export
CEILING_DAY <- function(x, ...) {
    lubridate::ceiling_date(x, ..., unit = "day")
}

#' @rdname excel_date_functions
#' @export
CEILING_WEEK <- function(x, ...) {
    lubridate::ceiling_date(x, ..., unit = "week")
}

#' @rdname excel_date_functions
#' @export
CEILING_MONTH <- function(x, ...) {
    lubridate::ceiling_date(x, ..., unit = "month")
}

#' @rdname excel_date_functions
#' @export
CEILING_QUARTER <- function(x, ...) {
    lubridate::ceiling_date(x, ..., unit = "quarter")
}

#' @rdname excel_date_functions
#' @export
CEILING_YEAR <- function(x, ...) {
    lubridate::ceiling_date(x, ..., unit = "year")
}

#' @rdname excel_date_functions
#' @export
ROUND_DATE <- function(x, ..., by = "day") {
    lubridate::round_date(x, ..., unit = by)
}

#' @rdname excel_date_functions
#' @export
ROUND_DAY <- function(x, ...) {
    lubridate::round_date(x, ..., unit = "day")
}

#' @rdname excel_date_functions
#' @export
ROUND_WEEK <- function(x, ...) {
    lubridate::round_date(x, ..., unit = "week")
}

#' @rdname excel_date_functions
#' @export
ROUND_MONTH <- function(x, ...) {
    lubridate::round_date(x, ..., unit = "month")
}

#' @rdname excel_date_functions
#' @export
ROUND_QUARTER <- function(x, ...) {
    lubridate::round_date(x, ..., unit = "quarter")
}

#' @rdname excel_date_functions
#' @export
ROUND_YEAR <- function(x, ...) {
    lubridate::round_date(x, ..., unit = "year")
}

