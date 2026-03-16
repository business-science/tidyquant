# Excel Date and Time Functions

50+ date and time functions familiar to users coming from an **Excel
Background.** The main benefits are:

1.  Integration of the amazing `lubridate` package for handling dates
    and times

2.  Integration of Holidays from `timeDate` and Business Calendars

3.  New Date Math and Date Sequence Functions that factor in Business
    Calendars (e.g. `EOMONTH()`, `NET_WORKDAYS()`)

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names in most cases match Excel function names

- Functionality replicates Excel

- By default, missing values are ignored (same as in Excel)

## Usage

``` r
AS_DATE(x, ...)

AS_DATETIME(x, ...)

DATE(year, month, day)

DATEVALUE(x, ...)

YMD(x, ...)

MDY(x, ...)

DMY(x, ...)

YMD_HMS(x, ...)

MDY_HMS(x, ...)

DMY_HMS(x, ...)

YMD_HM(x, ...)

MDY_HM(x, ...)

DMY_HM(x, ...)

YMD_H(x, ...)

MDY_H(x, ...)

DMY_H(x, ...)

WEEKDAY(x, ..., label = FALSE, abbr = TRUE)

WDAY(x, ..., label = FALSE, abbr = TRUE)

DOW(x, ..., label = FALSE, abbr = TRUE)

MONTHDAY(x, ...)

MDAY(x, ...)

DOM(x, ...)

QUARTERDAY(x, ...)

QDAY(x, ...)

DAY(x, ...)

WEEKNUM(x, ...)

WEEK(x, ...)

WEEKNUM_ISO(x, ...)

MONTH(x, ..., label = FALSE, abbr = TRUE)

QUARTER(x, ..., include_year = FALSE, fiscal_start = 1)

YEAR(x, ...)

YEAR_ISO(x, ...)

DATE_TO_NUMERIC(x, ...)

DATE_TO_DECIMAL(x, ...)

SECOND(x, ...)

MINUTE(x, ...)

HOUR(x, ...)

NOW(...)

TODAY(...)

EOMONTH(start_date, months = 0)

EDATE(start_date, months = 0)

NET_WORKDAYS(start_date, end_date, remove_weekends = TRUE, holidays = NULL)

COUNT_DAYS(start_date, end_date)

YEARFRAC(start_date, end_date)

DATE_SEQUENCE(start_date, end_date, by = "day")

WORKDAY_SEQUENCE(start_date, end_date, remove_weekends = TRUE, holidays = NULL)

HOLIDAY_SEQUENCE(
  start_date,
  end_date,
  calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH")
)

HOLIDAY_TABLE(years, pattern = ".")

FLOOR_DATE(x, ..., by = "day")

FLOOR_DAY(x, ...)

FLOOR_WEEK(x, ...)

FLOOR_MONTH(x, ...)

FLOOR_QUARTER(x, ...)

FLOOR_YEAR(x, ...)

CEILING_DATE(x, ..., by = "day")

CEILING_DAY(x, ...)

CEILING_WEEK(x, ...)

CEILING_MONTH(x, ...)

CEILING_QUARTER(x, ...)

CEILING_YEAR(x, ...)

ROUND_DATE(x, ..., by = "day")

ROUND_DAY(x, ...)

ROUND_WEEK(x, ...)

ROUND_MONTH(x, ...)

ROUND_QUARTER(x, ...)

ROUND_YEAR(x, ...)
```

## Arguments

- x:

  A vector of date or date-time objects

- ...:

  Parameters passed to underlying `lubridate` functions.

- year:

  Used in `DATE()`

- month:

  Used in `DATE()`

- day:

  Used in `DATE()`

- label:

  A logical used for `MONTH()` and `WEEKDAY()` Date Extractors to decide
  whether or not to return names (as ordered factors) or numeric values.

- abbr:

  A logical used for `MONTH()` and `WEEKDAY()`. If `label = TRUE`, used
  to determine if full names (e.g. Wednesday) or abbreviated names (e.g.
  Wed) should be returned.

- include_year:

  A logical value used in `QUARTER()`. Determines whether or not to
  return 2020 Q3 as `3` or `2020.3`.

- fiscal_start:

  A numeric value used in `QUARTER()`. Determines the fiscal-year
  starting quarter.

- start_date:

  Used in Date Math and Date Sequence operations. The starting date in
  the calculation.

- months:

  Used to offset months in `EOMONTH()` AND `EDATE()` Date Math
  calculations

- end_date:

  Used in Date Math and Date Sequence operations. The ending date in the
  calculation.

- remove_weekends:

  A logical value used in Date Sequence and Date Math calculations.
  Indicates whether or not weekends should be removed from the
  calculation.

- holidays:

  A vector of dates corresponding to holidays that should be removed
  from the calculation.

- by:

  Used to determine the gap in Date Sequence calculations and value to
  round to in Date Collapsing operations. Acceptable values are: A
  character string, containing one of `"day"`, `"week"`, `"month"`,
  `"quarter"` or `"year"`.

- calendar:

  The calendar to be used in Date Sequence calculations for Holidays
  from the `timeDate` package. Acceptable values are: `"NYSE"`,
  `"LONDON"`, `"NERC"`, `"TSX"`, `"ZURICH"`

- years:

  A numeric vector of years to return Holidays for in `HOLIDAY_TABLE()`

- pattern:

  Used to filter Holidays (e.g. `pattern = "Easter"`). A "regular
  expression" filtering pattern.

## Value

- **Converters** - Date or date-time object the length of x

- **Extractors** - Returns information from a time-stamp.

- **Current Time** - Returns the current date/date-time based on your
  locale.

- **Date Math** - Numeric values or Date Values depending on the
  calculation.

- **Date Sequences** - Return a vector of dates or a Holiday Table
  (`tibble`).

- **Date Collapsers** - Date or date-time object the length of x

## Details

**Converters** - Make date and date-time from text (character data)

- General String-to-Date Conversion: `AS_DATE()`, `AS_DATETIME()`

- Format-Specific String-to-Date Conversion: `YMD()` (YYYY-MM-DD),
  `MDY()` (MM-DD-YYYY), `DMY()` (DD-MM-YYYY)

- Hour-Minute-Second Conversion: `YMD_HMS()`, `YMD_HM()`, and friends.

**Extractors** - Returns information from a time-stamp.

- Extractors: `SECOND()`, `MINUTE()`, `HOUR()`, `DAY()`, `WEEK()`,
  `MONTH()`, `QUARTER()`, `YEAR()`

**Current Time** - Returns the current date/date-time based on your
locale.

- `NOW()`, `TODAY()`

**Date Math** - Perform popular Excel date calculations

- `EOMONTH()` - End of Month

- `NET_WORKDAYS()`, `COUNT_DAYS()` - Return number of days between 2
  dates factoring in working days and holidays

- `YEARFRAC()` - Return the fractional period of the year that has been
  completed between 2 dates.

**Date Sequences** - Return a vector of dates or a Holiday Table
(`tibble`).

- `DATE_SEQUENCE()`, `WORKDAY_SEQUENCE()`, HOLIDAY_SEQUENCE - Return a
  sequence of dates between 2 dates that factor in workdays and
  `timeDate` holiday calendars for popular business calendars including
  NYSE and London stock exchange.

**Date Collapsers** - Collapse a date sequence (useful in
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`pivot_table()`](https://business-science.github.io/tidyquant/reference/excel_pivot_table.md))

- `FLOOR_DATE()`, `FLOOR_DAY()`, `FLOOR_WEEK()`, `FLOOR_MONTH()`,
  `FLOOR_QUARTER()`, `FLOOR_YEAR()`

- Similar functions exist for CEILING and ROUND. These are wrappers for
  `lubridate` functions.

## Examples

``` r
# Libraries
library(lubridate)
#> 
#> Attaching package: ‘lubridate’
#> The following objects are masked from ‘package:base’:
#> 
#>     date, intersect, setdiff, union

# --- Basic Usage ----

# Converters ---
AS_DATE("2011 Jan-01") # General
#> [1] "2011-01-01"
YMD("2011 Jan-01")     # Year, Month-Day Format
#> [1] "2011-01-01"
MDY("01-02-20")        # Month-Day, Year Format (January 2nd, 2020)
#> [1] "2020-01-02"
DMY("01-02-20")        # Day-Month, Year Format (February 1st, 2020)
#> [1] "2020-02-01"

# Extractors ---
WEEKDAY("2020-01-01")                                  # Labelled Day
#> [1] 4
WEEKDAY("2020-01-01", label = FALSE)                   # Numeric Day
#> [1] 4
WEEKDAY("2020-01-01", label = FALSE, week_start = 1)   # Start at 1 (Monday) vs 7 (Sunday)
#> [1] 3
MONTH("2020-01-01")
#> [1] 1
QUARTER("2020-01-01")
#> [1] 1
YEAR("2020-01-01")
#> [1] 2020

# Current Date-Time ---
NOW()
#> [1] "2026-03-16 18:32:11 UTC"
TODAY()
#> [1] "2026-03-16"

# Date Math ---
EOMONTH("2020-01-01")
#> [1] "2020-01-31"
EOMONTH("2020-01-01", months = 1)
#> [1] "2020-02-29"
NET_WORKDAYS("2020-01-01", "2020-07-01") # 131 Skipping Weekends
#> [1] 131
NET_WORKDAYS("2020-01-01", "2020-07-01",
             holidays = HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01",
                                         calendar = "NYSE")) # 126 Skipping 5 NYSE Holidays
#> [1] 126

# Date Sequences ---
DATE_SEQUENCE("2020-01-01", "2020-07-01")
#>   [1] "2020-01-01" "2020-01-02" "2020-01-03" "2020-01-04" "2020-01-05"
#>   [6] "2020-01-06" "2020-01-07" "2020-01-08" "2020-01-09" "2020-01-10"
#>  [11] "2020-01-11" "2020-01-12" "2020-01-13" "2020-01-14" "2020-01-15"
#>  [16] "2020-01-16" "2020-01-17" "2020-01-18" "2020-01-19" "2020-01-20"
#>  [21] "2020-01-21" "2020-01-22" "2020-01-23" "2020-01-24" "2020-01-25"
#>  [26] "2020-01-26" "2020-01-27" "2020-01-28" "2020-01-29" "2020-01-30"
#>  [31] "2020-01-31" "2020-02-01" "2020-02-02" "2020-02-03" "2020-02-04"
#>  [36] "2020-02-05" "2020-02-06" "2020-02-07" "2020-02-08" "2020-02-09"
#>  [41] "2020-02-10" "2020-02-11" "2020-02-12" "2020-02-13" "2020-02-14"
#>  [46] "2020-02-15" "2020-02-16" "2020-02-17" "2020-02-18" "2020-02-19"
#>  [51] "2020-02-20" "2020-02-21" "2020-02-22" "2020-02-23" "2020-02-24"
#>  [56] "2020-02-25" "2020-02-26" "2020-02-27" "2020-02-28" "2020-02-29"
#>  [61] "2020-03-01" "2020-03-02" "2020-03-03" "2020-03-04" "2020-03-05"
#>  [66] "2020-03-06" "2020-03-07" "2020-03-08" "2020-03-09" "2020-03-10"
#>  [71] "2020-03-11" "2020-03-12" "2020-03-13" "2020-03-14" "2020-03-15"
#>  [76] "2020-03-16" "2020-03-17" "2020-03-18" "2020-03-19" "2020-03-20"
#>  [81] "2020-03-21" "2020-03-22" "2020-03-23" "2020-03-24" "2020-03-25"
#>  [86] "2020-03-26" "2020-03-27" "2020-03-28" "2020-03-29" "2020-03-30"
#>  [91] "2020-03-31" "2020-04-01" "2020-04-02" "2020-04-03" "2020-04-04"
#>  [96] "2020-04-05" "2020-04-06" "2020-04-07" "2020-04-08" "2020-04-09"
#> [101] "2020-04-10" "2020-04-11" "2020-04-12" "2020-04-13" "2020-04-14"
#> [106] "2020-04-15" "2020-04-16" "2020-04-17" "2020-04-18" "2020-04-19"
#> [111] "2020-04-20" "2020-04-21" "2020-04-22" "2020-04-23" "2020-04-24"
#> [116] "2020-04-25" "2020-04-26" "2020-04-27" "2020-04-28" "2020-04-29"
#> [121] "2020-04-30" "2020-05-01" "2020-05-02" "2020-05-03" "2020-05-04"
#> [126] "2020-05-05" "2020-05-06" "2020-05-07" "2020-05-08" "2020-05-09"
#> [131] "2020-05-10" "2020-05-11" "2020-05-12" "2020-05-13" "2020-05-14"
#> [136] "2020-05-15" "2020-05-16" "2020-05-17" "2020-05-18" "2020-05-19"
#> [141] "2020-05-20" "2020-05-21" "2020-05-22" "2020-05-23" "2020-05-24"
#> [146] "2020-05-25" "2020-05-26" "2020-05-27" "2020-05-28" "2020-05-29"
#> [151] "2020-05-30" "2020-05-31" "2020-06-01" "2020-06-02" "2020-06-03"
#> [156] "2020-06-04" "2020-06-05" "2020-06-06" "2020-06-07" "2020-06-08"
#> [161] "2020-06-09" "2020-06-10" "2020-06-11" "2020-06-12" "2020-06-13"
#> [166] "2020-06-14" "2020-06-15" "2020-06-16" "2020-06-17" "2020-06-18"
#> [171] "2020-06-19" "2020-06-20" "2020-06-21" "2020-06-22" "2020-06-23"
#> [176] "2020-06-24" "2020-06-25" "2020-06-26" "2020-06-27" "2020-06-28"
#> [181] "2020-06-29" "2020-06-30" "2020-07-01"
WORKDAY_SEQUENCE("2020-01-01", "2020-07-01")
#>   [1] "2020-01-01" "2020-01-02" "2020-01-03" "2020-01-06" "2020-01-07"
#>   [6] "2020-01-08" "2020-01-09" "2020-01-10" "2020-01-13" "2020-01-14"
#>  [11] "2020-01-15" "2020-01-16" "2020-01-17" "2020-01-20" "2020-01-21"
#>  [16] "2020-01-22" "2020-01-23" "2020-01-24" "2020-01-27" "2020-01-28"
#>  [21] "2020-01-29" "2020-01-30" "2020-01-31" "2020-02-03" "2020-02-04"
#>  [26] "2020-02-05" "2020-02-06" "2020-02-07" "2020-02-10" "2020-02-11"
#>  [31] "2020-02-12" "2020-02-13" "2020-02-14" "2020-02-17" "2020-02-18"
#>  [36] "2020-02-19" "2020-02-20" "2020-02-21" "2020-02-24" "2020-02-25"
#>  [41] "2020-02-26" "2020-02-27" "2020-02-28" "2020-03-02" "2020-03-03"
#>  [46] "2020-03-04" "2020-03-05" "2020-03-06" "2020-03-09" "2020-03-10"
#>  [51] "2020-03-11" "2020-03-12" "2020-03-13" "2020-03-16" "2020-03-17"
#>  [56] "2020-03-18" "2020-03-19" "2020-03-20" "2020-03-23" "2020-03-24"
#>  [61] "2020-03-25" "2020-03-26" "2020-03-27" "2020-03-30" "2020-03-31"
#>  [66] "2020-04-01" "2020-04-02" "2020-04-03" "2020-04-06" "2020-04-07"
#>  [71] "2020-04-08" "2020-04-09" "2020-04-10" "2020-04-13" "2020-04-14"
#>  [76] "2020-04-15" "2020-04-16" "2020-04-17" "2020-04-20" "2020-04-21"
#>  [81] "2020-04-22" "2020-04-23" "2020-04-24" "2020-04-27" "2020-04-28"
#>  [86] "2020-04-29" "2020-04-30" "2020-05-01" "2020-05-04" "2020-05-05"
#>  [91] "2020-05-06" "2020-05-07" "2020-05-08" "2020-05-11" "2020-05-12"
#>  [96] "2020-05-13" "2020-05-14" "2020-05-15" "2020-05-18" "2020-05-19"
#> [101] "2020-05-20" "2020-05-21" "2020-05-22" "2020-05-25" "2020-05-26"
#> [106] "2020-05-27" "2020-05-28" "2020-05-29" "2020-06-01" "2020-06-02"
#> [111] "2020-06-03" "2020-06-04" "2020-06-05" "2020-06-08" "2020-06-09"
#> [116] "2020-06-10" "2020-06-11" "2020-06-12" "2020-06-15" "2020-06-16"
#> [121] "2020-06-17" "2020-06-18" "2020-06-19" "2020-06-22" "2020-06-23"
#> [126] "2020-06-24" "2020-06-25" "2020-06-26" "2020-06-29" "2020-06-30"
#> [131] "2020-07-01"
HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01", calendar = "NYSE")
#> [1] "2020-01-01" "2020-01-20" "2020-02-17" "2020-04-10" "2020-05-25"
WORKDAY_SEQUENCE("2020-01-01", "2020-07-01",
                 holidays = HOLIDAY_SEQUENCE("2020-01-01", "2020-07-01",
                                             calendar = "NYSE"))
#>   [1] "2020-01-02" "2020-01-03" "2020-01-06" "2020-01-07" "2020-01-08"
#>   [6] "2020-01-09" "2020-01-10" "2020-01-13" "2020-01-14" "2020-01-15"
#>  [11] "2020-01-16" "2020-01-17" "2020-01-21" "2020-01-22" "2020-01-23"
#>  [16] "2020-01-24" "2020-01-27" "2020-01-28" "2020-01-29" "2020-01-30"
#>  [21] "2020-01-31" "2020-02-03" "2020-02-04" "2020-02-05" "2020-02-06"
#>  [26] "2020-02-07" "2020-02-10" "2020-02-11" "2020-02-12" "2020-02-13"
#>  [31] "2020-02-14" "2020-02-18" "2020-02-19" "2020-02-20" "2020-02-21"
#>  [36] "2020-02-24" "2020-02-25" "2020-02-26" "2020-02-27" "2020-02-28"
#>  [41] "2020-03-02" "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06"
#>  [46] "2020-03-09" "2020-03-10" "2020-03-11" "2020-03-12" "2020-03-13"
#>  [51] "2020-03-16" "2020-03-17" "2020-03-18" "2020-03-19" "2020-03-20"
#>  [56] "2020-03-23" "2020-03-24" "2020-03-25" "2020-03-26" "2020-03-27"
#>  [61] "2020-03-30" "2020-03-31" "2020-04-01" "2020-04-02" "2020-04-03"
#>  [66] "2020-04-06" "2020-04-07" "2020-04-08" "2020-04-09" "2020-04-13"
#>  [71] "2020-04-14" "2020-04-15" "2020-04-16" "2020-04-17" "2020-04-20"
#>  [76] "2020-04-21" "2020-04-22" "2020-04-23" "2020-04-24" "2020-04-27"
#>  [81] "2020-04-28" "2020-04-29" "2020-04-30" "2020-05-01" "2020-05-04"
#>  [86] "2020-05-05" "2020-05-06" "2020-05-07" "2020-05-08" "2020-05-11"
#>  [91] "2020-05-12" "2020-05-13" "2020-05-14" "2020-05-15" "2020-05-18"
#>  [96] "2020-05-19" "2020-05-20" "2020-05-21" "2020-05-22" "2020-05-26"
#> [101] "2020-05-27" "2020-05-28" "2020-05-29" "2020-06-01" "2020-06-02"
#> [106] "2020-06-03" "2020-06-04" "2020-06-05" "2020-06-08" "2020-06-09"
#> [111] "2020-06-10" "2020-06-11" "2020-06-12" "2020-06-15" "2020-06-16"
#> [116] "2020-06-17" "2020-06-18" "2020-06-19" "2020-06-22" "2020-06-23"
#> [121] "2020-06-24" "2020-06-25" "2020-06-26" "2020-06-29" "2020-06-30"
#> [126] "2020-07-01"

# Date Collapsers ---
FLOOR_DATE(AS_DATE("2020-01-15"), by = "month")
#> [1] "2020-01-01"
CEILING_DATE(AS_DATE("2020-01-15"), by = "month")
#> [1] "2020-02-01"
CEILING_DATE(AS_DATE("2020-01-15"), by = "month") - ddays(1) # EOMONTH using lubridate
#> [1] "2020-01-31"

# --- Usage with tidyverse ---

# Calculate returns by symbol/year/quarter
FANG %>%
    pivot_table(
        .rows       = c(symbol, ~ QUARTER(date)),
        .columns    = ~ YEAR(date),
        .values     = ~ PCT_CHANGE_FIRSTLAST(adjusted)
    )
#> # A tibble: 16 × 6
#>    symbol `QUARTER(date)`  `2013`   `2014`  `2015`    `2016`
#>    <chr>            <int>   <dbl>    <dbl>   <dbl>     <dbl>
#>  1 AMZN                 1  0.0357 -0.155    0.206  -0.0681  
#>  2 AMZN                 2  0.0615 -0.0531   0.172   0.196   
#>  3 AMZN                 3  0.108  -0.0299   0.170   0.154   
#>  4 AMZN                 4  0.243  -0.0224   0.298  -0.104   
#>  5 GOOG                 1  0.0981  0.00174  0.0442  0.00419 
#>  6 GOOG                 2  0.0988  0.0143  -0.0406 -0.0771  
#>  7 GOOG                 3 -0.0135 -0.00911  0.166   0.112   
#>  8 GOOG                 4  0.263  -0.0737   0.241  -0.000958
#>  9 META                 1 -0.0864  0.101    0.0481  0.116   
#> 10 META                 2 -0.0255  0.0746   0.0502 -0.0153  
#> 11 META                 3  1.02    0.161    0.0344  0.123   
#> 12 META                 4  0.0839  0.0192   0.151  -0.107   
#> 13 NFLX                 1  1.06   -0.0297   0.194  -0.0703  
#> 14 NFLX                 2  0.157   0.208    0.590  -0.135   
#> 15 NFLX                 3  0.379  -0.0463   0.103   0.0194  
#> 16 NFLX                 4  0.134  -0.221    0.0793  0.206   
```
