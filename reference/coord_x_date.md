# Zoom in on plot regions using date ranges or date-time ranges

Zoom in on plot regions using date ranges or date-time ranges

## Usage

``` r
coord_x_date(xlim = NULL, ylim = NULL, expand = TRUE)

coord_x_datetime(xlim = NULL, ylim = NULL, expand = TRUE)
```

## Arguments

- xlim:

  Limits for the x axis, entered as character dates in "YYYY-MM-DD"
  format for date or "YYYY-MM-DD HH:MM:SS" for date-time.

- ylim:

  Limits for the y axis, entered as values

- expand:

  If `TRUE`, the default, adds a small expansion factor to the limits to
  ensure that data and axes don't overlap. If `FALSE`, limits are taken
  exactly from the data or `xlim`/`ylim`.

## Details

The `coord_` functions prevent loss of data during zooming, which is
necessary when zooming in on plots that calculate `stats` using data
outside of the zoom range (e.g. when plotting moving averages with
[`geom_ma()`](https://business-science.github.io/tidyquant/reference/geom_ma.md)).
Setting limits using `scale_x_date` changes the underlying data which
causes moving averages to fail.

`coord_x_date` is a wrapper for `coord_cartesian` that enables quickly
zooming in on plot regions using a date range.

`coord_x_datetime` is a wrapper for `coord_cartesian` that enables
quickly zooming in on plot regions using a date-time range.

## See also

[`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)

## Examples

``` r
# Load libraries
library(dplyr)
#> 
#> ######################### Warning from 'xts' package ##########################
#> #                                                                             #
#> # The dplyr lag() function breaks how base R's lag() function is supposed to  #
#> # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
#> # source() into this session won't work correctly.                            #
#> #                                                                             #
#> # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
#> # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
#> # dplyr from breaking base R's lag() function.                                #
#> #                                                                             #
#> # Code in packages is not affected. It's protected by R's namespace mechanism #
#> # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
#> #                                                                             #
#> ###############################################################################
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:xts’:
#> 
#>     first, last
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

# coord_x_date
AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")
AAPL %>%
    ggplot(aes(x = date, y = adjusted)) +
    geom_line() +                         # Plot stock price
    geom_ma(n = 50) +                     # Plot 50-day Moving Average
    geom_ma(n = 200, color = "red") +     # Plot 200-day Moving Average
    # Zoom in
    coord_x_date(xlim = c("2016-01-01", "2016-12-31"))



# coord_x_datetime
time_index <- seq(from = as.POSIXct("2012-05-15 07:00"),
                  to   = as.POSIXct("2012-05-17 18:00"),
                  by   = "hour")
set.seed(1)
value <- rnorm(n = length(time_index))
hourly_data <- tibble(time.index = time_index,
                      value      = value)
hourly_data %>%
    ggplot(aes(x = time.index, y = value)) +
    geom_point() +
    coord_x_datetime(xlim = c("2012-05-15 07:00:00", "2012-05-15 16:00:00"))
```
