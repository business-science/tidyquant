% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot-coord_date.R
\name{coord_x_date}
\alias{coord_x_date}
\alias{coord_x_datetime}
\title{Zoom in on plot regions using date ranges or date-time ranges}
\usage{
coord_x_date(xlim = NULL, ylim = NULL, expand = TRUE)

coord_x_datetime(xlim = NULL, ylim = NULL, expand = TRUE)
}
\arguments{
\item{xlim}{Limits for the x axis, entered as character dates in "YYYY-MM-DD"
format for date or "YYYY-MM-DD HH:MM:SS" for date-time.}

\item{ylim}{Limits for the y axis, entered as values}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor to
the limits to ensure that data and axes don't overlap. If \code{FALSE},
limits are taken exactly from the data or \code{xlim}/\code{ylim}.}
}
\description{
Zoom in on plot regions using date ranges or date-time ranges
}
\details{
The \code{coord_} functions prevent loss of data during zooming, which is
necessary when zooming in on plots that calculate \code{stats} using data
outside of the zoom range (e.g. when plotting moving averages
with \code{\link[=geom_ma]{geom_ma()}}). Setting limits using \code{scale_x_date}
changes the underlying data which causes moving averages to fail.

\code{coord_x_date} is a wrapper for \code{coord_cartesian}
that enables quickly zooming in on plot regions using a date range.

\code{coord_x_datetime} is a wrapper for \code{coord_cartesian}
that enables quickly zooming in on plot regions using a date-time range.
}
\examples{
# Load libraries
library(dplyr)
library(ggplot2)

# coord_x_date
AAPL <- tq_get("AAPL", from = "2013-01-01", to = "2016-12-31")
AAPL \%>\%
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
hourly_data \%>\%
    ggplot(aes(x = time.index, y = value)) +
    geom_point() +
    coord_x_datetime(xlim = c("2012-05-15 07:00:00", "2012-05-15 16:00:00"))
}
\seealso{
\code{\link[ggplot2:coord_cartesian]{ggplot2::coord_cartesian()}}
}
