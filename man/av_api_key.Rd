% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-alphavantager.R
\name{av_api_key}
\alias{av_api_key}
\title{Set Alpha Vantage API Key}
\usage{
av_api_key(api_key)
}
\arguments{
\item{api_key}{Optionally passed parameter to set Alpha Vantage \code{api_key}.}
}
\value{
Returns invisibly the currently set \code{api_key}
}
\description{
Requires the alphavantager packager to use.
}
\details{
A wrapper for \code{alphavantager::av_api_key()}
}
\examples{

\dontrun{
if (rlang::is_installed("alphavantager")) {
av_api_key(api_key = "foobar")
}
}

}
\seealso{
\code{\link[=tq_get]{tq_get()}} \code{get = "alphavantager"}
}
