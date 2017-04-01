## Release Summary
This is the fifth release of tidyquant for CRAN review. This release adds new functionality including a new "tidyverse" style _select_ argument instead of relying on OHLC functions, new data source integrations `Quandl` and Yahoo! Finance Japan, and additional `PerformanceAnalytics` functions for use in cleaning and transforming asset returns.


## Test environments
* local Windows install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
I have also run R CMD check on downstream dependencies. Zero problems were detected.
