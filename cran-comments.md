## Release Summary
This is the fourth release of tidyquant for CRAN review. This release adds new functionality including `PerformanceAnalytics` integration, portfolio aggregation, improved documentation, and some new `ggplot2` themes. Several fixes were made to enable quickly getting, scaling, and manipulating data.


## Test environments
* local Windows install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)


## R CMD check results
There were no ERRORs or WARNINGs. There is one NOTE.

    R CMD check results
    0 errors | 0 warnings | 1 note 

    R CMD check succeeded

## Note and Justification:
Depends: includes the non-default packages:
  'lubridate' 'PerformanceAnalytics' 'quantmod' 'tidyverse' 'TTR' 'xts'
Adding so many packages to the search path is excessive and importing selectively is preferable.

Because `tidyquant` utilizes these packages as wrappers, the function auto-complete and documentation is needed to run effectively. Therefore, the package will not work effectively by selectively importing functions. The only option is to load the underlying packages, which loads the functions.

## Downstream dependencies
I have also run R CMD check on downstream dependencies. Zero problems were detected.
