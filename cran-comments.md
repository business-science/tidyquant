## Release Summary
This is a bug fix release of tidyquant for CRAN review. This release is mainly to address bugs related to Oanda returning a shorter maximum amount of data (180 days) and Yahoo redirecting a HTTP request to HTTPS. The bugs were fixed with the newest version of `quantmod`, and we have required that version in the Depends. There are also updated tests relating to the Oanda fix.

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
