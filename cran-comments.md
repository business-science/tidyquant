## Release Summary
This release of tidyquant fixes the ERROR messages on CRAN. The Google Finance
API no longer provides data, which was the cause of most failures. It has been
deprecated in tidyquant.

## Test environments
* local Windows install, R 3.4.4
* local Mac install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
I have also run R CMD check on downstream dependencies. Zero problems were detected.
