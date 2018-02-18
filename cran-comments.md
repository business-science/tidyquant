## Resubmission
This resubmission removed Rblpapi from Imports and moved it to Suggests. This
should remove the NOTE about licensing.

## Release Summary
This release of tidyquant updates for compatibility with `testthat` v2.0 and
adds a few bug fixes. A few new sources of data are added for `tq_get()`.

## Test environments
* local Windows install, R 3.4.3
* local Mac install, R 3.4.3
* ubuntu 12.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
I have also run R CMD check on downstream dependencies. Zero problems were detected.
