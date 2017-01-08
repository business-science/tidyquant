## Release Summary
This is the second release of tidyquant for CRAN review. This release adds new functionality including `get = "key.ratios"` option for `tq_get()`, and `tq_mutate` and `tq_transform` integration with the `rollapply` functions from the `zoo` package. Several function arguments were renamed to make more intuitive, and several fixes were made to the naming of columns.


## Test environments
* local Windows install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)


## R CMD check results
There were no ERRORs, WARNINGs, or NOTES.

    R CMD check results
    0 errors | 0 warnings | 0 notes
    
    R CMD check succeeded

  

## Downstream dependencies
I have also run R CMD check on downstream dependencies. Zero problems were detected.
