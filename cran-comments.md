## Release Summary
This is a bug fix release of tidyquant for CRAN review. This release fixes 2 tests that were still broken due to the Yahoo API change. It also fixes `tq_index()` which had broken due to a change with Marketvolume. Lastly, it uses the coercion functions from `timekit` instead of `tidyquant`, and does a soft deprecation of `tidyquant::as_tibble()` and `tidyquant::as_xts()`.

## Test environments
* local Windows install, R 3.4.0
* local Mac install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

    R CMD check results
    0 errors | 0 warnings | 0 note 

    R CMD check succeeded

## Downstream dependencies
I have also run R CMD check on downstream dependencies. Zero problems were detected.
