## Test environments
* local Windows install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2


## R CMD check results
There were no ERRORs, WARNINGs, or NOTES.

    R CMD check results
    0 errors | 0 warnings | 0 notes
    
    R CMD check succeeded

  

## Downstream dependencies
I have also run R CMD check on downstream dependencies of quantmod, xts, 
TTR, tidyverse, lubridate, and magrittr. All packages that I could install
have passed.
