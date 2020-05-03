## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

The note is because of the vignette folder size 5MB. The long-term plan is to move
the vignettes to a bookdown book, which will fix the note. 
