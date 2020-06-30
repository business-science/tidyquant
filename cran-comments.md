## Test environments
* local OS X install, R 4.0.2
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

The note is because of the vignette folder size 5MB. The long-term plan is to move
the vignettes to a bookdown book, which will fix the note. 
