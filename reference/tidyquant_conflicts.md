# Conflicts between the tidyquant and other packages

This function lists all the conflicts between packages in the tidyverse
and other packages that you have loaded.

## Usage

``` r
tidyquant_conflicts(only = NULL)
```

## Arguments

- only:

  Set this to a character vector to restrict to conflicts only with
  these packages.

## Details

There are four conflicts that are deliberately ignored: `intersect`,
`union`, `setequal`, and `setdiff` from dplyr. These functions make the
base equivalents generic, so shouldn't negatively affect any existing
code.

## Examples

``` r
tidyquant_conflicts()
#> ── Conflicts ────────────────────────────────────────── tidyquant_conflicts() ──
#> ✖ zoo::as.Date()                 masks base::as.Date()
#> ✖ zoo::as.Date.numeric()         masks base::as.Date.numeric()
#> ✖ dplyr::filter()                masks stats::filter()
#> ✖ dplyr::first()                 masks xts::first()
#> ✖ dplyr::lag()                   masks stats::lag()
#> ✖ dplyr::last()                  masks xts::last()
#> ✖ PerformanceAnalytics::legend() masks graphics::legend()
#> ✖ quantmod::summary()            masks base::summary()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```
