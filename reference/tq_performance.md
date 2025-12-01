# Computes a wide variety of summary performance metrics from stock or portfolio returns

Asset and portfolio performance analysis is a deep field with a wide
range of theories and methods for analyzing risk versus reward. The
`PerformanceAnalytics` package consolidates many of the most widely used
performance metrics as functions that can be applied to stock or
portfolio returns. `tq_performance` implements these performance
analysis functions in a tidy way, enabling scaling analysis using the
split, apply, combine framework.

## Usage

``` r
tq_performance(data, Ra, Rb = NULL, performance_fun, ...)

tq_performance_(data, Ra, Rb = NULL, performance_fun, ...)

tq_performance_fun_options()
```

## Arguments

- data:

  A `tibble` (tidy data frame) of returns in tidy format (i.e long
  format).

- Ra:

  The column of asset returns

- Rb:

  The column of baseline returns (for functions that require comparison
  to a baseline)

- performance_fun:

  The performance function from `PerformanceAnalytics`. See
  `tq_performance_fun_options()` for a complete list of integrated
  functions.

- ...:

  Additional parameters passed to the `PerformanceAnalytics` function.

## Value

Returns data in the form of a `tibble` object.

## Details

**Important concept**: Performance is based on the statistical
properties of returns, and as a result this function uses stock or
portfolio returns as opposed to stock prices.

`tq_performance` is a wrapper for various `PerformanceAnalytics`
functions that return portfolio statistics. The main advantage is the
ability to scale with the `tidyverse`.

`Ra` and `Rb` are the columns containing asset and baseline returns,
respectively. These columns are mapped to the `PerformanceAnalytics`
functions. Note that `Rb` is not always required, and in these instances
the argument defaults to `Rb = NULL`. The user can tell if `Rb` is
required by researching the underlying performance function.

`...` are additional arguments that are passed to the
`PerformanceAnalytics` function. Search the underlying function to see
what arguments can be passed through.

`tq_performance_fun_options` returns a list of compatible
`PerformanceAnalytics` functions that can be supplied to the
`performance_fun` argument.

## See also

- [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  which can be used to calculate period returns from a set of stock
  prices. Use `mutate_fun = periodReturn` with the appropriate
  periodicity such as `period = "monthly"`.

- [`tq_portfolio()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)
  which can be used to aggregate period returns from multiple stocks to
  period returns for a portfolio.

- The `PerformanceAnalytics` package, which contains the underlying
  functions for the `performance_fun` argument. Additional parameters
  can be passed via `...`.

## Examples

``` r
# Load libraries
library(dplyr)

# Use FANG data set

# Get returns for individual stock components grouped by symbol
Ra <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Ra")

# Get returns for SP500 as baseline
Rb <- "^GSPC" %>%
    tq_get(get  = "stock.prices",
           from = "2010-01-01",
           to   = "2015-12-31") %>%
    tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "Rb")

# Merge stock returns with baseline
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))

##### Performance Metrics #####

# View options
tq_performance_fun_options()
#> $table.funs
#>  [1] "table.AnnualizedReturns" "table.Arbitrary"        
#>  [3] "table.Autocorrelation"   "table.CAPM"             
#>  [5] "table.CaptureRatios"     "table.Correlation"      
#>  [7] "table.Distributions"     "table.DownsideRisk"     
#>  [9] "table.DownsideRiskRatio" "table.DrawdownsRatio"   
#> [11] "table.HigherMoments"     "table.InformationRatio" 
#> [13] "table.RollingPeriods"    "table.SFM"              
#> [15] "table.SpecificRisk"      "table.Stats"            
#> [17] "table.TrailingPeriods"   "table.UpDownRatios"     
#> [19] "table.Variability"      
#> 
#> $CAPM.funs
#>  [1] "CAPM.alpha"       "CAPM.beta"        "CAPM.beta.bear"   "CAPM.beta.bull"  
#>  [5] "CAPM.CML"         "CAPM.CML.slope"   "CAPM.dynamic"     "CAPM.epsilon"    
#>  [9] "CAPM.jensenAlpha" "CAPM.RiskPremium" "CAPM.SML.slope"   "TimingRatio"     
#> [13] "MarketTiming"    
#> 
#> $SFM.funs
#> [1] "SFM.alpha"       "SFM.beta"        "SFM.CML"         "SFM.CML.slope"  
#> [5] "SFM.dynamic"     "SFM.epsilon"     "SFM.jensenAlpha"
#> 
#> $descriptive.funs
#> [1] "mean"           "sd"             "min"            "max"           
#> [5] "cor"            "mean.geometric" "mean.stderr"    "mean.LCL"      
#> [9] "mean.UCL"      
#> 
#> $annualized.funs
#> [1] "Return.annualized"        "Return.annualized.excess"
#> [3] "sd.annualized"            "SharpeRatio.annualized"  
#> 
#> $VaR.funs
#> [1] "VaR"  "ES"   "ETL"  "CDD"  "CVaR"
#> 
#> $moment.funs
#>  [1] "var"              "cov"              "skewness"         "kurtosis"        
#>  [5] "CoVariance"       "CoSkewness"       "CoSkewnessMatrix" "CoKurtosis"      
#>  [9] "CoKurtosisMatrix" "M3.MM"            "M4.MM"            "BetaCoVariance"  
#> [13] "BetaCoSkewness"   "BetaCoKurtosis"  
#> 
#> $drawdown.funs
#> [1] "AverageDrawdown"   "AverageLength"     "AverageRecovery"  
#> [4] "DrawdownDeviation" "DrawdownPeak"      "maxDrawdown"      
#> 
#> $Bacon.risk.funs
#> [1] "MeanAbsoluteDeviation" "Frequency"             "SharpeRatio"          
#> [4] "MSquared"              "MSquaredExcess"        "HurstIndex"           
#> 
#> $Bacon.regression.funs
#>  [1] "CAPM.alpha"       "CAPM.beta"        "CAPM.epsilon"     "CAPM.jensenAlpha"
#>  [5] "SystematicRisk"   "SpecificRisk"     "TotalRisk"        "TreynorRatio"    
#>  [9] "AppraisalRatio"   "FamaBeta"         "Selectivity"      "NetSelectivity"  
#> 
#> $Bacon.relative.risk.funs
#> [1] "ActivePremium"    "ActiveReturn"     "TrackingError"    "InformationRatio"
#> 
#> $Bacon.drawdown.funs
#> [1] "PainIndex"     "PainRatio"     "CalmarRatio"   "SterlingRatio"
#> [5] "BurkeRatio"    "MartinRatio"   "UlcerIndex"   
#> 
#> $Bacon.downside.risk.funs
#>  [1] "DownsideDeviation"     "DownsidePotential"     "DownsideFrequency"    
#>  [4] "SemiDeviation"         "SemiVariance"          "UpsideRisk"           
#>  [7] "UpsidePotentialRatio"  "UpsideFrequency"       "BernardoLedoitRatio"  
#> [10] "DRatio"                "Omega"                 "OmegaSharpeRatio"     
#> [13] "OmegaExcessReturn"     "SortinoRatio"          "M2Sortino"            
#> [16] "Kappa"                 "VolatilitySkewness"    "AdjustedSharpeRatio"  
#> [19] "SkewnessKurtosisRatio" "ProspectRatio"        
#> 
#> $misc.funs
#> [1] "KellyRatio"   "Modigliani"   "UpDownRatios"
#> 

# Get performance metrics
RaRb %>%
    tq_performance(Ra = Ra, performance_fun = SharpeRatio, p = 0.95)
#> # A tibble: 4 × 5
#> # Groups:   symbol [4]
#>   symbol `ESSharpe(Rf=0%,p=95%)` SemiSDSharpe(Rf=0%,p=9…¹ StdDevSharpe(Rf=0%,p…²
#>   <chr>                    <dbl>                    <dbl>                  <dbl>
#> 1 META                     0.193                    0.424                  0.345
#> 2 AMZN                     0.215                    0.339                  0.314
#> 3 NFLX                     0.199                    0.438                  0.355
#> 4 GOOG                     0.213                    0.354                  0.296
#> # ℹ abbreviated names: ¹​`SemiSDSharpe(Rf=0%,p=95%)`,
#> #   ²​`StdDevSharpe(Rf=0%,p=95%)`
#> # ℹ 1 more variable: `VaRSharpe(Rf=0%,p=95%)` <dbl>

RaRb %>%
    tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
#> Registered S3 method overwritten by 'robustbase':
#>   method          from     
#>   hatvalues.lmrob RobStatTM
#> # A tibble: 4 × 18
#> # Groups:   symbol [4]
#>   symbol ActivePremium  Alpha AlphaRobust AnnualizedAlpha  Beta `Beta+`
#>   <chr>          <dbl>  <dbl>       <dbl>           <dbl> <dbl>   <dbl>
#> 1 META           0.431 0.034       0.0273           0.493 0.846    3.00
#> 2 AMZN           0.246 0.0144      0.0094           0.187 1.46     2.04
#> 3 NFLX           1.02  0.0632      0.0516           1.09  1.35     1.90
#> 4 GOOG           0.142 0.0123      0.0053           0.158 0.901    1.56
#> # ℹ 11 more variables: `Beta+Robust` <dbl>, `Beta-` <dbl>, `Beta-Robust` <dbl>,
#> #   BetaRobust <dbl>, Correlation <dbl>, `Correlationp-value` <dbl>,
#> #   InformationRatio <dbl>, `R-squared` <dbl>, `R-squaredRobust` <dbl>,
#> #   TrackingError <dbl>, TreynorRatio <dbl>
```
