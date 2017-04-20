
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidyquant
=========

[![Travis-CI Build Status](https://travis-ci.org/business-science/tidyquant.svg?branch=master)](https://travis-ci.org/business-science/tidyquant.svg?branch=master) [![codecov](https://codecov.io/gh/business-science/tidyquant/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/tidyquant) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidyquant)](https://cran.r-project.org/package=tidyquant) ![](http://cranlogs.r-pkg.org/badges/tidyquant?color=brightgreen) ![](http://cranlogs.r-pkg.org/badges/grand-total/tidyquant?color=brightgreen)

<img src="tools/logo.png" width="147" height="170" align="right" />

> Bringing financial analysis to the tidyverse

`tidyquant` integrates the best resources for collecting and analyzing financial data, `zoo`, `xts`, `quantmod`, `TTR`, and `PerformanceAnalytics`, with the tidy data infrastructure of the `tidyverse` allowing for seamless interaction between each. You can now perform complete financial analyses in the `tidyverse`.

Benefits
--------

-   **A few core functions with a lot of power**
-   **Integrates the quantitative analysis functionality of `zoo`, `xts`, `quantmod`, `TTR`, and *now* `PerformanceAnalytics`**
-   **Designed for modeling and scaling analyses using the the `tidyverse` tools in [*R for Data Science*](http://r4ds.had.co.nz/)**
-   **Implements `ggplot2` functionality for beautiful and meaningful financial visualizations**
-   **User-friendly documentation to get you up to speed quickly!**

One-Stop Shop for Serious Financial Analysis
--------------------------------------------

With `tidyquant` all the benefits add up to one thing: *a one-stop shop for serious financial analysis!*

### Core Functions

-   **Getting Financial Data from the web: `tq_get()`**. This is a one-stop shop for getting web-based financial data in a "tidy" data frame format. Get data for daily stock prices (historical), key statistics (real-time), key ratios (historical), financial statements, dividends, splits, economic data from the FRED, FOREX rates from Oanda.

-   **Manipulating Financial Data: `tq_transmute()` and `tq_mutate()`**. Integration for many financial functions from `xts`, `zoo`, `quantmod`,`TTR` and `PerformanceAnalytics` packages. `tq_mutate()` is used to add a column to the data frame, and `tq_transmute()` is used to return a new data frame which is necessary for periodicity changes.

-   **Coercing Data To and From xts and tibble: `as_tibble()`and `as_xts()`**. There are a ton of [Stack Overflow articles](http://stackoverflow.com/search?q=xts+data+frame) on converting data frames to and from xts. These two functions can be used to answer 99% of these questions.

-   **Performance Analysis and Portfolio Analysis: `tq_performance()` and `tq_portfolio()`**. The newest additions to the `tidyquant` family integrate `PerformanceAnalytics` functions. `tq_performance()` converts investment returns into performance metrics. `tq_portfolio()` aggregates a group (or multiple groups) of asset returns into one or more portfolios.

### Comparing Stock Prices

Visualizing the stock price volatility of four stocks side-by-side is quick and easy...

<img src="img/sample_img_1_volatility.png" width="100%" />

### Evaluating Stock Performance

What about stock performance? Quickly visualize how a $10,000 investment in various stocks would perform.

<img src="img/sample_img_2_stock_returns.png" width="100%" />

### Evaluating Portfolio Performance

Ok, stocks are too easy. What about portfolios? With the `PerformanceAnalytics` integration, visualizing blended portfolios are easy too!

-   Portfolio 1: 50% FB, 25% AMZN, 25% NFLX, 0% GOOG
-   Portfolio 2: 0% FB, 50% AMZN, 25% NFLX, 25% GOOG
-   Portfolio 3: 25% FB, 0% AMZN, 50% NFLX, 25% GOOG
-   Portfolio 4: 25% FB, 25% AMZN, 0% NFLX, 50% GOOG

<img src="img/sample_img_3_portfolio_returns.png" width="100%" />

This just scratches the surface of `tidyquant`. Here's how to install to get started.

Installation
------------

Development Version with Latest Features:

``` r
# install.packages("devtools")
devtools::install_github("business-science/tidyquant")
```

CRAN Approved Version:

``` r
install.packages("tidyquant")
```

Further Information
-------------------

The `tidyquant` package includes several vignettes to help users get up to speed quickly:

-   TQ00 - Introduction to `tidyquant`
-   TQ01 - Core Functions in `tidyquant`
-   TQ02 - R Quantitative Analysis Package Integrations in `tidyquant`
-   TQ03 - Scaling and Modeling with `tidyquant`
-   TQ04 - Charting with `tidyquant`
-   TQ05 - Performance Analysis with `tidyquant`

See the [`tidyquant` vignettes](https://cran.r-project.org/package=tidyquant) for further details on the package.
