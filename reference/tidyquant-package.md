# tidyquant: Integrating quantitative financial analysis tools with the tidyverse

The main advantage of `tidyquant` is to bridge the gap between the best
quantitative resources for collecting and manipulating quantitative
data, `xts`, `quantmod` and `TTR`, and the data modeling workflow and
infrastructure of the `tidyverse`.

## Details

In this package, `tidyquant` functions and supporting data sets are
provided to seamlessly combine tidy tools with existing quantitative
analytics packages. The main advantage is being able to use tidy
functions with purrr for mapping and tidyr for nesting to extend
modeling to many stocks. See the tidyquant website for more information,
documentation and examples.

Users will probably be interested in the following:

- **Getting Data from the Web:**
  [`tq_get()`](https://business-science.github.io/tidyquant/reference/tq_get.md)

- **Manipulating Data:**
  [`tq_transmute()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)
  and
  [`tq_mutate()`](https://business-science.github.io/tidyquant/reference/tq_mutate.md)

- **Performance Analysis and Portfolio Aggregation:**
  [`tq_performance()`](https://business-science.github.io/tidyquant/reference/tq_performance.md)
  and
  [`tq_portfolio()`](https://business-science.github.io/tidyquant/reference/tq_portfolio.md)

To learn more about tidyquant, start with the vignettes:
`browseVignettes(package = "tidyquant")`

## See also

Useful links:

- <https://business-science.github.io/tidyquant/>

- <https://github.com/business-science/tidyquant>

- Report bugs at <https://github.com/business-science/tidyquant/issues>

## Author

**Maintainer**: Matt Dancho <mdancho@business-science.io>

Authors:

- Davis Vaughan <dvaughan@business-science.io>
