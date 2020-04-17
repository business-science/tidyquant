context("Testing tq_exchange()")

# library(tidyquant)

#### Setup
options <- tq_exchange_options()

#### Tests

test_that("Test returns list of 18+ options when x = 'options'", {
    options %>%
        expect_is("character") %>%
        length() %>%
        expect_gte(3)
})

# Long running script: Collecting all stock lists
test_that("Test all exchange options to ensure no issues during fetch.", {

    skip_on_cran()
    skip_on_travis()
    skip_on_os("windows")
    for (i in seq_along(options)) {
        tq_exchange(options[[i]]) %>%
            expect_is("tbl") %>%
            nrow() %>%
            expect_gt(3)
    }

})

test_that("Test returns error on invalid x input.", {
    skip_on_travis()
    skip_on_os("windows")
    expect_error(tq_exchange("XYZ"))
})

test_that("Test parse_dollar can parse market cap strings", {
    tst <- c("$3.2B", "$9M", "$944.43K", NA)
    expect_equal(parse_dollar(tst), c(3.2e9, 9e6, 944430, NA))
})
