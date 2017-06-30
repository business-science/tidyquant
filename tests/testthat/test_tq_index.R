library(tidyquant)

#### Setup
context("Testing tq_index")

options <- tq_index_options()

#### Tests

test_that("Test returns list of 9 options when x = 'options'", {
    options %>%
        expect_is("character") %>%
        length() %>%
        expect_gte(9)
})

# Long running script: Collecting all stock lists
test_that("Test all stock.index options to ensure no issues during fetch.", {

    skip_on_cran()
    for (i in seq_along(options)) {
        tq_index(options[[i]]) %>%
            expect_is("tbl") %>%
            nrow() %>%
            expect_gt(3)
    }

})

test_that("Test returns warning on invalid x input.", {
    expect_warning(tq_index("XYZ"))
})

test_that("Test returns error on invalid x input.", {
    expect_warning(tq_index("XYZ", use_fallback = TRUE))
})

test_that("Test returns message on use_fallback = TRUE.", {
    expect_message(tq_index("SP500", use_fallback = TRUE))
})

test_that("Test returns tibble on use_fallback = TRUE.", {
    for (i in seq_along(options)) {
        tq_index(options[[i]], use_fallback = TRUE) %>%
            expect_is("tbl") %>%
            nrow() %>%
            expect_gt(3)
    }
})
