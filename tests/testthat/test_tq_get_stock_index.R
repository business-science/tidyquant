library(tidyquant)

#### Setup
get <- "stock.index"
context(paste0("Testing tq_get(get = '", get, "')"))

options <- tq_get("options", get = get)

#### Tests

test_that("Test returns list of 18+ options when x = 'options'", {
    options %>%
        expect_is("character") %>%
        length() %>%
        expect_gte(18)
})

# Long running script: Collecting all stock lists
test_that("Test all stock.index options to ensure no issues during fetch.", {

    skip_on_cran()
    for (i in seq_along(options)) {
        tq_get(options[[i]], get = get) %>%
            expect_is("tbl") %>%
            nrow() %>%
            expect_gt(3)
    }

})

test_that("Test returns error on invalid x input.", {
    expect_error(tq_get("XYZ", get = get))
})

test_that("Test returns error on invalid x input.", {
    expect_error(tq_get("XYZ", get = get, use_fallback = TRUE))
})

test_that("Test returns message on use_fallback = TRUE.", {
    expect_message(tq_get("SP500", get = get, use_fallback = TRUE))
})

test_that("Test returns tibble on use_fallback = TRUE.", {
    for (i in seq_along(options)) {
        tq_get(options[[i]], get = get, use_fallback = TRUE) %>%
            expect_is("tbl") %>%
            nrow() %>%
            expect_gt(3)
    }
})
