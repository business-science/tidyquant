context("Testing tq_index()")

#### Setup
options <- tq_index_options()

#### Tests

test_that("Test returns list of 5 options", {
    expect_length(options, 5)
})

# Long running script: Collecting all stock lists
test_that("Test all stock index options to ensure no issues during fetch.", {


    skip_on_cran()
    test_option <- function(object) {
        expect_s3_class(object, "tbl_df")
        expect_gt(nrow(object), 3)
    }
    tq_indexes <- purrr::map(
        purrr::set_names(options),
        tq_index
    )
    # Apply the test to every option
    purrr::walk(tq_indexes, test_option)

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
