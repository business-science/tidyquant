library(tidyquant)

#### Setup
get <- "economic.data"
context(paste0("Testing tq_get(get = '", get, "')"))

test1 <- tq_get("CPIAUCSL", get = get,
                from = "2016-01-01", to = "2016-06-01",
                adjust = TRUE, type = "splits")


test2 <- tq_get("CPIAUCSL", get = get,
                from = "2016-01-01", to = "2016-06-01",
                adjust = FALSE, type = "price")

#### Tests

test_that("Test returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    expect_is(test2, "tbl")
    # Identical
    expect_identical(test1, test2)
    # Rows
    expect_equal(nrow(test1), 6)
    # Columns
    expect_equal(ncol(test1), 2)
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get)), NA)
})
