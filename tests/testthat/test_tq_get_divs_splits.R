library(tidyquant)

#### Setup
get <- "divs.splits"
context(paste0("Testing tq_get(get = '", get, "')"))

test1 <- tq_get("AAPL", get = get,
                from = "2016-01-01", to = "2016-06-01",
                adjust = TRUE, type = "splits")


test2 <- tq_get("AAPL", get = get,
                from = "2016-01-01", to = "2016-06-01",
                adjust = FALSE, type = "price")

#### Tests

test_that("Test returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(test1, "tbl")
    expect_is(test2, "tbl")
    # Rows
    expect_equal(nrow(test1), 2)
    expect_equal(nrow(test2), 2)
    # Columns
    expect_equal(ncol(test1), 4)
    expect_equal(ncol(test2), 4)
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get)), NA)
})
