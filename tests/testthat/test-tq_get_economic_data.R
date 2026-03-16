library(tidyquant)

#### Setup
get <- "economic.data"
context(paste0("Testing tq_get(get = '", get, "')"))
skip_if_offline()

test1 <- suppressWarnings(
    tq_get("CPIAUCSL", get = get,
           from = "2016-01-01", to = "2016-06-01",
           adjust = TRUE, type = "splits")
)


test2 <- suppressWarnings(
    tq_get("CPIAUCSL", get = get,
           from = "2016-01-01", to = "2016-06-01",
           adjust = FALSE, type = "price")
)

fred_available <- tibble::is_tibble(test1) && tibble::is_tibble(test2)

#### Tests

test_that("Test returns tibble with correct rows and columns when FRED is available.", {
    skip_if_not(fred_available, "FRED unavailable; tq_get() returned NA as designed.")
    # Tibble
    expect_s3_class(test1, "tbl_df")
    expect_s3_class(test2, "tbl_df")
    # Identical
    expect_identical(test1, test2)
    # Rows
    expect_equal(nrow(test1), 6)
    # Columns
    expect_equal(ncol(test1), 3)
})

test_that("Test returns NA when FRED is unavailable.", {
    skip_if(fred_available, "FRED available.")
    expect_identical(test1, NA)
    expect_identical(test2, NA)
})

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get)), NA)
})
