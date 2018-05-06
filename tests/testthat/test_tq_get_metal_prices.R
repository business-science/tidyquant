library(tidyquant)

#### Setup

get <- "metal.prices"
context(paste0("Testing tq_get(get = '", get, "')"))

# test1 <- tq_get("gold", get = get,
#                 from = Sys.Date()-100, to = Sys.Date(),
#                 adjust = TRUE, type = "splits")
#
#
# test2 <- tq_get("gold", get = get,
#                 from = Sys.Date()-100, to = Sys.Date(),
#                 adjust = FALSE, type = "price")

#### Tests

# test_that("Test returns tibble with correct rows and columns.", {
#     # Tibble
#     expect_is(test1, "tbl")
#     expect_is(test2, "tbl")
#     # Identical
#     expect_identical(test1, test2)
#     # Columns
#     expect_equal(ncol(test1), 2)
# })

test_that("Test prints warning message on invalid x input.", {
    expect_warning(tq_get("XYZ", get = get, from = Sys.Date() - 100, to = Sys.Date()))
})

test_that("Test returns NA on invalid x input.", {
    expect_equal(suppressWarnings(tq_get("XYZ", get = get, from = Sys.Date() - 100, to = Sys.Date())), NA)
})
