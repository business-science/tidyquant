context("Testing pivot_table()")

# NORMAL ----

piv_1_tbl <- FANG %>%
    pivot_table(
        .rows    = c(symbol, ~ YEAR(date)),
        .columns = c(~ MONTH(date, label = TRUE)),
        .values  = ~ SUM(volume)
    )

test_that("Test 1: Pivot table returns tibble with correct rows and columns.", {
    # Tibble
    expect_is(piv_1_tbl, "tbl")
    # Rows + columns
    expect_equal(dim(piv_1_tbl), c(16, 14))
})

# PROGRAMMING ----

rws <- "c(symbol, ~ YEAR(date))"
cls <- "c(~ MONTH(date, label = TRUE))"
vls <- "~ SUM(volume)"

piv_2_tbl <- FANG %>%
    pivot_table(
        .rows    = !! rws,
        .columns = !! cls,
        .values  = !! vls
    )

test_that("Test 2: Pivot table returns tibble with correct rows and columns.", {
    # Tibble
    expect_s3_class(piv_2_tbl, "tbl_df")
    # Rows
    expect_equal(nrow(piv_2_tbl), 16)
    # Columns
    expect_equal(ncol(piv_2_tbl), 14)
})
