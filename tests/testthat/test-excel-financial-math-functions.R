context("Testing Excel financial math functions")

# ZERO RATE ----
# Expected values match Excel, which uses the limit of each formula when
# rate = 0 (#256)

test_that("PMT() matches Excel when rate is 0", {
    expect_equal(PMT(rate = 0, nper = 480, pv = 100000), -100000 / 480)
    expect_equal(PMT(rate = 0, nper = 12, pv = 1200, type = 1), -100)
    expect_equal(PMT(rate = 0, nper = 10, pv = 1000, fv = 500), -150)
})

test_that("FV() matches Excel when rate is 0", {
    expect_equal(FV(rate = 0, nper = 10, pv = -100, pmt = -10), 200)
    expect_equal(FV(rate = 0, nper = 10, pv = -100, pmt = -10, type = 1), 200)
    expect_equal(FV(rate = 0, nper = 12, pmt = -100), 1200)
})

test_that("PV() matches Excel when rate is 0", {
    expect_equal(PV(rate = 0, nper = 10, fv = 100, pmt = 10), -200)
    expect_equal(PV(rate = 0, nper = 10, fv = 100, pmt = 10, type = 1), -200)
})

test_that("Zero rates are handled element-wise for vector inputs", {
    rate <- c(0, 0.05 / 12, 0)
    expect_equal(
        PMT(rate = rate, nper = 360, pv = 200000),
        c(-200000 / 360, -1073.6432460243, -200000 / 360)
    )
    expect_equal(
        FV(rate = c(0, 0.05), nper = 10, pv = -100, pmt = -10),
        c(200, 288.6683880332)
    )
    expect_equal(
        PV(rate = c(0.05, 0), nper = 10, fv = 100, pmt = 10),
        c(-138.6086746459, -200)
    )

    # A single zero rate is recycled over vector inputs
    expect_equal(PMT(rate = 0, nper = 10, pv = c(100, 200)), c(-10, -20))

    # Missing rates still give missing values
    expect_equal(PMT(rate = c(NA, 0), nper = 10, pv = 100), c(NA, -10))
})

# NON-ZERO RATE ----

test_that("Non-zero rates give the same results as before", {
    expect_equal(
        PMT(rate = 0.05 / 12, nper = 360, pv = 200000),
        -1073.6432460243
    )
    expect_equal(
        PMT(rate = 0.05 / 12, nper = 360, pv = 200000, type = 1),
        -1069.1882947960
    )
    expect_equal(PMT(rate = 0.05, nper = 20, pv = -100), 8.0242587191)
    expect_equal(FV(rate = 0.05, nper = 5, pv = -100), 127.62815625)
    expect_equal(
        FV(rate = 0.05, nper = 10, pv = -100, pmt = -10, type = 1),
        294.9573343010
    )
    expect_equal(PV(rate = 0.05, nper = 5, fv = -100), 78.3526166468)
    expect_equal(
        PV(rate = 0.05, nper = 10, fv = 100, pmt = 10, type = 1),
        -142.4695421105
    )
})
