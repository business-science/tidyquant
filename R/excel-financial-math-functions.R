#' Excel Financial Math Functions
#'
#' @description
#' __Excel financial math functions__ are designed to easily calculate Net Present Value ([NPV()]),
#' Future Value of cashflow ([FV()]), Present Value of future cashflow ([PV()]), and more.
#'
#' These functions are designed to help users coming from an __Excel background__.
#' Most functions replicate the behavior of Excel:
#' - Names are similar to Excel function names
#' - By default, missing values are ignored (same as in Excel)
#'
#'
#'
#' @param cashflow Cash flow values. When one value is provided, it's assumed constant cash flow.
#' @param rate One or more rate. When one rate is provided it's assumed constant rate.
#' @param nper Number of periods. When `nper`` is provided, the cashflow values and rate are assumed constant.
#' @param pv Present value. Initial investments (cash inflows) are typcially a negative value.
#' @param fv Future value. Cash outflows are typically a positive value.
#' @param pmt Number of payments per period.
#' @param type Should payments (`pmt`) occur at the beginning (`type = 0`) or
#' the end (`type = 1`) of each period.
#'
#' @return
#' - Summary functions return a single value
#'
#' @details
#' __Net Present Value (NPV)__
#' Net present value (NPV) is the difference between the present value of cash inflows and
#' the present value of cash outflows over a period of time. NPV is used in capital budgeting
#' and investment planning to analyze the profitability of a projected investment or project.
#' For more information, see [Investopedia NPV](https://www.investopedia.com/terms/n/npv.asp).
#'
#' __Internal Rate of Return (IRR)__
#' The internal rate of return (IRR) is a metric used in capital budgeting to estimate the
#' profitability of potential investments. The internal rate of return is a discount rate
#' that makes the net present value (NPV) of all cash flows from a particular project equal
#' to zero. IRR calculations rely on the same formula as NPV does.
#' For more information, see [Investopedia IRR](https://www.investopedia.com/terms/i/irr.asp).
#'
#' __Future Value (FV)__
#' Future value (FV) is the value of a current asset at a future date based on an assumed
#' rate of growth. The future value (FV) is important to investors and financial planners
#' as they use it to estimate how much an investment made today will be worth in the future.
#' Knowing the future value enables investors to make sound investment decisions based on
#' their anticipated needs. However, external economic factors, such as inflation, can adversely
#' affect the future value of the asset by eroding its value.
#' For more information, see [Investopedia FV](https://www.investopedia.com/terms/f/futurevalue.asp).
#'
#' __Present Value (PV)__
#' Present value (PV) is the current value of a future sum of money or stream of cash flows given a
#' specified rate of return. Future cash flows are discounted at the discount rate, and the higher
#' the discount rate, the lower the present value of the future cash flows. Determining the
#' appropriate discount rate is the key to properly valuing future cash flows, whether they be earnings
#'  or obligations. For more information, see [Investopedia PV](https://www.investopedia.com/terms/p/presentvalue.asp).
#'
#' __Payment (PMT)__
#' The Payment [PMT()] function calculates the payment for a loan based on constant payments and a constant interest rate.
#'
#' __Rate (RATE)__
#' Returns the interest rate per period of a loan or an investment.
#' For example, use 6%/4 for quarterly payments at 6% APR.
#'
#' @examples
#'
#' NPV(c(-1000, 250, 350, 450, 450), rate = 0.05)
#'
#' IRR(c(-1000, 250, 350, 450, 450))
#'
#' FV(rate = 0.05, nper = 5, pv = -100, pmt = 0, type = 0)
#'
#' PV(rate = 0.05, nper = 5, fv = -100, pmt = 0, type = 0)
#'
#' PMT(nper = 20, rate = 0.05, pv = -100, fv = 0, type = 0)
#'
#' RATE(nper = 20, pmt = 8, pv = -100, fv = 0, type = 0)
#'
#' @name excel_financial_math_functions


#' @rdname excel_financial_math_functions
#' @export
NPV <- function(cashflow, rate, nper = NULL) {

    validate_numericish(cashflow)
    validate_numericish(rate)

    # Case when cash outflow (initial investment) is present
    period <- 1:length(cashflow)
    if (cashflow[[1]] < 0) {
      # Cash outflow
        period <- period - 1
    }

    # Case when nper is present
    if (!is.null(nper)) {
        if (length(cashflow) > 1)
            warning("NPV(): Using nper with more than one cashflow value. Only first value being used. You probably want to use nper = NULL.", call. = FALSE)

        period   <- 1:nper
        cashflow <- cashflow[[1]]
        rate     <- rate[[1]]
    }

    # Perform NPV
    npv <- tibble::tibble(
        cashflow = cashflow,
        rate     = rate,
        period   = period
    ) %>%
        dplyr::summarise(npv = sum(cashflow / (1 + rate)^period )) %>%
        dplyr::pull(npv)

    return(npv)

}


#' @rdname excel_financial_math_functions
#' @export
IRR <- function(cashflow) {

    # Case when cash outflow (initial investment) is not present
    if (cashflow[[1]] > 0) {
        stop("IRR(): cashflow[1] is positive. Initial investment must be a negative cashflow to calculate IRR.")
    }

    # Setup IRR function to optimize
    starting_value <- 0.10
    irr_fun <- function(r, x){
        ( sum(x / (1 + r)^{0:(length(x)-1)}) )^2
    }

    # Optimize IRR
    result <- stats::optim(
        par    = starting_value,
        fn     = irr_fun,
        x      = cashflow,
        method = "Brent",
        lower  = -1000000,
        upper  =  1000000)

    return(result$par)

}

#' @rdname excel_financial_math_functions
#' @export
FV <- function(rate, nper, pv = 0, pmt = 0, type = 0) {
    (-1 * pv * (1 + rate)^nper) + (-1 * ( pmt / rate * ((1 + rate)^nper - 1) ) * (1 + rate)^type)
}

#' @rdname excel_financial_math_functions
#' @export
PV <- function(rate, nper, fv = 0, pmt = 0, type = 0) {
    (-1 * fv / (1 + rate)^nper) + (-1 * (pmt / rate * (1 - 1 / (1 + rate)^nper))*(1 + rate)^type)
}

#' @rdname excel_financial_math_functions
#' @export
PMT <- function(rate, nper, pv, fv = 0, type = 0) {
    ( pv + fv / (1 + rate)^nper ) * rate/ (1 - 1 / (1 + rate)^nper) * (-1) * (1 + rate)^(-1 * type)
}

#' @rdname excel_financial_math_functions
#' @export
RATE <- function(nper, pmt, pv, fv = 0, type = 0) {

    # Setup rate function to optimize
    rate_fun <- function(r, nper, pmt, pv, fv, type){
        FV(rate = r, nper = nper, pv = pv, pmt = pmt, type = type) - fv
    }

    # Find rate
    result <- stats::uniroot(
        f      = rate_fun,
        nper   = nper,
        pmt    = pmt,
        pv     = pv,
        fv     = fv,
        type   = type,
        lower  = 1e-6,
        upper  = 1e6,
    )

    return(result$root)

}



