# Excel Financial Math Functions

**Excel financial math functions** are designed to easily calculate Net
Present Value (`NPV()`), Future Value of cashflow (`FV()`), Present
Value of future cashflow (`PV()`), and more.

These functions are designed to help users coming from an **Excel
background**. Most functions replicate the behavior of Excel:

- Names are similar to Excel function names

- By default, missing values are ignored (same as in Excel)

## Usage

``` r
NPV(cashflow, rate, nper = NULL)

IRR(cashflow)

FV(rate, nper, pv = 0, pmt = 0, type = 0)

PV(rate, nper, fv = 0, pmt = 0, type = 0)

PMT(rate, nper, pv, fv = 0, type = 0)

RATE(nper, pmt, pv, fv = 0, type = 0)
```

## Arguments

- cashflow:

  Cash flow values. When one value is provided, it's assumed constant
  cash flow.

- rate:

  One or more rate. When one rate is provided it's assumed constant
  rate.

- nper:

  Number of periods. When \`nper“ is provided, the cashflow values and
  rate are assumed constant.

- pv:

  Present value. Initial investments (cash inflows) are typically a
  negative value.

- pmt:

  Number of payments per period.

- type:

  Should payments (`pmt`) occur at the beginning (`type = 0`) or the end
  (`type = 1`) of each period.

- fv:

  Future value. Cash outflows are typically a positive value.

## Value

- Summary functions return a single value

## Details

**Net Present Value (NPV)** Net present value (NPV) is the difference
between the present value of cash inflows and the present value of cash
outflows over a period of time. NPV is used in capital budgeting and
investment planning to analyze the profitability of a projected
investment or project. For more information, see [Investopedia
NPV](https://www.investopedia.com/terms/n/npv.asp).

**Internal Rate of Return (IRR)** The internal rate of return (IRR) is a
metric used in capital budgeting to estimate the profitability of
potential investments. The internal rate of return is a discount rate
that makes the net present value (NPV) of all cash flows from a
particular project equal to zero. IRR calculations rely on the same
formula as NPV does. For more information, see [Investopedia
IRR](https://www.investopedia.com/terms/i/irr.asp).

**Future Value (FV)** Future value (FV) is the value of a current asset
at a future date based on an assumed rate of growth. The future value
(FV) is important to investors and financial planners as they use it to
estimate how much an investment made today will be worth in the future.
Knowing the future value enables investors to make sound investment
decisions based on their anticipated needs. However, external economic
factors, such as inflation, can adversely affect the future value of the
asset by eroding its value. For more information, see [Investopedia
FV](https://www.investopedia.com/terms/f/futurevalue.asp).

**Present Value (PV)** Present value (PV) is the current value of a
future sum of money or stream of cash flows given a specified rate of
return. Future cash flows are discounted at the discount rate, and the
higher the discount rate, the lower the present value of the future cash
flows. Determining the appropriate discount rate is the key to properly
valuing future cash flows, whether they be earnings or obligations. For
more information, see [Investopedia
PV](https://www.investopedia.com/terms/p/presentvalue.asp).

**Payment (PMT)** The Payment `PMT()` function calculates the payment
for a loan based on constant payments and a constant interest rate.

**Rate (RATE)** Returns the interest rate per period of a loan or an
investment. For example, use 6%/4 for quarterly payments at 6% APR.

## Examples

``` r
NPV(c(-1000, 250, 350, 450, 450), rate = 0.05)
#> [1] 314.4986

IRR(c(-1000, 250, 350, 450, 450))
#> [1] 0.1656206

FV(rate = 0.05, nper = 5, pv = -100, pmt = 0, type = 0)
#> [1] 127.6282

PV(rate = 0.05, nper = 5, fv = -100, pmt = 0, type = 0)
#> [1] 78.35262

PMT(nper = 20, rate = 0.05, pv = -100, fv = 0, type = 0)
#> [1] 8.024259

RATE(nper = 20, pmt = 8, pv = -100, fv = 0, type = 0)
#> [1] 0.04964019
```
