
# Functions ----

# Function to download latest stock indexes to R/sysdata.rda
# Data is used as a fail-safe if cannot be retrieved from www.marketvolume.com
re_run_fallback <- function() {

    index.option <- tq_index_options()
    group <- seq_along(index.option)
    date.downloaded <- rep(as.character(Sys.Date()), length.out = length(index.option))

    # Function to collect stock indexes from web
    get_stock_indexes <- function(group, index.option, date.downloaded) {
        stock_indexes <- tibble::tibble(
            group,
            index.option,
            date.downloaded
        ) %>%
            dplyr::mutate(index.components =
                              purrr::map(index.option,
                                         ~ tq_index(.x, use_fallback = FALSE))) %>%
            dplyr::select(group, index.option, index.components, date.downloaded)
        stock_indexes
    }

    if (!exists("stock_indexes")) {

        # if no stock_indexes, build the first instance

        stock_indexes <- get_stock_indexes(group, index.option, date.downloaded)

    } else {

        # Update stock_indexes with new data that can be downloaded

        stock_indexes_old <- stock_indexes
        names(stock_indexes_old) <- stringr::str_c(names(stock_indexes), ".old", sep = "")

        stock_indexes_new <- get_stock_indexes(group, index.option, date.downloaded)

        # Compare new data to old. If new data could not be retrieved, use old
        stock_indexes_comp <- dplyr::bind_cols(stock_indexes_new, stock_indexes_old) %>%
            dplyr::mutate(len = purrr::map_int(index.components, length)) %>%
            dplyr::mutate(index.components = ifelse(len != 1, index.components, index.components.old),
                          date.downloaded = ifelse(len != 1, date.downloaded, date.downloaded.old))

        stock_indexes <- stock_indexes_comp %>%
            dplyr::select(group:date.downloaded)

    }

    stock_indexes

}

# Function to get yahoo key statistic codes
run_yahoo_finance_tags <- function() {
    require(readxl)
    yahoo_tags <- readxl::read_excel("../yahoo.key.statistics.xlsx") %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(as.character)
    yahoo_tags
}

# TQ Performance -------

# Script ----

stock_indexes <- re_run_fallback()
yahoo_tags <- run_yahoo_finance_tags()

if (FALSE) {
    # If yahoo tags and stock indexes are updated
    readr::write_rds(stock_indexes, "data-raw/stock_index.RDS")
    readr::write_rds(yahoo_tags, "data-raw/yahoo_tags.RDS")
}

# To improve reproducibility, I saved stock_indexes and yahoo_tags as RDS files.
stock_indexes <- readr::read_rds("data-raw/stock_index.RDS")
yahoo_tags <- readr::read_rds("data-raw/yahoo_tags.RDS")

library(PerformanceAnalytics)
pkg_regex_table <- "^table"
funs_table <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_table)
funs_table <- ls("package:PerformanceAnalytics")[funs_table]
funs_table <- funs_table[!stringr::str_detect(funs_table, "(Drawdowns$|CalendarReturns$|ProbOutPerformance$)")] # remove table.Drawdowns

pkg_regex_capm <- "^CAPM"
funs_capm <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_capm)
funs_capm <- c(ls("package:PerformanceAnalytics")[funs_capm], "TimingRatio", "MarketTiming")

pkg_regex_sfm <- "^SFM"
funs_sfm <- stringr::str_detect(ls("package:PerformanceAnalytics"), pkg_regex_sfm)
funs_sfm <- ls("package:PerformanceAnalytics")[funs_sfm]

funs_VaR <- c("VaR", "ES", "ETL", "CDD", "CVaR")

funs_descriptive <- c("mean", "sd", "min", "max", "cor", "mean.geometric", "mean.stderr", "mean.LCL", "mean.UCL")

funs_annualized <- c("Return.annualized", "Return.annualized.excess", "sd.annualized", "SharpeRatio.annualized")

funs_moments <- c(
    "var", "cov", "skewness", "kurtosis", "CoVariance", "CoSkewness", "CoSkewnessMatrix",
    "CoKurtosis", "CoKurtosisMatrix", "M3.MM", "M4.MM", "BetaCoVariance", "BetaCoSkewness", "BetaCoKurtosis"
)

funs_drawdown <- c("AverageDrawdown", "AverageLength", "AverageRecovery", "DrawdownDeviation", "DrawdownPeak", "maxDrawdown")

funs_risk <- c("MeanAbsoluteDeviation", "Frequency", "SharpeRatio", "MSquared", "MSquaredExcess", "HurstIndex")

funs_regression <- c(
    "CAPM.alpha", "CAPM.beta", "CAPM.epsilon", "CAPM.jensenAlpha", "SystematicRisk",
    "SpecificRisk", "TotalRisk", "TreynorRatio", "AppraisalRatio", "FamaBeta",
    "Selectivity", "NetSelectivity"
)

funs_rel_risk <- c("ActivePremium", "ActiveReturn", "TrackingError", "InformationRatio")

funs_drw_dn <- c("PainIndex", "PainRatio", "CalmarRatio", "SterlingRatio", "BurkeRatio", "MartinRatio", "UlcerIndex")

funs_dside_risk <- c(
    "DownsideDeviation", "DownsidePotential", "DownsideFrequency", "SemiDeviation", "SemiVariance",
    "UpsideRisk", "UpsidePotentialRatio", "UpsideFrequency",
    "BernardoLedoitRatio", "DRatio", "Omega", "OmegaSharpeRatio", "OmegaExcessReturn", "SortinoRatio", "M2Sortino", "Kappa",
    "VolatilitySkewness", "AdjustedSharpeRatio", "SkewnessKurtosisRatio", "ProspectRatio"
)

funs_misc <- c("KellyRatio", "Modigliani", "UpDownRatios")

tq_performance_options <- list(
    table.funs = funs_table,
    CAPM.funs = funs_capm,
    SFM.funs = funs_sfm,
    descriptive.funs = funs_descriptive,
    annualized.funs = funs_annualized,
    VaR.funs = funs_VaR,
    moment.funs = funs_moments,
    drawdown.funs = funs_drawdown,
    Bacon.risk.funs = funs_risk,
    Bacon.regression.funs = funs_regression,
    Bacon.relative.risk.funs = funs_rel_risk,
    Bacon.drawdown.funs = funs_drw_dn,
    Bacon.downside.risk.funs = funs_dside_risk,
    misc.funs = funs_misc
)

library(zoo)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(TTR)
# zoo rollapply functions
pkg_regex_zoo <- "roll"
funs_zoo <- ls("package:zoo")[stringr::str_detect(ls("package:zoo"), pkg_regex_zoo)]

# xts apply.period, to.period, lag and diff functions
pkg_regex_xts <- "apply|to\\.|period|lag|diff"
funs_xts <- ls("package:xts")[stringr::str_detect(ls("package:xts"), pkg_regex_xts)]

# quantmod periodReturns, Delt, series functions
pkg_regex_quantmod <- "Return|Delt|Lag|Next|^Op..|^Cl..|^Hi..|^Lo..|^series"
funs_quantmod <- ls("package:quantmod")[stringr::str_detect(ls("package:quantmod"), pkg_regex_quantmod)]

# TTR functions
pkg_regex_ttr <- "^get*|^stock|^naCh" # NOT these
funs_ttr <- ls("package:TTR")[!stringr::str_detect(ls("package:TTR"), pkg_regex_ttr)]

# PerformanceAnalytics apply.rolling, Return...
pkg_PA <- "package:PerformanceAnalytics"
pkg_regex_PA <- "Return.annualized|Return.excess|Return.Geltner|Return.cumulative|Return.clean|zerofill"
funs_PA <- ls(pkg_PA)[stringr::str_detect(ls(pkg_PA), pkg_regex_PA)]



tq_transmute_options <- list(
    zoo = funs_zoo,
    xts = funs_xts,
    quantmod = funs_quantmod,
    TTR = funs_ttr,
    PerformanceAnalytics = funs_PA
)



usethis::use_data(stock_indexes, yahoo_tags,tq_performance_options, tq_transmute_options, internal = TRUE, overwrite = TRUE)
