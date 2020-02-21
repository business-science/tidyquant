

SUM <- function(x) {
    sum(x, na.rm = TRUE)
}

AVERAGE <- function(x) {
    mean(x, na.rm = TRUE)
}

MEDIAN <- function(x) {
    median(x, na.rm = TRUE)
}

MIN <- function(x) {
    min(x, na.rm = TRUE)
}

MAX <- function(x) {
    max(x, na.rm = TRUE)
}

MIN_PARALLEL <- function(...) {
    pmin(..., na.rm = TRUE)
}

MAX_PARALLEL <- function(...) {
    pmax(..., na.rm = TRUE)
}

ABS <- function(x) {
    abs(x)
}

SQRT <- function(x) {
    sqrt(x)
}

COUNT <- function(x) {
    length(x[!is.na(x)])
}




