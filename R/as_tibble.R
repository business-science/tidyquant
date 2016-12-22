#' Coerce to tibble. Enable preserving row names when coercing matrix
#' and time-series-like objects with row names to tibbles
#'
#' @param x A list, matrix, xts, zoo, timeSeries, etc object.
#' @param preserve_row_names When \code{TRUE}, creates a row.names column with
#' names of rows.
#' @param ... Additional parameters passed to the appropriate
#' \code{\link[tibble]{as_tibble}}
#' function.
#'
#' @return Returns a \code{tibble} object.
#'
#' @details \code{tidyquant::as_tibble} is a wrapper for \code{\link[tibble]{as_tibble}}
#' that includes a \code{preserve_row_names} argument. When \code{preserve_row_names = TRUE},
#' a new column, \code{row.names}, is created during object coercion.
#'
#' It is possible to convert \code{xts}, \code{zoo}, \code{timeSeries}, \code{ts},
#' and \code{irts} objects.
#'
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyquant)
#'
#' m <- matrix(rnorm(50), ncol = 5)
#' colnames(m) <- c("a", "b", "c", "d", "e")
#' rownames(m) <- letters[1:nrow(m)]
#' m_tbl <- as_tibble(m, preserve_row_names = TRUE)
#'

as_tibble <- function(x, preserve_row_names = FALSE, ...) {

    # Handle various time series objects
    if (xts::is.xts(x) ||
        zoo::is.zoo(x) ||
        timeSeries::is.timeSeries(x) ||
        stats::is.ts(x)) {

        ret <- matrix_to_tibble(as.matrix(x), preserve_row_names, ...)

    } else if (tseries::is.irts(x)) {

        ret <- matrix_to_tibble(x %>% xts::as.xts() %>% as.matrix(),
                                preserve_row_names, ...)

    } else if (is.matrix(x)) {

        ret <- matrix_to_tibble(x, preserve_row_names, ...)

    } else {

        ret <- tibble::as_tibble(x, ...)

    }

    ret

}

# UTILITY FUNCTIONS FOR as_tibble()

# matrix to tibble conversion:
#     allow preserving rownames when converting to tibble
matrix_to_tibble <- function(x, preserve_row_names, ...) {

    if (!is.matrix(x)) {
        stop("Error: `x` is not a matrix object.")
    }

    if (preserve_row_names == TRUE) {

        row.names <- rownames(x)

        # Detect if row.names exist beyond sequential 1:nrow(x) or null value
        if (!identical(row.names, 1:nrow(x) %>% as.character()) &&
            !is.null(row.names)) {

            dplyr::bind_cols(
                tibble::tibble(row.names),
                tibble::as_tibble(x, ...)
            )

        } else {

            warning("Warning: No row names to preserve")
            tibble::as_tibble(x, ...)
        }

    } else {

        tibble::as_tibble(x, ...)

    }

}


