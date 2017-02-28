#' Coerce to tibble. Enable preserving row names when coercing matrix
#' and time-series-like objects with row names.
#'
#' @param x A list, matrix, xts, zoo, timeSeries, etc object.
#' @param preserve_row_names Used during coercion from matrix, xts, zoo,
#' timeSeries, etc objects that have row names. When `TRUE`, creates
#' a `row.names` column with names of rows as character class.
#' @param ... Additional parameters passed to the appropriate
#' [tibble::as_tibble()] function.
#'
#' @return Returns a `tibble` object.
#'
#' @details `as_tibble` is a wrapper for `tibble::as_tibble`
#' that includes a `preserve_row_names` argument. The function is designed
#' to coerce `xts`, `zoo`, `timeSeries`, `ts`, and `irts`
#' objects that are used frequently in quantitative financial analysis.
#' When `preserve_row_names = TRUE` is specified, a new column,
#' `row.names`, is created during object coercion as a character class.
#'
#' @seealso [as_xts()]
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyverse)
#' library(tidyquant)
#'
#' # Matrix coercion to tibble
#' m <- matrix(rnorm(50), ncol = 5)
#' colnames(m) <- c("a", "b", "c", "d", "e")
#' rownames(m) <- letters[1:nrow(m)]
#' m_tbl <- as_tibble(m, preserve_row_names = TRUE)
#'
#' # xts coercion to tibble
#' quantmod::getSymbols("AAPL", auto.assign = FALSE) %>%
#'     as_tibble(preserve_row_names = TRUE)
#'
as_tibble <- function(x, preserve_row_names = FALSE, ...) {

    warn <- FALSE

    # Handle various time series objects
    ret <- tryCatch({

        if (xts::is.xts(x) ||
            zoo::is.zoo(x) ||
            timeSeries::is.timeSeries(x) ||
            stats::is.ts(x)) {

            matrix_to_tibble(as.matrix(x), preserve_row_names, ...)

        } else if (tseries::is.irts(x)) {

            matrix_to_tibble(x %>% xts::as.xts() %>% as.matrix(),
                                    preserve_row_names, ...)

        } else if (is.matrix(x)) {

            matrix_to_tibble(x, preserve_row_names, ...)

        } else {

            if (preserve_row_names) warn <- TRUE
            tibble::as_tibble(x, ...)

        }

    }, error = function(e) {

        warning(paste0("Error at ", x,
                       ": Could not convert to tibble. Check input."))
        NA

    })

    if (warn) {
        warning(paste0("\nWarning at input ", x, ":",
                       "\nPreserving row names not used for this object class.",
                       " Object was otherwise converted to tibble successfully."))
    }

    ret

}

# UTILITY FUNCTIONS ----

# matrix to tibble conversion:
#     allow preserving rownames when converting to tibble
matrix_to_tibble <- function(x, preserve_row_names, ...) {

    if (!is.matrix(x)) stop("Error: `x` is not a matrix object.")

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

            warning(paste0("Warning: No row names to preserve. ",
                           "Object otherwise converted to tibble successfully."))
            tibble::as_tibble(x, ...)
        }

    } else {

        tibble::as_tibble(x, ...)

    }
}


