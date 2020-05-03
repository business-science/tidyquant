
# HELPER FUNCTIONS -----

validate_numericish <- function(x, function_label) {

    if ( !is_numericish(x) ) {
        stop(paste0(function_label, "(): input data type must be numeric or logical. Type supplied is: ", class(x)[[1]]), call. = FALSE)
    }
}

is_numericish <- function(x) {
    ret <- (is.numeric(x)) | (is.logical(x))
    return(ret)
}
