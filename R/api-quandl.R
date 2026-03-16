#' Query or set Nasdaq Data Link API Key
#'
#' @param api_key Optionally passed parameter to set the Nasdaq Data Link
#'   `api_key`.
#'
#' @return Returns invisibly the currently set `api_key`.
#'
#' @details `quandl_api_key()` stores the API key used by tidyquant's
#'   built-in Nasdaq Data Link client. The option name remains
#'   `Quandl.api_key` for backward compatibility.
#'
#' @seealso [tq_get()] `get = "quandl"`
#'
#' @name quandl_api_key
#'
#' @export
#'
#' @examples
#' \dontrun{
#' quandl_api_key(api_key = "foobar")
#' }
NULL

#' @rdname quandl_api_key
#' @export
quandl_api_key <- function(api_key) {
    if (!missing(api_key)) {
        options(Quandl.api_key = api_key)
    }
    invisible(getOption("Quandl.api_key"))
}


#' Search the Nasdaq Data Link database
#'
#' @param query A character string giving the search term passed to the
#'   Nasdaq Data Link dataset search endpoint.
#' @param silent A logical indicating whether to suppress console output for
#'   matching datasets.
#' @param per_page An integer specifying how many search results to return per page.
#' @param ... Additional arguments passed to the Nasdaq Data Link search API.
#'
#' @return Returns a tibble with search results.
#'
#' @details A wrapper around the Nasdaq Data Link dataset search endpoint.
#'
#' @seealso [tq_get()] `get = "quandl"`
#'
#' @name quandl_search
#'
#' @export
#'
#' @examples
#' \dontrun{
#' quandl_search(query = "oil")
#' }
NULL

#' @rdname quandl_search
#' @export
quandl_search <- function(query, silent = FALSE, per_page = 10, ...) {
    tryCatch({
        params <- c(list(query = query, per_page = per_page), list(...))
        json <- quandl_api_call(path = "datasets", params = params)

        results <- tibble::as_tibble(json$datasets)

        if (nrow(results) == 0) {
            warning("No datasets found", call. = FALSE)
            return(results)
        }

        if (!silent) {
            for (i in seq_len(nrow(results))) {
                cols <- results$column_names[[i]]
                if (is.null(cols)) cols <- character()

                cat(
                    results$name[[i]], "\n",
                    "Code: ", results$database_code[[i]], "/", results$dataset_code[[i]], "\n",
                    "Desc: ", results$description[[i]], "\n",
                    "Freq: ", results$frequency[[i]], "\n",
                    "Cols: ", paste(cols, collapse = " | "), "\n\n",
                    sep = ""
                )
            }
        }

        results
    }, error = function(e) {
        warning(paste0("query = '", query, "': ", e$message), call. = FALSE)
        tibble::tibble()
    })
}


quandl_get <- function(code,
                       type = c("raw", "ts", "zoo", "xts", "timeSeries"),
                       transform = c("", "diff", "rdiff", "normalize", "cumul", "rdiff_from"),
                       collapse = c("", "daily", "weekly", "monthly", "quarterly", "annual"),
                       order = c("desc", "asc"),
                       meta = FALSE,
                       force_irregular = FALSE,
                       ...) {

    type <- match.arg(type)
    transform <- match.arg(transform)
    collapse <- match.arg(collapse)
    order <- match.arg(order)

    if (!identical(type, "raw")) {
        stop("Only type = 'raw' is supported.", call. = FALSE)
    }

    if (isTRUE(force_irregular)) {
        stop("force_irregular is not supported.", call. = FALSE)
    }

    params <- c(
        list(
            transform = transform,
            collapse = collapse,
            order = order
        ),
        list(...)
    )

    quandl_dataset_get(code = code, params = params, meta = meta)
}


quandl_datatable <- function(code, paginate = FALSE, ...) {
    path <- paste0("datatables/", code)
    quandl_datatable_perform(path = path, paginate = paginate, params = list(...))
}


quandl_dataset_get <- function(code, params, meta = FALSE) {
    code_parts <- quandl_normalize_code(code)
    dataset_code <- code_parts$code
    column_index <- code_parts$column_index

    if (!is.null(column_index)) {
        params$column_index <- column_index
    }

    path <- paste0("datasets/", dataset_code)
    json <- quandl_api_call(path = path, params = params)$dataset

    if (length(json$data) == 0) {
        stop("Requested Entity does not exist.", call. = FALSE)
    }

    if (!is.null(params$column_index) && length(json$column_names) > 2) {
        selected_cols <- c(1, as.numeric(params$column_index) + 1)
        json$column_names <- json$column_names[selected_cols]
    }

    data <- as.data.frame(json$data, stringsAsFactors = FALSE)
    names(data) <- json$column_names
    data[[1]] <- as.Date(data[[1]])

    if (ncol(data) > 1) {
        for (i in 2:ncol(data)) {
            data[[i]] <- as.numeric(data[[i]])
        }
    }

    if (isTRUE(meta)) {
        meta_data <- json
        meta_data$data <- NULL
        attr(data, "meta") <- meta_data
    }

    attr(data, "freq") <- json$frequency

    data
}


quandl_datatable_perform <- function(path, paginate, params) {
    json <- quandl_api_call(path = path, params = params)
    datatable <- json$datatable
    data <- datatable$data
    columns <- datatable$columns
    next_cursor_id <- json$meta$next_cursor_id

    df <- as.data.frame(data, stringsAsFactors = FALSE)

    while (isTRUE(paginate) && !is.null(next_cursor_id)) {
        params[["qopts.cursor_id"]] <- next_cursor_id
        json <- quandl_api_call(path = path, params = params)
        df_page <- as.data.frame(json$datatable$data, stringsAsFactors = FALSE)
        df <- rbind(df, df_page)
        next_cursor_id <- json$meta$next_cursor_id

        if (nrow(df) >= 1000000 && !is.null(next_cursor_id)) {
            warning(
                paste(
                    "This call returns a larger amount of data than tidyquant retrieves.",
                    "Please narrow the query or disable pagination."
                ),
                call. = FALSE
            )
            break
        }
    }

    if (!isTRUE(paginate) && !is.null(next_cursor_id)) {
        warning(
            paste(
                "This call returns more data.",
                "Set paginate = TRUE to request additional pages."
            ),
            call. = FALSE
        )
    }

    quandl_datatable_set_df_columns(df = df, columns = columns)
}


quandl_datatable_set_df_columns <- function(df, columns) {
    ncols <- length(columns[, 1])

    if (nrow(df) <= 0 && ncols > 0) {
        df <- data.frame(matrix(ncol = ncols, nrow = 0))
    }

    names(df) <- columns[, 1]

    quandl_datatable_convert_df_columns(df = df, column_types = columns[, 2])
}


quandl_datatable_convert_df_columns <- function(df, column_types) {
    if (length(column_types) <= 0) {
        return(df)
    }

    column_types <- tolower(column_types)

    for (i in seq_along(column_types)) {
        if (grepl("^float|^bigdecimal|^integer|^double", column_types[[i]])) {
            df[[i]] <- as.numeric(df[[i]])
        } else if (grepl("^datetime", column_types[[i]])) {
            df[[i]] <- as.POSIXct(df[[i]])
        } else if (grepl("^date", column_types[[i]])) {
            df[[i]] <- as.Date(df[[i]])
        } else {
            df[[i]] <- as.character(df[[i]])
        }
    }

    df
}


quandl_api_call <- function(path, params = list()) {
    request <- quandl_build_request(path = path, params = params)

    response <- httr::GET(
        request$url,
        config = do.call(httr::add_headers, request$headers),
        query = request$params
    )

    quandl_handle_errors(response)

    text_response <- httr::content(response, as = "text", encoding = "UTF-8")

    tryCatch(
        jsonlite::fromJSON(text_response, simplifyVector = TRUE),
        error = function(e) {
            stop(e, " Failed to parse response: ", text_response, call. = FALSE)
        }
    )
}


quandl_build_request <- function(path, params) {
    params <- quandl_build_query_params(params)
    params <- quandl_convert_dates_to_character(params)

    api_key <- quandl_api_key()
    if (!is.null(params$api_key)) {
        api_key <- params$api_key
        params$api_key <- NULL
    }

    headers <- list(
        Accept = "application/json",
        `Request-Source` = "R",
        `Request-Source-Version` = as.character(utils::packageVersion("tidyquant"))
    )

    if (!is.null(api_key)) {
        headers <- c(headers, list(`X-Api-Token` = api_key))
    }

    list(
        url = paste0("https://data.nasdaq.com/api/v3/", path),
        headers = headers,
        params = params
    )
}


quandl_handle_errors <- function(response) {
    status <- httr::status_code(response)
    if (status < 200 || status >= 300) {
        content <- httr::content(response, as = "text", encoding = "UTF-8")
        parsed <- tryCatch(
            jsonlite::fromJSON(content, simplifyVector = TRUE),
            error = function(e) NULL
        )

        message <- NULL
        if (status == 403 &&
            grepl("Incapsula|Request unsuccessful", content, ignore.case = TRUE)) {
            message <- paste(
                "Nasdaq Data Link blocked this request before API authentication",
                "(HTTP 403 / edge security challenge).",
                "This is typically a site-side access restriction rather than an invalid API key."
            )
        }
        if (!is.null(parsed$quandl_error$message)) {
            message <- parsed$quandl_error$message
        }
        if (is.null(message) && !is.null(parsed$errors$message)) {
            message <- paste(parsed$errors$message, collapse = "; ")
        }
        if (is.null(message)) {
            message <- paste("Nasdaq Data Link request failed with status", status)
        }

        stop(message, call. = FALSE)
    }
}


quandl_build_query_params <- function(params) {
    if (length(params) <= 0) {
        return(params)
    }

    modified <- list()

    for (i in seq_along(params)) {
        converted <- params[i]
        if (length(params[[i]]) > 1) {
            converted <- quandl_convert_vector_params(names(params[i]), params[[i]])
        }
        modified <- c(modified, converted)
    }

    modified
}


quandl_convert_vector_params <- function(name, values) {
    query_name <- paste0(name, "[]")
    modified <- list()

    for (value in values) {
        item <- list()
        item[[query_name]] <- value
        modified <- c(modified, item)
    }

    modified
}


quandl_convert_dates_to_character <- function(params) {
    lapply(params, function(param) {
        if (inherits(param, "Date")) {
            return(as.character(param))
        }
        param
    })
}


quandl_normalize_code <- function(code) {
    if (!is.character(code) || length(code) != 1) {
        stop("code must be a single character string.", call. = FALSE)
    }

    if (!identical(gsub("[^A-Z0-9_./]", "", code), code)) {
        stop("Codes are comprised of capital letters, numbers and underscores only.", call. = FALSE)
    }

    code_parts <- strsplit(code, "/")[[1]]
    column_index <- NULL

    if (length(code_parts) == 3) {
        column_index <- code_parts[[3]]
        code <- paste(code_parts[1:2], collapse = "/")
    } else {
        dot_parts <- strsplit(code, "\\.")[[1]]
        if (length(dot_parts) == 2) {
            column_index <- dot_parts[[2]]
            code <- dot_parts[[1]]
        } else if (length(dot_parts) == 3) {
            column_index <- dot_parts[[3]]
            code <- paste(dot_parts[1:2], collapse = "/")
        }
    }

    list(code = code, column_index = column_index)
}
