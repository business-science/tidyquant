# Taken from tidyverse
core <- c("xts", "quantmod", "TTR", "PerformanceAnalytics")

core_unloaded <- function() {
    search <- paste0("package:", core)
    core[!search %in% search()]
}

# Attach the package from the same package library it was
# loaded from before. https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
    loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
    library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

tidyquant_attach <- function() {
    to_load <- core_unloaded()

    suppressPackageStartupMessages(
        lapply(to_load, same_library)
    )

    invisible(to_load)
}

tidyquant_attach_message <- function(to_load) {
    if (length(to_load) == 0) {
        return(NULL)
    }

    header <- cli::rule(
        left = cli::style_bold("Attaching core tidyquant packages"),
        right = paste0("tidyquant ", package_version_h("tidyquant"))
    )

    to_load <- sort(to_load)
    versions <- vapply(to_load, package_version_h, character(1))

    packages <- paste0(
        cli::col_green(cli::symbol$tick), " ", cli::col_blue(format(to_load)), " ",
        cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
    )

    if (length(packages) %% 2 == 1) {
        packages <- append(packages, "")
    }
    col1 <- seq_len(length(packages) / 2)
    info <- paste0(packages[col1], "     ", packages[-col1])

    paste0(header, "\n", paste(info, collapse = "\n"))
}

package_version_h <- function(pkg) {
    highlight_version(utils::packageVersion(pkg))
}

highlight_version <- function(x) {
    x <- as.character(x)

    is_dev <- function(x) {
        x <- suppressWarnings(as.numeric(x))
        !is.na(x) & x >= 9000
    }

    pieces <- strsplit(x, ".", fixed = TRUE)
    pieces <- lapply(pieces, function(x) ifelse(is_dev(x), cli::col_red(x), x))
    vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}

#' Conflicts between the tidyquant and other packages
#'
#' This function lists all the conflicts between packages in the tidyverse
#' and other packages that you have loaded.
#'
#' There are four conflicts that are deliberately ignored: \code{intersect},
#' \code{union}, \code{setequal}, and \code{setdiff} from dplyr. These functions
#' make the base equivalents generic, so shouldn't negatively affect any
#' existing code.
#'
#' @export
#' @param only Set this to a character vector to restrict to conflicts only
#'   with these packages.
#' @examples
#' tidyquant_conflicts()
tidyquant_conflicts <- function(only = NULL) {
    envs <- grep("^package:", search(), value = TRUE)
    envs <- purrr::set_names(envs)

    if (!is.null(only)) {
        only <- union(only, core)
        envs <- envs[names(envs) %in% paste0("package:", only)]
    }

    objs <- invert(lapply(envs, ls_env))

    conflicts <- purrr::keep(objs, ~ length(.x) > 1)

    tidy_names <- paste0("package:", tidyquant_packages())
    conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

    conflict_funs <- purrr::imap(conflicts, confirm_conflict)
    conflict_funs <- purrr::compact(conflict_funs)

    structure(conflict_funs, class = "tidyquant_conflicts")
}

tidyquant_conflict_message <- function(x) {
    header <- cli::rule(
        left = cli::style_bold("Conflicts"),
        right = "tidyquant_conflicts()"
    )

    pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
    others <- pkgs %>% purrr::map(`[`, -1)
    other_calls <- purrr::map2_chr(
        others, names(others),
        ~ paste0(cli::col_blue(.x), "::", .y, "()", collapse = ", ")
    )

    winner <- pkgs %>% purrr::map_chr(1)
    funs <- format(paste0(cli::col_blue(winner), "::", cli::col_green(paste0(names(x), "()"))))
    bullets <- paste0(
        cli::col_red(cli::symbol$cross), " ", funs, " masks ", other_calls,
        collapse = "\n"
    )

    conflicted <- paste0(
        cli::col_cyan(cli::symbol$info), " ",
        cli::format_inline("Use the {.href [conflicted package](http://conflicted.r-lib.org/)} to force all conflicts to become errors"
        ))

    paste0(
        header, "\n",
        bullets, "\n",
        conflicted
    )
}

#' @export
print.tidyquant_conflicts <- function(x, ..., startup = FALSE) {
    cli::cat_line(tidyquant_conflict_message(x))
    invisible(x)
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
    # Only look at functions
    objs <- packages %>%
        purrr::map(~ get(name, pos = .)) %>%
        purrr::keep(is.function)

    if (length(objs) <= 1)
        return()

    # Remove identical functions
    objs <- objs[!duplicated(objs)]
    packages <- packages[!duplicated(packages)]
    if (length(objs) == 1)
        return()

    packages
}

ls_env <- function(env) {
    x <- ls(pos = env)

    # intersect, setdiff, setequal, union come from generics
    if (env %in% c("package:dplyr", "package:lubridate")) {
        x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
    }

    if (env == "package:lubridate") {
        x <- setdiff(x, c(
            "as.difftime", # lubridate makes into an S4 generic
            "date"         # matches base behaviour
        ))
    }

    x
}

inform_startup <- function(msg, ...) {
    if (is.null(msg)) {
        return()
    }
    if (isTRUE(getOption("tidyquant.quiet"))) {
        return()
    }

    rlang::inform(msg, ..., class = "packageStartupMessage")
}
tidyquant_packages <- function(include_self = TRUE) {
    raw <- utils::packageDescription("tidyquant")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

    if (include_self) {
        names <- c(names, "tidyquant")
    }

    names
}

invert <- function(x) {
    if (length(x) == 0) return()
    stacked <- utils::stack(x)
    tapply(as.character(stacked$ind), stacked$values, list)
}

