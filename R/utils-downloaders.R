


csv_downloader <- function(path, ...) {

    # Sample Path:
    # path <-  "https://www.invesco.com/us/financial-products/etfs/holdings?audienceType=Investor&ticker=QQQ"
    # path <- "https://www.invesco.com/us/financial-products/etfs/holdings/main/holdings/0?audienceType=Investor&action=download&ticker=QQQ"

    # Results container
    res <- list(df = NULL, err = NULL)

    # Download quietly
    tryCatch({

        # Establish connection
        handle <- curl::new_handle(verbose = FALSE)
        curl::handle_setopt(
            handle,
            useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:79.0) Gecko/20100101 Firefox/79.0"
        )

        con <- curl::curl(
            url    = path,
            handle = handle
        )

        # Read the csv file
        suppressWarnings({
            suppressMessages({
                res$df <- readr::read_csv(con, ...)
            })
        })

        return(res)

    }, error = function(e) {

        # On error, catch it and return
        res$err <- paste0("Error during download. \n", e)

        return(res)

    })
}


excel_downloader <- function(path, ...) {

    # Sample Path:
    # path <- paste0("https://www.ssga.com/us/en/institutional/etfs/library-content/products/fund-data/etfs/us/holdings-daily-us-en-", tolower(x), ".xlsx")

    # Results container
    res <- list(df = NULL, err = NULL)

    # Download quietly
    tryCatch({

        # Download to disk, force as a xlsx
        httr::GET(
            url = path,
            httr::write_disk(tf <- tempfile(fileext = ".xlsx")),
            httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:79.0) Gecko/20100101 Firefox/79.0")
        )

        # Read the xls file
        suppressMessages({
            res$df <- readxl::read_xlsx(tf, ...)
        })

        # Release temp file
        unlink(tf)

        return(res)

    }, error = function(e) {

        # On error, catch it and return
        res$err <- paste0("Error during download. \n", e)

        return(res)

    })
}
