# AnVIL/R/av.R
#' @importFrom httr status_code http_condition headers
.stop_for_status <-
    function(response, op)
{
    status <- status_code(response)
    if (status < 400L)
        return(invisible(response))

    cond <- http_condition(status, "error")
    type <- headers(response)[["content-type"]]
    msg <- NULL
    if (nzchar(type) && grepl("application/json", type)) {
        content <- as.list(response)
        msg <- content[["message"]]
        if (is.null(msg))
            ## e.g., from bond DRS server
            msg <- content$response$text
    } else if (nzchar(type) && grepl("text/html", type)) {
        ## these pages can be too long for a standard 'stop()' message
        cat(as.character(response), file = stderr())
    }

    message <- paste0(
        "'", op, "' failed:\n  ",
        conditionMessage(cond),
        if (!is.null(msg)) "\n  ", msg
    )
    stop(message, call.=FALSE)
}

## from AnVILGCP
.az_shQuote <- function(source) {
    ## Expand local paths with ~ or . or .. to full path names.
    ## Needed because we also use shQuote() (to allow for spaces in
    ## file names), and shQuote() would otherwise use paths with ~ or
    ## . in the current working directory.
    source <- normalizePath(source)
    shQuote(source)
}
