#' @name avdata-methods
#'
#' @title Azure "Reference" and "Other" Data methods on AnVIL
#'
#' @description This file contains methods for working with "Reference" and
#'   "Other" data on AnVIL.
#'
#' @inheritParams azure-methods
#'
#' @include azure-class.R
#'
NULL

# avdata -----------------------------------------------------------------

#' @describeIn avdata-methods List the available "Reference" and "Other" data
#'
#' @importFrom AnVILBase avdata
#' @exportMethod avdata
setMethod("avdata", signature = c(platform = "azure"),
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace), isScalarCharacter(name)
        )

        name <- utils::URLencode(name)

        api_endpoint <-
            "/api/workspaces/{{workspaceNamespace}}/{{workspaceName}}"
        url <- paste0(.LEONARDO_URL, api_endpoint)
        url <- whisker.render(url)

        response <- request(url) |>
            req_auth_bearer_token(az_token()) |>
            req_perform() |>
            resp_body_json()

        content <- response[["workspace"]][["attributes"]]

        ## a workspace DATA element may be preceded by the 'workspace:'
        ## tag, remove it
        names(content) <- sub("^workspace:", "", names(content))
        ## remove non-DATA attributes. `description` is from the workspace
        ## landing page. The `:` seems to be used as a delimiter, e.g.,
        ## `tag:tags`
        exclude <-
            names(content) %in% "description" |
            grepl("^[a-z]+:", names(content))
        content <- content[!exclude]

        ## some elements are lists, e.g., a vector of google
        ## buckets. Translate these to their character(1) representation,
        ## so the tibble has a column of type <chr> and shows the value of
        ## the character(1) entries, rather than a column of type list
        ## showing "chr(1)" for most elements
        is_character <- vapply(content, is.character, logical(1))
        content[!is_character] <- vapply(
            content[!is_character],
            ## list-like elements usually have a key-value structure, use
            ## the value
            function(x) jsonlite::toJSON(unlist(x[["items"]], use.names = FALSE)),
            character(1)
        )

        ## create the referenceData tibble; 'referenceData' keys start
        ## with "referenceData_"
        referenceData_id <- "referenceData_"
        referenceData_regex <- "^referenceData_([^_]+)_(.*)$"
        is_referenceData <- startsWith(names(content), referenceData_id)
        referenceData <- content[is_referenceData]
        referenceData_tbl <- tibble::tibble(
            type = rep("reference", length(referenceData)),
            table = sub(referenceData_regex, "\\1", names(referenceData)),
            key = sub(referenceData_regex, "\\2", names(referenceData)),
            value = as.character(unlist(referenceData, use.names = FALSE))
        )

        ## 'other' data
        otherData <- content[!is_referenceData]
        otherData_tbl <- tibble::tibble(
            type = "other",
            table = "workspace",
            key = names(otherData),
            value = as.character(unlist(otherData, use.names = FALSE))
        )

        if (!requireNamespace("dplyr", quietly = TRUE)) {
            do.call(rbind, list(otherData_tbl, referenceData_tbl))
        } else {
            dplyr::bind_rows(otherData_tbl, referenceData_tbl)
        }
    }
)
