#' @name avdata-methods
#'
#' @aliases avdata avdata_import
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
#' @examples
#' if (az_exists() && identical(get_platform(), "AnVILAz")) {
#'    ## from within AnVIL
#'    data <- avdata()
#'    avdata_import(data)
#' }
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

        response <- request(.RAWLS_URL) |>
            req_template(
                "/api/workspaces/{workspaceNamespace}/{workspaceName}",
                workspaceNamespace = namespace,
                workspaceName = name
            ) |>
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

# avdata_import -----------------------------------------------------------

#' @describeIn avdata-methods Import "Reference" and "Other" data to an AnVIL
#'   workspace
#'
#' @importFrom AnVILBase avdata_import
#' @exportMethod avdata_import
setMethod("avdata_import", signature = c(platform = "azure"), definition =
    function(
        .data, namespace = avworkspace_namespace(), name = avworkspace_name(),
        ..., platform = cloud_platform()
    ) {
        stopifnot(
            is.data.frame(.data),
            all(c("type", "table", "key", "value") %in% names(.data)),
            all(vapply(
                select(.data, "type", "table", "key", "value"),
                is.character,
                logical(1)
            )),
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )

        .data <- subset(
            .data, .data$type == "other" & .data$table %in% "workspace"
        )

        if (!nrow(.data)) {
            message(
                "'avdata_import()' has no rows of type 'other' and ",
                "table 'workspace'"
            )
            return(invisible(.data))
        }

        request(.RAWLS_URL) |>
            req_template(
                "/api/workspaces/{workspaceNamespace}/{workspaceName}",
                workspaceNamespace = namespace,
                workspaceName = name
            ) |>
            req_auth_bearer_token(az_token()) |>
            req_method("PATCH") |>
            req_body_json(
                data.frame(
                    op = "AddUpdateAttributes",
                    attributeName = .data$key,
                    addUpdateAttribute = .data$value,
                )
            ) |>
            req_perform()

        invisible(.data)
    }
)
