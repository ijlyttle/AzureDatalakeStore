#' Delete a file/directory.
#'
#'
#' @inheritParams adls_mkdirs
#' @param recursive            `logical`, indicates if operation will act on
#'   content in subdirectories
#'
#' @return A `logical` indicating the success of the operation.
#' @seealso
#'   WebHDFS documentation for ["Delete a File/Directory"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Delete_a_FileDirectory)
#' @examples
#' \dontrun{
#'   library("AzureOAuth")
#'
#'   # create token (assumes Azure native app)
#'   token <- oauth_token_azure(
#'     tenant_id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
#'     application_id = "ffffffff-gggg-hhhh-iiii-jjjjjjjjjjjj",
#'     name = "foo"
#'   )
#'
#'   # create adls object with base-URL and token
#'   adls_example <- adls(
#'     base_url = adls_url("bar"),
#'     token = token
#'   )
#'
#'   # create a directory
#'   adls_mkdirs(adls_example, "baz")
#'
#'   # delete the newly-created directory
#'   adls_delete(adls_example, "baz")
#' }
#' @export
#'
adls_delete <- function(adls, path = NULL, recursive = FALSE) {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    (is.character(path) && identical(length(path), 1L)) || is.null(path),
    is.logical(recursive)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "DELETE",
      recursive = lgl_to_char(recursive)
    )

  # hack to compose URL properly for root directory
  if (is.null(path)) {
    len <- length(url$path)
    url$path[len] <- paste0(url$path[len], "/")
  }

  response <-
    url %>%
    httr::DELETE(
      httr::content_type_json(),
      httr::accept_json(),
      config = httr::config(token = adls$token)
    ) %>%
    httr::stop_for_status(
      task = "delete file/directory on Azure Datalake store"
    )

  result <-
    response %>%
    unpack_response() %>%
    `[[`("boolean")

  result
}
