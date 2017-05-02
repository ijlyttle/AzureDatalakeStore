#' Rename a file/directory.
#'
#'
#' @inheritParams adls_mkdirs
#' @param destination          `character`, remote path for the destination,
#' defined with respect to root-path in `adls$base_url`.
#'
#' @return A `logical` indicating the success of the operation.
#' @seealso
#'   WebHDFS documentation for ["Rename a File/Directory"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Rename_a_FileDirectory)
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
#'   # reanme the newly-created directory
#'   adls_rename(adls_example, "baz", "baz_new")
#' }
#' @export
#'
adls_rename <- function(adls, path, destination) {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    is.character(path) && identical(length(path), 1L),
    is.character(destination) && identical(length(destination), 1L)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "RENAME",
      destination = destination
    )

  response <-
    url %>%
    httr::PUT(
      httr::content_type_json(),
      httr::accept_json(),
      config = httr::config(token = adls$token)
    ) %>%
    httr::stop_for_status(
      task = "rename file/directory on Azure Datalake store"
    )

  result <-
    response %>%
    unpack_response() %>%
    `[[`("boolean")

  result
}
