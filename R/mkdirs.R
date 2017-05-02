#' Make a directory.
#'
#' @param adls        `adls` S3 object, generated using [`adls()`].
#' @param path        `character`, remote path, defined with respect to root-path
#'   in `adls$base_url`. You may find [`file.path()`] to be useful to compose the path.
#' @param permission  `integer` (octal form), permission to assign to the object.
#'
#' @return logical, indicating success
#' @seealso
#'   WebHDFS documentation for ["Make a Directory"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Make_a_Directory)
#' @examples
#' \dontrun{
#'   # create token (assumes Azure native app)
#'   token <- AzureOAuth::oauth_token_azure(
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
#' }
#' @export
#'
adls_mkdirs <- function(adls, path, permission = NULL) {

  if (!is.null(permission)) {
    permission <- as.integer(permission)
  }

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    is.character(path) && identical(length(path), 1L),
    is.integer(permission) || is.null(permission)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "MKDIRS",
      permission = permission
    )

  response <-
    url %>%
    httr::PUT(
      httr::content_type_json(),
      httr::accept_json(),
      config = httr::config(token = adls$token)
    ) %>%
    httr::stop_for_status(
      task = "make directory on Azure Datalake store"
    )

  result <-
    response %>%
    unpack_response() %>%
    `[[`("boolean")

  result
}
