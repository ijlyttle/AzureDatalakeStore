#' Make a directory.
#'
#' Apparently returns `TRUE` if folder already exists.
#'
#' @param adls        `adls` S3 object, generated using [`adls()`]
#' @param path        character, directory to act on, defined with respect to path
#'   defined in `adls$base_url` - [`file.path()`] can be used here
#' @param permission  integer (octal form), permissions to assign to the directory
#'
#' @return logical, indicating success
#' @seealso [`adls()`], [`adls_url()`]
#'   WebHDFS documentation for ["Make a Directory"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Make_a_Directory)
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
