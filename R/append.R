#' Append to a file.
#'
#' @inheritParams adls_create
#'
#' @return A logical indicating success of the operation.
#' @seealso [`adls()`], [`adls_url()`]
#'   WebHDFS documentation for ["Open and Read a File"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Open_and_Read_a_File)
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
#'   # upload file
#'   adls_mkdirs(adls_example, "baz")
#' }
#' @export
#'
adls_append <- function(adls, file, path) {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    inherits(file, "form_file"),
    assertthat::is.string(path)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "APPEND",
      write = "true",
      append = "true"
    )

  response <-
    url %>%
    httr::POST(
      body = file,
      config = httr::config(token = adls$token)
    )

  response
}
