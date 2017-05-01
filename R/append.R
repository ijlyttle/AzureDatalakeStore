#' Create and write to a file.
#'
#' @inheritParams adls_mkdirs
#' @param form_file  form_file made using [`httr::upload_file()`] or [`curl::form_file()`]
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
adls_append <- function(adls, form_file, path) {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    inherits(form_file, "form_file"),
    assertthat::is.string(path)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "APPEND",
      write = "true"
    )

  # note: write = "true" is not in the WebHDFS documentation, but it does appear here:
  #
  # https://blogs.msdn.microsoft.com/microsoftrservertigerteam/2017/03/14/using-r-to-perform-filesystem-operations-on-azure-data-lake-store/
  #
  # maybe this is some sort of local (Azure) modification?

  response <-
    url %>%
    httr::POST(
      body = form_file,
      config = httr::config(token = adls$token)
    ) %>%
    httr::stop_for_status(
      response,
      task = "append file on Azure Datalake store"
    )

  TRUE
}
