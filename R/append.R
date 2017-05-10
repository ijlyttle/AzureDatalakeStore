#' Append to a file.
#'
#' @inheritParams adls_create
#'
#' @return A `logical` indicating success of the operation.
#' @seealso
#'   WebHDFS documentation for ["Append to a File"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Append_to_a_File)
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
#'   # for this example, write a temporary file
#'   temp_file <- tempfile(fileext = ".csv")
#'   write.csv(iris, file = temp_file)
#'
#'   # upload file
#'   adls_create(
#'     adls_example,
#'     file = httr::file_upload(temp_file),
#'     path = "iris.csv"
#'   )
#'
#'   # append with another copy of iris.csv
#'   adls_append(
#'     adls_example,
#'     file = httr::file_upload(temp_file),
#'     path = "iris.csv"
#'   )
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
      config = httr::config(token = adls$token),
      add_headers(`Transfer-Encoding` = "chunked")
    ) %>%
    httr::stop_for_status(
      task = "append to file on Azure Datalake store"
    )

  TRUE
}
