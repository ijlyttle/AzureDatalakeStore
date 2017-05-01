#' Create and write to a file.
#'
#' @inheritParams adls_mkdirs
#' @param offset numeric, the position of the starting-byte
#' @param length numeric, the number of bytes to be processed
#' @param encoding character, encoding to use
#'
#' @return response object from [`httr::VERB()`]
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
adls_open_to_text <- function(adls, path, offset = NULL, length = NULL,
                              encoding = "UTF-8") {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    is.character(path) && identical(length(path), 1L),
    is.numeric(offset) || is.null(offset),
    is.numeric(length) || is.null(length)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "OPEN",
      read = "true",
      offset = offset,
      length = length
    )

  response <-
    url %>%
    httr::GET(
     config = httr::config(token = adls$token)
    ) %>%
    httr::stop_for_status(
      task = "open file on Azure Datalake store"
    )

  result <-
    response %>%
    httr::content(as = "text", encoding = encoding)

  result
}
