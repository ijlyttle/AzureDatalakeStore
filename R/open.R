#' Create and write to a file.
#'
#' @inheritParams adls_mkdirs
#' @param offset numeric, the position of the starting-byte
#' @param length numeric, the number of bytes to be processed
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
adls_open_to_raw <- function(adls, path, offset = NULL, length = NULL) {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    assertthat::is.string(path),
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
    httr::content(as = "raw")

  result
}

#' @param encoding character, encoding to use
#' @rdname adls_open_to_raw
#' @export
#'
adls_open_to_text <- function(adls, path, encoding = "UTF-8",
                              offset = NULL, length = NULL) {

  # validate inputs
  assertthat::assert_that(
    assertthat::is.string(encoding)
  )

  result <-
    adls_open_to_raw(
      adls = adls,
      path = path,
      offset = offset,
      length = length
    ) %>%
    readBin(character()) %>%
    iconv(from = encoding, to = "UTF-8")

  result
}

#' @param path_local character, path to file on local computer
#' @rdname adls_open_to_raw
#' @export
#'
adls_open_to_file <- function(adls, path, path_local,
                              offset = NULL, length = NULL) {

  # validate inputs
  assertthat::assert_that(
    assertthat::is.string(path_local)
  )

  adls_open_to_raw(
    adls = adls,
    path = path,
    offset = offset,
    length = length
  ) %>%
  writeBin(con = path_local)

  TRUE
}


