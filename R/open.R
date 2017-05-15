#' Create and write to a file.
#'
#' @inheritParams adls_mkdirs
#' @param offset `numeric`, the position of the starting-byte.
#' @param length `numeric`, the number of bytes to be processed.
#'
#'
#' @return
#' \describe{
#'   \item{`adls_open_to_raw()`}{A `raw` vector.}
#' }
#' @seealso
#'   WebHDFS documentation for ["Open and Read a File"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Open_and_Read_a_File)
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
#'   # read file to a string
#'   adls_open_to_text(
#'     adls_example,
#'     path = "iris.csv"
#'   )
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
      # read = "true",
      offset = offset,
      length = length
    )

  response <-
    url %>%
    httr::GET(
     config = httr::config(followlocation = 1L, token = adls$token)
    ) %>%
    httr::stop_for_status(
      task = "open file on Azure Datalake store"
    )

  result <-
    response %>%
    httr::content(as = "raw")

  result
}

#' @rdname adls_open_to_raw
#'
#' @param encoding `character`, encoding to use.
#'
#' @return
#' \describe{
#'   \item{`adls_open_to_text()`}{A single `character` string.}
#' }
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

#' @rdname adls_open_to_raw
#'
#' @param path_local `character`, path on local computer.
#'
#' @return
#' \describe{
#'   \item{`adls_open_to_file()`}{A `logical` indicating success of the operation.}
#' }
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


