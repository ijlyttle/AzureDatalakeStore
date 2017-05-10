#' Create and write to a file.
#'
#' @inheritParams adls_mkdirs
#' @param file  `form_file` S3 object, representing the "thing" to be uploaded.
#' You can compose this using [`httr::upload_file()`] or [`curl::form_file()`].
#' @param overwrite  logical, indicating if existing (remote) file is to be overwritten.
#'
#' @return A `logical` indicating success of the operation.
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
#' }
#' @export
#'
adls_create <- function(adls, file, path, overwrite = FALSE, permission = NULL) {

  if (!is.null(permission)) {
    permission <- as.integer(permission)
  }

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    inherits(file, "form_file"),
    is.character(path) && identical(length(path), 1L),
    is.logical(overwrite),
    is.integer(permission) || is.null(permission)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(
      op = "CREATE",
      write = "true",
      overwrite = lgl_to_char(overwrite),
      permission = permission
    )

  # note: write = "true" is not in the WebHDFS documentation, but it does appear here:
  #
  # https://blogs.msdn.microsoft.com/microsoftrservertigerteam/2017/03/14/using-r-to-perform-filesystem-operations-on-azure-data-lake-store/
  #
  # maybe this is some sort of local (Azure) modification?

  response <-
    url %>%
    httr::PUT(
      body = file,
      config = httr::config(token = adls$token),
      add_headers(`Transfer-Encoding` = "chunked")
    )

  # determine success
  if (identical(response$status_code, 201L)) {
    result <- TRUE
  } else {
    result <- FALSE
  }

  message(response$status_code)

  # parse warning
  if (response$status_code %in% c(400L, 403L)) {
    error_message <- unpack_response(response)
    message(error_message)
    # message(error_message$RemoteException$message)
  }

  httr::stop_for_status(
    response,
    task = "create (write) file on Azure Datalake store"
  )

  result
}
