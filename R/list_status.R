#' List a directory.
#'
#'
#' @inheritParams adls_mkdirs
#'
#' @return If empty, returns `NULL`. Otherwise returns `tbl_df` with with directory listing.
#' @seealso [`adls()`], [`adls_url()`]
#'   WebHDFS documentation for ["List a Directory"](http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#List_a_Directory)
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
#'
#'   # list the root directory
#'   adls_liststatus(adls_example)
#' }
#' @export
#'
adls_list_status <- function(adls, path = NULL) {

  # validate inputs
  assertthat::assert_that(
    inherits(adls, "adls"),
    (is.character(path) && identical(length(path), 1L)) || is.null(path)
  )

  url <-
    adls$base_url %>%
    url_path_append(path) %>%
    url_query_append(op = "LISTSTATUS")

  # hack to compose URL properly for root directory
  if (is.null(path)) {
    len <- length(url$path)
    url$path[len] <- paste0(url$path[len], "/")
  }

  response <-
    url %>%
    httr::GET(
      httr::content_type_json(),
      httr::accept_json(),
      config = httr::config(token = adls$token)
    ) %>%
    httr::stop_for_status(
      task = "list directory on Azure Datalake store"
    )

  result <-
    response %>%
    unpack_response() %>%
    `[[`("FileStatuses") %>%
    `[[`("FileStatus")

  if (is.data.frame(result)){
    result$accessTime <- POSIXct_from_ms(result$accessTime)
    result$modificationTime <- POSIXct_from_ms(result$modificationTime)
    result$length <- as.numeric(result$length)

    result <- tibble::as_data_frame(result)
  } else {
    result <- NULL
  }

  result
}
