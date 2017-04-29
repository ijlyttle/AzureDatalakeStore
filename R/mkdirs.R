#' Make a directory.
#'
#' @param adls_url    url object, generated using [`adls_url()`]
#' @param path        character, directory to create with respect to root -
#'   [`file.path()`] can be used here
#' @param permission  integer (octal), permissions to assign to the directory
#'
#' @return logical, indicating success
#' @examples
#' \dontrun{
#'   url <- adls_url("foo")
#'   adls_mkdirs("bar")
#' }
#' @export
#'
adls_mkdirs <- function(url, path, permission = NULL) {

  TRUE
}
