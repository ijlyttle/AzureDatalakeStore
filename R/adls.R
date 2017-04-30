#' Create an adls object
#'
#' @param url    url
#' @param token  token
#'
#' @return An `adls` S3 object.
#' @examples
#'   adls(url, token)
#' @export
#'
adls <- function(url, token) {

  # assert that these are URL and token

}

#' Base URL
#'
#' This is a convenience function to return the base URL for the service.
#'
#' @param adls_name  character, Azure Data Lake Store name
#' @param domain     character, domain name
#'
#' @return `url` object returned by [`httr::parse_url()`]
#' @keywords internal
#' @examples
#'   adls_url("foo")
#' @export
#'
adls_url <- function(adls_name, domain_name = "azuredatalakestore.net") {

  url <-
    "https://{adls_name}.{domain_name}/webhdfs/v1" %>%
    glue::glue() %>%
    httr::parse_url()

  url
}

