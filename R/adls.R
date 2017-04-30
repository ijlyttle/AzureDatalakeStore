#' Create an adls object
#'
#' @param base_url    character or `url` object made using [`httr::parse_url()`], this is
#'   the base URL for the datalake store. It may be convenient to use [`adls_url()`] to construct this.
#' @param token       `Token2.0`` reference-class (R6) object.
#'   It may be convenient to use [`AzureOAuth::oauth_token_azure()`] to construct this.
#'
#' @return An `adls` S3 object.
#' @examples
#' \dontrun{
#'   adls(url, token)
#' }
#' @export
#'
adls <- function(base_url, token) {

  # coerce into url
  base_url <- httr::parse_url(base_url)

  # assert that these are url and token objects
  assertthat::assert_that(
    inherits(base_url, "url"),
    inherits(token, "Token2.0")
  )

  structure(
    list(
      base_url = base_url,
      token = token
    ),
    class = "adls"
  )
}

print.adls <- function(x, ...){
  cat("<adls> (Azure Datalake Store)\n")
  cat("<url>: <url>\n")
  cat(paste0(" address:   ", httr::build_url(x$base_url), "\n"))
  cat("<token>: ")
  print(x$token)
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

