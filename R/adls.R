#' Create an adls object.
#'
#' All the functions you will use to access the Azure Data Lake Store will
#' need to know where your data lake is, and how to get into it. Thus, the
#' location and access information are stored in a single object you
#' instantiate once, then use with each function call.
#'
#' @param base_url    `character` or `url` object made using [`httr::parse_url()`],
#'   the base URL for the data lake store.
#'   It may be convenient to use [`adls_url()`] to make your URL.
#' @param token       `Token2.0` reference-class (R6) object, [`httr::Token-class`].
#'   It may be convenient to use [`AzureOAuth::oauth_token_azure()`] or
#'   [`AzureOAuth::oauth_service_token_azure()`] to obtain your token.
#'
#' @return An `adls` S3 object.
#' @seealso [`adls_url()`], [`AzureOAuth::oauth_token_azure()`],
#'   [`AzureOAuth::oauth_service_token_azure()`],
#'
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
#' @param adls_name    `character`, Azure Data Lake Store name
#' @param path         `character`, path to add to default root.
#'   You may find [`file.path()`] to be useful to compose the path.
#' @param domain_name  `character`, domain name
#'
#' @return `url` object returned by [`httr::parse_url()`]
#' @seealso Azure Datalake Store documentation on
#'   ["WebHDFS Filesystem API"](https://docs.microsoft.com/en-us/rest/api/datalakestore/webhdfs-filesystem-apis#url-for-rest-calls)
#' @examples
#'   adls_url("foo")
#' @export
#'
adls_url <- function(adls_name, path = NULL,
                     domain_name = "azuredatalakestore.net") {

  url <-
    "https://{adls_name}.{domain_name}/webhdfs/v1" %>%
    glue::glue() %>%
    httr::parse_url() %>%
    url_path_append(path)

  url
}

