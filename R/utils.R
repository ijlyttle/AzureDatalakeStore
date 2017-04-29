#' Path operators for URL.
#'
#' @param url     `url` object, as returned by [httr::parse_url()]
#' @param ...     arguments to comprise, append, or preped the path
#'
#' @return `url` object
#' @keywords internal
#'
url_path <- function(url, ...) {

  url$path <- file.path(...)

  url
}

#' @rdname url_path
#' @keywords internal
#'
url_path_append <- function(url, ...) {

  url$path <- file.path(c(url$path, ...))

  url
}

#' @rdname url_path
#' @keywords internal
#'
url_path_prepend <- function(url, ...) {

  url$path <- file.path(c(..., url$path))

  url
}

#' Path operators for URL.
#'
#' @inheritParams url_path
#' @param ...     arguments to comprise or append the query
#'
#' @return `url` object
#' @keywords internal
#'
url_query <- function(url, ...) {

  url$query <- list(...)

  url
}

#' @rdname url_query
#' @keywords internal
#'
url_query_append <- function(url, ...) {

  url$query <- c(url$query, list(...))

  url
}

#' unpacks response
#'
#' @param response `httr` response object
#' @param ...      other arguments passed to [`jsonlite::fromJSON`]
#'
#' @return whatever the JSON spits out
#' @keywords internal
#'
unpack_response <- function(response, ...) {

  response %>%
    httr::content(type = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(...)

}
