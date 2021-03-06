% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename.R
\name{adls_rename}
\alias{adls_rename}
\title{Rename a file/directory.}
\usage{
adls_rename(adls, path, destination)
}
\arguments{
\item{adls}{\code{adls} S3 object, generated using \code{\link[=adls]{adls()}}.}

\item{path}{\code{character}, remote path, defined with respect to root-path
in \code{adls$base_url}. You may find \code{\link[=file.path]{file.path()}} to be useful to compose the path.}

\item{destination}{\code{character}, remote path for the destination,
defined with respect to root-path in \code{adls$base_url}.}
}
\value{
A \code{logical} indicating the success of the operation.
}
\description{
Rename a file/directory.
}
\examples{
\dontrun{
  library("AzureOAuth")

  # create token (assumes Azure native app)
  token <- oauth_token_azure(
    tenant_id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
    application_id = "ffffffff-gggg-hhhh-iiii-jjjjjjjjjjjj",
    name = "foo"
  )

  # create adls object with base-URL and token
  adls_example <- adls(
    base_url = adls_url("bar"),
    token = token
  )

  # create a directory
  adls_mkdirs(adls_example, "baz")

  # reanme the newly-created directory
  adls_rename(adls_example, "baz", "baz_new")
}
}
\seealso{
WebHDFS documentation for \href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Rename_a_FileDirectory}{"Rename a File/Directory"}
}
