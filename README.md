
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AzureDatalakeStore)](https://cran.r-project.org/package=AzureDatalakeStore) [![Travis-CI Build Status](https://travis-ci.org/ijlyttle/AzureDatalakeStore.svg?branch=master)](https://travis-ci.org/ijlyttle/AzureDatalakeStore) [![Coverage Status](https://img.shields.io/codecov/c/github/ijlyttle/AzureDatalakeStore/master.svg)](https://codecov.io/github/ijlyttle/AzureDatalakeStore?branch=master)

AzureDatalakeStore
==================

Overview
--------

This package is used to access the datalake store itself. Looking at the [reference](https://docs.microsoft.com/en-us/rest/api/datalakestore/webhdfs-filesystem-apis), we note that this is using WebHDFS.

Just to note that the goal of this package is to get things working for Azure Datalake Store. If we end up being able to support WebHDFS, that will be a bonus.

WebHFDS seems to use a lot of redirects. However, this [Microsoft blog](https://blogs.msdn.microsoft.com/microsoftrservertigerteam/2017/03/14/using-r-to-perform-filesystem-operations-on-azure-data-lake-store/) suggests that we may not have to do that. If so, I don't know if this would be a "feature" of Azure or a "feature" of **httr**, or maybe **curl**.

What we will need
-----------------

-   Make a directory

-   Delete a file/folder from a directory

-   List a directory

-   Upload a file to a directory

-   Read a file from a directory

-   Append to a file in a directory

-   Rename a file/folder in a directory

Installation
------------

**AzureDatalakeStore** is not yet available on CRAN. You may install from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("ijlyttle/AzureDatalakeStore")
```

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/ijlyttle/AzureDatalakeStore/issues).

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
