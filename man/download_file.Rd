% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_file.R
\name{download_file}
\alias{download_file}
\title{Wrapper to Automate `download.file()`}
\usage{
download_file(
  url,
  dir = here::here(),
  destfile = NULL,
  overwrite = FALSE,
  quiet = TRUE,
  ...
)
}
\arguments{
\item{url}{URL to download. Passed to `download.file()`.}

\item{dir}{(optional) Directory to download to. Defaults to the output of
`here::here()` i.e. the project directory or equivalent (since
[this beauty](https://github.com/r-lib/rprojroot/pull/59) got merged you can
nest `here()`s and it just takes it in its stride.)}

\item{destfile}{(optional) Name of local file to download to.
The whole point of this is not to have to specify a destfile,
but if you want or need to, you can. Passed to `download.file()`.}

\item{overwrite}{(optional) Force allow overwrite of existing local destfile.
I haven't checked whether `download.file()` will actually do this in the end,
though.}

\item{quiet}{don't say anything while downloading}

\item{...}{Any other arguments you want to pass through to `download.file()`,
such as mode, method, headers and so on.}
}
\description{
Sometimes you just want to call download.file and not have to specify
the name of the destination file. You just want the local file to be the same
as the remote file that forms the basename of the URL.
}
\details{
Default setting for dir is "" which means file will end up in project root
directory (according to `here()`).

destfile we want to extract from the URL using `basename` but we need to offer
it as an argument to the function anyway because the user might want or need
to specify it, as you do with `download.file` eg if it turns out destfile
already exists.

The user can specify `overwrite = TRUE` which will allow the file to be
downloaded even if the target already exists.
}
\examples{
download_file("https://r-bootcamp.netlify.app/fishermen_mercury_README.md", dir = "data")
}
