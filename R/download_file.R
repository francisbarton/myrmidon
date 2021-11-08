#' Wrapper to Automate `download.file()`
#'
#' Sometimes you just want to call download.file and not have to specify
#' the name of the destination file. You just want the local file to be the same
#' as the remote file that forms the basename of the URL.
#'
#' Default setting for dir is "" which means file will end up in project root
#' directory (according to `here()`).
#'
#' destfile we want to extract from the URL using `basename` but we need to offer
#' it as an argument to the function anyway because the user might want or need
#' to specify it, as you do with `download.file` eg if it turns out destfile
#' already exists.
#'
#' The user can specify `overwrite = TRUE` which will allow the file to be
#' downloaded even if the target already exists.
#'
#' @name download_file
#'
#' @param url URL to download. Passed to `download.file()`.
#' @param dir (optional) Directory to download to. Defaults to the output of
#' `here::here()` i.e. the project directory or equivalent (since
#' [this beauty](https://github.com/r-lib/rprojroot/pull/59) got merged you can
#' nest `here()`s and it just takes it in its stride.)
#' @param destfile (optional) Name of local file to download to.
#' The whole point of this is not to have to specify a destfile,
#' but if you want or need to, you can. Passed to `download.file()`.
#' @param overwrite (optional) Force allow overwrite of existing local destfile.
#' I haven't checked whether `download.file()` will actually do this in the end,
#' though.
#' @param quiet don't say anything while downloading
#' @param ... Any other arguments you want to pass through to `download.file()`,
#' such as mode, method, headers and so on.
#'
#' @export
#'
#' @examples
#' download_file("https://r-bootcamp.netlify.app/fishermen_mercury_README.md", dir = "data")
download_file <- function(
  url,
  dir = here::here(),
  destfile = NULL,
  overwrite = FALSE,
  quiet = TRUE,
  ...) {

  # the point of this function is to save the user having to specify a destfile
  # name. If destfile hasn't been specified by the user then get it from URL
  if (is.null(destfile)) {
    destfile <- basename(url)
  }

  # check that destfile is valid
  if (!is.character(destfile) || !length(destfile) == 1) {
    usethis::ui_stop(
      "The destfile is not a string and/or is not of length 1.")
  }

  # if dir exists, check it is a character and that the directory doesn't
  # already exist. If all ok then create the directory under the project root.
  if (is.character(dir) && !dir.exists(here::here(dir))) {
    dir.create(here::here(dir))
  }

  destpath <- normalizePath(here::here(dir, destfile))

  # check to see if the file already exists
  if (file.exists(destpath) && !overwrite) {
    usethis::ui_stop("A file already exists with that name.
                Try again, providing a `destfile` argument,
                or specify `overwrite = TRUE` to overwrite the existing file.")
  }

  # if we've got this far then it's probably ok to download the file.
  # other arguments to `download.file` can be passed via the dots.
  # We're supposed to implement some form of checking for download errors
  # as R may not report them to the user
  utils::download.file(url, destpath, overwrite = overwrite, quiet = quiet, ...)
}
