#' Automatically download files from multiple URLs
#'
#' Passes URLs to download_file() after checking that there are no duplicate
#' destination files
#'
#' @param urls A list or vector of URLs
#' @param download_dir specify a directory to download to. Defaults to "" (cwd)
#' @param overwrite allow downloaded files to overwrite existing files. Defaults to FALSE
#'
#' @return downloaded files
#' @export
#'
download_files <- function(urls, download_dir = "", overwrite = FALSE, quiet = TRUE, ...) {

  urls <- unique(unlist(urls))
  destfiles <- basename(urls)

  # come up with a way to rename any duplicated destfiles so as to
  # eliminate duplicates - a job for future. For now:
  if (anyDuplicated(destfiles)) {
    usethis::ui_stop("There are duplicate destination filenames.")
  }

  mapply(myrmidon::download_file, urls, destfiles, MoreArgs = list(dir = download_dir, overwrite = overwrite, quiet = quiet, ...))

}
