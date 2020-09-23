#' Automatically download files from multiple URLs
#'
#' Passes URLs to download_file() after checking that there are no duplicate
#' destination files
#'
#' @param urls A list or vector of URLs
#' @param download_dir specify a directory to download to. Defaults to "" (cwd)
#' @param overwrite allow downloaded files to overwrite existing files. Defaults to FALSE
#' @param quiet don't say anything while downloading
#' @param ... Any other arguments you want to pass through to `download.file()`, such as
#' mode, method, headers and so on.
#'
#' @importFrom usethis ui_warn ui_info
#'
#' @return downloaded files
#' @export
#'
download_files <- function(urls, download_dir = "", overwrite = FALSE, quiet = TRUE, ...) {

  urls <- unique(unlist(urls))
  destfiles <- basename(urls)


  if (anyDuplicated(destfiles)) {
    usethis::ui_warn("There are duplicate destination filenames.
                        These filenames will be altered to remove the duplication.")

    dupes <- which(duplicated(destfiles))

    for (i in dupes) {

      destfiles[i] <- paste0(
        tools::file_path_sans_ext(destfiles[i]),
        "_",
        i,
        ".",
        tools::file_ext(destfiles[i]))

    }

    ui_info("The following files will have their names altered: ", destfiles[dupes])

  }

  mapply(myrmidon::download_file, urls, destfiles, MoreArgs = list(dir = download_dir, overwrite = overwrite, quiet = quiet, ...))

}
