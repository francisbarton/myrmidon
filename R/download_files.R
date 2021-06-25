#' Automatically download files from multiple URLs
#'
#' Passes URLs to download_file() after checking that there are no duplicate
#' destination files
#'
#' @param urls A list or vector of URLs
#' @inheritParams download_file
#'
#' @export
download_files <- function(
  urls,
  dir = here::here(),
  overwrite = FALSE,
  quiet = TRUE,
  ...) {

  urls <- unique(unlist(urls))
  destfiles <- basename(urls)

  if (anyDuplicated(destfiles)) {
    usethis::ui_warn("There are duplicate filenames.
        Downloaded filenames will be altered to avoid the duplication.")

    dupes <- destfiles[which(duplicated(destfiles))]

    dupes_alt <- purrr::map2_chr(dupes, seq_along(dupes),
      ~ stringr::str_replace(.x, "\\.(?=[a-z]+$)", paste0("_alt", .y, ".")))

    destfiles <- c(
      destfiles[-which(duplicated(destfiles))],
      dupes_alt)

    urls <- c(
      urls[-which(duplicated(destfiles))],
      urls[which(duplicated(destfiles))]
    )
  }

  mapply(myrmidon::download_file, urls, destfiles, MoreArgs = list(dir = dir, overwrite = overwrite, quiet = quiet, ...))

}
