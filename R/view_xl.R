# https://gist.github.com/b-rodrigues/948156d09607e5e8e66b80e5b318a854
# Thanks Bruno

#' View a data frame in Excel
#'
#' @param .data A data frame or tibble
#' @returns Passes `.data` through, invisibly (so can be used in a pipeline?)
#' @export
view_xl <- function(.data) {
  if (rlang::is_interactive()) {
    tmp <- tempfile(fileext = ".csv")
    readr::write_csv(.data, tmp)
    browseURL(tmp)
  }
  invisible(.data)
}
