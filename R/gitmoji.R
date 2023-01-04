#' List gitmoji names matching a text string (`x`)
#' If `x` is "" (as by default), all names in the table will be returned
#' @inheritParams gitmoji
#' @returns A character vector of all matching gitmoji names
#' @export
gitmoji_list <- function(x = "", exact = FALSE) {
  x <- tolower(x)
  if (exact) x <- paste0("^", x, "$")
  grep(x, gitmoji_df$name, value = TRUE)
}

#' List gitmoji whose description matches `x`
#' If `x` is "" (as by default), all rows in the table will be returned
#' @inheritParams gitmoji
#' @returns a three-column table showing matching emoji with their code and description
#' @export
gitmoji_desc <- function(x = "") {
  assertthat::is.string(x)
  n <- grep(x, gitmoji_df$description, ignore.case = TRUE)
  res <- gitmoji_df[n,]
  res$code <- stringr::str_pad(res$code, max(nchar(res$code)), "right")

  usethis::ui_info(
    stringr::str_glue_data(res, "{emoji} {code} {description}")
  )
}

#' Writes emoji to the system clipboard, whose name(s) match `x`
#' Use `gitmoji_list()` to list all available gitmoji or to search the table of gitmoji by emoji name.
#' Use `gitmoji_desc()` to search the table of gitmoji by description
#'
#' @param x string. A piece of text to search for
#' @param exact boolean, default TRUE. Whether to only return a result for the exact string provided, or return all names that contain the string.
#' @returns TRUE if successful; the emoji is/are copied to the clipboard
#' @export
gitmoji <- function(x, exact = TRUE) {
  assertthat::is.string(x)
  x <- tolower(x)
  if (exact) x <- paste0("^", x, "$")
  n <- grep(x, gitmoji_df$name)
  assertthat::not_empty(n)
  res <- gitmoji_df[n,]
  writeLines(res$emoji, con = "clipboard", sep = "")
  res$description <- stringr::str_remove(res$description, "\\.?$")
  usethis::ui_info(
    stringr::str_glue_data(res, "{emoji} ('{description}') gitmoji has been copied to clipboard.")
  )
  invisible(TRUE)
}
