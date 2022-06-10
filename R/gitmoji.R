#' @export
gitmoji_list <- function(x = "") {
  grep(x, gitmoji_df$name, value = TRUE)
}

#' @export
gitmoji_desc <- function(x = "") {
  assertthat::is.string(x)
  n <- grep(x, gitmoji_df$description, ignore.case = TRUE)
  res <- gitmoji_df[n,]

  usethis::ui_info(
    stringr::str_glue(
      "{res$emoji} {stringr::str_pad(res$code, max(nchar(res$code)), \"right\")} {res$description}"
    )
  )
}

#' @export
gitmoji <- function(x) {
  assertthat::is.string(x)
  n <- grep(x, gitmoji_df$name, ignore.case = TRUE)
  assertthat::not_empty(n)
  assertthat::is.count(n)
  res <- gitmoji_df[n,]
  utils::writeClipboard(res$emoji)
  usethis::ui_info(
    stringr::str_glue(
      "{res$emoji} ({stringr::str_remove(res$description, \"\\.?$\")}) copied to clipboard."
    )
  )
  invisible(NULL)
}
