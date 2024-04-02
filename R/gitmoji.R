#' List gitmoji names matching `x`
#'
#' If `x` is "" (as by default), all names in the table will be returned
#' @inheritParams gitmoji
#' @returns A character vector of all matching gitmoji names
#' @export
gitmoji_name <- function(x = "", exact = FALSE) {
  x <- tolower(x)
  if (exact) x <- paste0("^", x, "$")
  rn <- grep(x, gitmoji_df[["name"]], ignore.case = TRUE)
  gitmoji_df |>
    dplyr::slice(rn)
    stringr::str_glue_data("{emoji} {name}")
}

#' List gitmoji with a description matching `x`
#'
#' If `x` is "" (as by default), all rows in the table will be returned
#'
#' @inheritParams gitmoji
#' @returns A three-column table showing matching emoji with their code
#'   and description
#' @export
gitmoji_desc <- function(x = "") {
  assertthat::is.string(x)
  rn <- grep(x, gitmoji_df[["description"]], ignore.case = TRUE)
  res <- gitmoji_df |>
    dplyr::slice(rn) |>
    dplyr::mutate(across("emoji",
      \(x) stringr::str_pad(x, 3L, "right"))) |>
    dplyr::mutate(across("name",
      \(x) stringr::str_pad(x, max(nchar(x)), "right", use_width = TRUE))) |>
    dplyr::mutate(across("description",
      \(x) stringr::str_remove(x, "\\.?$")))

  stringr::str_glue_data(res, " {emoji} {name}  | {description}")
}

#' Writes a gitmoji to the clipboard
#'
#' @description
#' Use [gitmoji_name()] to list all available gitmoji or to search the table of
#'   gitmoji by name.
#' Use [gitmoji_desc()] to search the table of gitmoji by description
#'
#' @param x string. A piece of text to search for.
#' @param exact Boolean, default `TRUE`. Whether to only return a result for the
#'   exact string provided, or return all names that match the string.
#' @returns `TRUE` if successful; the gitmoji is/are copied to the clipboard.
#' @examples
#' if (FALSE) {
#'  gitmoji("memo")
#'  gitmoji("flag", exact = FALSE)
#' }
#' @export
gitmoji <- function(x, exact = TRUE) {
  assertthat::is.string(x)
  x <- tolower(x)
  if (exact) x <- paste0("^", x, "$")
  rn <- grep(x, gitmoji_df[["name"]])

  if (rlang::is_empty(rn) | !is.numeric(rn)) {
    cli::cli_alert_warning("No matching gitmoji found.")
    return(invisible(TRUE))
  }
  
  res <- dplyr::slice(gitmoji_df, rn[[1L]])
  utils::writeClipboard(res[["emoji"]])
  res[["description"]] <- stringr::str_remove(res[["description"]], "\\.?$")

  res |>
    stringr::str_glue_data(
      "Gitmoji: {emoji} ('{description}') copied to clipboard."
    ) |>
    cli::cli_alert_success()
  invisible(TRUE)
}
