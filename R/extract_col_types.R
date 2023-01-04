#' Extracts all the column types of a data frame to a character vector
#'
#' @param .data A data frame
#' @param named logical: Whether to retain the column names as names for the vector of types. Default `FALSE`.
#' @param collapse logical: Whether to collapse the vector of column names to a single string comprising the initial letters of each type. Default `FALSE`.
#'
#' @returns A character vector (with or without names), or if `collapse` is `TRUE`, a single condensed string.
#'
#' @export
extract_col_types <- function(.data, named = FALSE, collapse = FALSE) {
  v <- .data |>
    purrr::map_chr(pillar::type_sum)

  if (collapse) {
    v |>
      unname() |>
      stringr::str_extract("^[a-z]{1}") |>
      stringr::str_c(collapse = "")
  } else if (named) v
  else unname(v)
}
