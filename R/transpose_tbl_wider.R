# See https://stackoverflow.com/a/67765616/5168907

#' Neatly transpose a long tibble to wide format
#'
#' The first column will be used to provide the column headings for the new
#'  columns
#'
#' @param .tbl A tibble or data frame
#' @export
transpose_tbl_wider <- function(.tbl) {
  .tbl |>
    dplyr::mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(cols = !1) |>
    tidyr::pivot_wider(names_from = 1, values_from = value) |>
    dplyr::rename_with(\(x) x = colnames(.tbl)[[1]], 1)
}
