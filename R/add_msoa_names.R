#' Add the House of Commons Library MSOA Names to a data frame
#'
#' This will only work if you have a column in the df that matches
#' a (clean) column name from the MSOA Names resource (so the left_join works).
#'
#' @param df A data frame. Obv.
#' @param welsh Whether to keep the Welsh language names. Defaults to TRUE.
#'
#' @return A wider data frame.
#' @export
add_msoa_names <- function(df, welsh = TRUE) {
  msoa_names <- readr::read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest.csv", col_types = "cccccc", col_select = 1:5) %>%
    dplyr::select(!ends_with("nmw")) %>%
    janitor::clean_names()

  if (!welsh) msoa_names <- dplyr::select(msoa_names, !ends_with("nmw"))

  join_vars <- intersect(names(df), names(msoa_names))
  move_vars <- setdiff(names(msoa_names), join_vars)

  df %>%
    dplyr::left_join(msoa_names, by = join_vars) %>%
    dplyr::relocate(all_of(move_vars), .after = dplyr::last(join_vars))
}
