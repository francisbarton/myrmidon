#' Add the House of Commons Library MSOA Names to a data frame
#'
#' This will only work if you have a column in the df that matches
#' a column name from the Names resource, for the left_join to work.
#'
#' @param df A data frame. Obv.
#' @param welsh Whether to keep the Welsh language names. Defaults to TRUE.
#'
#' @return A wider data frame.
#' @export
add_msoa_names <- function(df, welsh = TRUE) {
  names <- readr::read_csv("https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest.csv", col_types = "cccccc", col_select = 1:5)

  if (!welsh) names <- dplyr::select(names, !ends_with("nmw"))

  df %>%
    dplyr::left_join(names)
}
