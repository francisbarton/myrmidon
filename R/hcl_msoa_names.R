#' House of Commons Library MSOA Names as an easy tibble
#'
#' @param year character. Whether to return names and codes for 2021 or 2011
#'  MSOAs. "2021" is the default.
#' @param keep_welsh Boolean. Whether to retain Welsh language names (nmw
#'  columns). `TRUE` by default.
#' @returns A tibble
#' @export
hcl_msoa_names <- function(year = c("2021", "2011"), keep_welsh = TRUE) {

  year <- match.arg(year)
  url_base <- "https://houseofcommonslibrary.github.io/msoanames/"

  if (year == "2021") {
    url_file <- "MSOA-Names-Latest2.csv"
    col_types <- "ccccccc"
  }
  if (year == "2011") {
    url_file <- "MSOA-Names-Latest.csv"
    col_types <- "cccccc"
  }

  out <- paste0(url_base, url_file) |>
    readr::read_csv(col_types = col_types, col_select = 1:5) |>
    janitor::clean_names()

  if (keep_welsh) {
    out
  } else {
    out |>
      dplyr::select(!ends_with("nmw"))
  }
}
