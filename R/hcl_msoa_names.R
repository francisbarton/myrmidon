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
  base <- "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-Latest"

  url <- if (year == "2021") paste0(base, "2.csv") else paste0(base, ".csv")
  col_types <- if (year == "2021") "ccccccc" else "cccccc"
  
  out <- url |>
    readr::read_csv(col_types = col_types, col_select = 1:5) |>
    janitor::clean_names()

  if (keep_welsh) out else dplyr::select(out, !ends_with("nmw"))
}
