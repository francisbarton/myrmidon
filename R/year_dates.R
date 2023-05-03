#' Create vectors of dates for one or more years (and/or months)
#'
#' @param x A vector of years, provided as numeric or string.
#' @param months A vector of month numbers, provided as numeric or string. 1:12
#'  by default (i.e. the whole of the year).
#'
#' @returns A list of vectors of dates, one element per year requested.
#' @export
#'
#' @examples
#' head(year_dates(2019:2020, months = c(1, 4)), 10)
year_dates <- function(x, months = 1:12) {
  x <- as.character(x)

  assertthat::assert_that(all(as.numeric(months) %in% 1:12),
    msg = usethis::ui_oops(
      "One or more supplied months is not a whole number between 1 and 12."
    ))


  padded_months <- stringr::str_pad(as.character(months), 2, "left", "0")

  create_dates <- function(x, months) {
    lubridate::as_date(
      lubridate::ymd(paste0(x, "-01-01")):lubridate::ymd(paste0(x, "-12-31"))
    ) |>
      stringr::str_subset(
        stringr::str_c(
          paste0("-", months, "-"),
          collapse = "|"
        )
      ) |>
      lubridate::as_date()
  }

  x |>
    purrr::map(\(x) create_dates(x, months = padded_months))
}
