#' A quick function using `lubridate` to report how old I am
#'
#' @param date Your date of birth. Set to mine by default.
#' @returns A string
#' @export
how_old_am_i <- function(date = "1977-02-17") {
  date |>
    lubridate::as_date() |>
    lubridate::interval(lubridate::today()) |>
    lubridate::as.period(unit = "years") |>
    stringr::str_remove_all("\\s0[A-Z]{1}")
}
