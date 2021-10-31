#' A quick function using `lubridate` to report how old I am
#'
#' @param date your date of birth. Set to mine by default.
#' @export
how_old_am_i <- function(date = "1977-02-17") {
  date %>%
    lubridate::as_date() %>%
    lubridate::interval(lubridate::today()) %>%
    lubridate::as.period(unit = "year") %>%
    stringr::str_remove_all("\\s0[A-Z]{1}")
}
