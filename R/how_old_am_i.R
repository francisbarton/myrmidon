how_old_am_i <- function(date) {
  date %>%
    lubridate::interval(lubridate::today()) %>%
    lubridate::as.period(unit = "year") %>%
    stringr::str_remove_all("\\s0[A-Z]{1}")
}
