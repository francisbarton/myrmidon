#' as_year
#'
#' @param x a vector of years, provided as numeric or string
#' @param months a numeric vector. 1:12 by default (whole year).
#' @param unlist FALSE by default (allows returning a list). TRUE causes it to return a vector of dates.
#'
#' @return a vector of dates, or a list of vectors, one for each year, where multiple years are requested.
#' @export
#'
#' @examples
#' head(as_year(2019:2020, months = c(1, 4), unlist = TRUE), 10)
as_year <- function(x, months = 1:12, unlist = FALSE) {
  x <- as.character(x)

  assertthat::assert_that(is.numeric(months),
    msg = usethis::ui_oops(
      "Months must be supplied as numbers"
    )
  )


  months <- stringr::str_pad(
    as.character(months),
    width = 2, side = "left", pad = "0"
  )

  out <- purrr::map(
    x,
    ~ lubridate::as_date(
      lubridate::ymd(paste0(., "-01-01")):
      lubridate::ymd(paste0(., "-12-31"))
    ) %>%
      stringr::str_subset(
        stringr::str_c(
          paste0("-", months, "-"),
          collapse = "|"
        )
      ) %>%
      lubridate::as_date()
  )

  if (length(out) == 1 || unlist) {
    out <- do.call("c", out)
  }

  out
}
