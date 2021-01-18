# check for invalid postcodes
check_for_bad_postcodes <- function(postcodes) {

    endpoint <- "https://api.postcodes.io/postcodes"
    out <- httr::POST(
      url = endpoint,
      body = list(postcodes = unique(postcodes)),
      encode = "json")

    httr::warn_for_status(out)

    check_null <- httr::content(out) %>%
      purrr::pluck("result") %>%
      purrr::map("result") %>%
      purrr::map_lgl(purrr::is_null)


    if (!any(check_null)) {
      usethis::ui_done("No postcodes returned NULL")
      invisible(NULL)
      } else {
        usethis::ui_info(
          paste(
          "There may be problems with postcodes: ",
          postcodes[which(check_null)]
          )
        )
        postcodes[which(check_null)]
  }
}
