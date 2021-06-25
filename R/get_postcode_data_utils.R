validate_postcode <- function(code) {

  endpoint <- "https://api.postcodes.io/postcodes/"

  query <- paste0(endpoint, code, "/validate")

  response <- httr::GET(query)

  httr::stop_for_status(response)

  if (httr::status_code(response) == 200) {
    httr::content(response) %>%
      `[[`("result")
  } else {
    NA
  }
}





validate_postcode_list <- function(lst) {

  check <- lst %>%
    purrr::map_lgl(validate_postcode)

  if (!all(check)) {
    valid <- lst[which(check)]
    invalid <- lst[which(!check)]
    fixed <- fix_invalid_postcodes(invalid)
    validate_fixed <- validate_postcode_list(fixed)
    c(valid, validate_fixed)
  } else {
    lst
  }
}





fix_invalid_postcodes <- function(codes) {

  fix_postcode <- function(code, search_limit = 1) {

    endpoint <- "https://api.postcodes.io/terminated_postcodes/"

    query <- paste0(endpoint, code)

    response <- httr::GET(query)

    httr::stop_for_status(response)

    if (!httr::status_code(response) == 200) {
      return("ERROR")
    }


   httr::content(response) %>%
     reverse_geocode_postcode(search_limit) %>%
     utils::tail(1)

  }

  codes %>%
    purrr::map_chr(fix_postcode) %>%
    validate_postcode_list()
}





reverse_geocode_postcode <- function(lst, limit = 1) {

  endpoint <- "https://api.postcodes.io/postcodes"

  lon <- lst[["result"]][["longitude"]]
  lat <- lst[["result"]][["latitude"]]

  query_data <- dplyr::tribble(
    ~ longitude, ~ latitude, ~ limit,
    lon, lat, limit
  )

  query <- list(geolocations = query_data)

  response <- httr::POST(
      url = endpoint
      , body = query
      , encode = "json"
    )

  httr::stop_for_status(response)

  if (!httr::status_code(response) == 200) {
    return("ERROR")
  }

  httr::content(response) %>%
    # purrr::pluck("result", 1, "result", limit, "postcode")
    purrr::pluck("result", 1, "result") %>%
    purrr::map_chr("postcode")

}


