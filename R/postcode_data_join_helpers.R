req_base <- function(ep = "") {
  ua <- "github.com/francisbarton/myrmidon 0.6.6 // httr2"

  paste0("https://api.postcodes.io/", ep, "postcodes/") %>%
    httr2::request() %>%
    httr2::req_user_agent(ua)
}




pluck_result <- function(req) {
  req %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    purrr::pluck("result")
}



validate <- function(x) {
  req_base() %>%
    httr2::req_url_path_append(URLencode(x)) %>%
    httr2::req_url_path_append("/validate") %>%
    pluck_result()
}



check_term <- function(x) {
  req_base("terminated_") %>%
    httr2::req_url_path_append(URLencode(x)) %>%
    pluck_result()
}
check_term_possibly <- purrr::possibly(check_term, otherwise = NULL)



extract_lonlat <- function(df) {
  df %>%
    tidyr::unnest_wider(.data$response) %>%
    dplyr::select(c(.data$longitude, .data$latitude))
}



bulk_reverse_geocode <- function(df) {
  req_base() %>%
    httr2::req_body_json(list(geolocations = df)) %>%
    httr2::req_url_query(limit = 1) %>%
    pluck_result() %>%
    purrr::map_df("result") %>%
    dplyr::mutate(codes_names = names(.data$codes), codes = unlist(.data$codes)) %>%
    tidyr::pivot_wider(names_from = .data$codes_names, names_glue = "{codes_names}_code", values_from = .data$codes) %>%
    dplyr::mutate(across(everything(), unname))
}



autocomplete <- function(x) {
  x <- stringr::str_remove(x, "[A-Z]{1}$") %>%
    URLencode()
  req_base() %>%
    httr2::req_url_path_append(x) %>%
    httr2::req_url_path_append("/autocomplete") %>%
    httr2::req_url_query(limit = 1) %>%
    pluck_result() %>%
    purrr::pluck(1)
}
autocomplete_possibly <- purrr::possibly(autocomplete, otherwise = NULL)



bulk_lookup <- function(x) {
  len1 <- length(x)
  st <- lubridate::now()
  out <- req_base() %>%
    httr2::req_body_json(list(postcodes = x), auto_unbox = FALSE) %>%
    pluck_result() %>%
    purrr::map_df("result") %>%
    dplyr::mutate(codes_names = names(.data$codes), codes = unlist(.data$codes)) %>%
    tidyr::pivot_wider(names_from = .data$codes_names, names_glue = "{codes_names}_code", values_from = .data$codes) %>%
    dplyr::mutate(across(everything(), unname))
  fi <- lubridate::now()
  len2 <- nrow(out)
  if (interactive()) {
    usethis::ui_info(stringr::str_glue(
      "{len2} of {len1} postcodes successfully queried ({round(as.numeric(substr(fi - st, 1, 19)), 3)}s)."
    ))
  }
  out
}


narrow <- function(df) {
  df %>%
    dplyr::rename(
      lsoa11nm = .data$lsoa,
      lsoa11cd = .data$lsoa_code,
      msoa11nm = .data$msoa,
      msoa11cd = .data$msoa_code,
      wd21nm = .data$admin_ward,
      wd21cd = .data$admin_ward_code,
      pcon21nm = .data$parliamentary_constituency,
      pcon21cd = .data$parliamentary_constituency_code,
      ltla21nm = .data$admin_district,
      ltla21cd = .data$admin_district_code
    ) %>%
    dplyr::select(!c(
      any_of(
        c(
          "quality",
          "result_type",
          "nhs_ha",
          "longitude",
          "latitude",
          "european_electoral_region",
          "primary_care_trust",
          "incode",
          "outcode",
          "ced",
          "admin_county"
        )),
      starts_with("ccg"),
      starts_with("parish"),
      starts_with("nuts"),
      ends_with("_code")
    )) %>%
    dplyr::relocate(
      c(
        starts_with("lsoa"),
        starts_with("msoa"),
        starts_with("wd"),
        starts_with("ltla"),
        starts_with("pcon"),
        any_of(
          c(
          "region",
          "country",
          "eastings",
          "northings"
        ))
      ),
      .after = last_col()
    )
}
