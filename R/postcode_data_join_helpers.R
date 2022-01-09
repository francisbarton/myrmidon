req_base <- function(ep = "") {
  ua <- "github.com/francisbarton/myrmidon // httr2"

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
    httr2::req_url_path_append(x) %>%
    httr2::req_url_path_append("/validate") %>%
    pluck_result()
}



check_term <- function(x) {
  req_base("terminated_") %>%
    httr2::req_url_path_append(x) %>%
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
    tidyr::pivot_wider(names_from = .data$codes_names, names_glue = "{codes_names}_code", values_from = .data$codes)
}



autocomplete <- function(x) {
  x <- stringr::str_remove(x, "[A-Z]{1}$")
  req_base() %>%
    httr2::req_url_path_append(x) %>%
    httr2::req_url_path_append("/autocomplete") %>%
    httr2::req_url_query(limit = 1) %>%
    pluck_result() %>%
    purrr::pluck(1)
}
autocomplete_possibly <- purrr::possibly(autocomplete, otherwise = NULL)



bulk_lookup <- function(x) {
  req_base() %>%
    httr2::req_body_json(list(postcodes = x), auto_unbox = FALSE) %>%
    pluck_result() %>%
    purrr::map_df("result") %>%
    dplyr::mutate(codes_names = names(.data$codes), codes = unlist(.data$codes)) %>%
    tidyr::pivot_wider(names_from = .data$codes_names, names_glue = "{codes_names}_code", values_from = .data$codes)
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
      .data$quality,
      .data$result_type,
      .data$nhs_ha,
      .data$longitude,
      .data$latitude,
      .data$european_electoral_region,
      .data$primary_care_trust,
      .data$incode,
      .data$outcode,
      .data$ced, # county electoral division
      .data$admin_county,
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
        .data$region,
        .data$country,
        .data$eastings,
        .data$northings
      ),
      .after = last_col()
    )
}
