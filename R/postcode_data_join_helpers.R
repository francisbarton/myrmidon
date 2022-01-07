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
