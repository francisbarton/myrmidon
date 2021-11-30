in_names_test <- function(df, var = postcode) {
  var <- rlang::ensym(var)
  assertthat::assert_that(
    rlang::as_string({{var}}) %in% names(df),
    msg = "That variable doesn't seem to exist in this data frame."
  )
}

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
    tidyr::unnest_wider(response) %>%
    dplyr::select(longitude, latitude)
}


bulk_reverse_geocode <- function(df) {
  req_base() %>%
    httr2::req_body_json(list(geolocations = df)) %>%
    httr2::req_url_query(limit = 1) %>%
    pluck_result() %>%
    purrr::map_df("result") %>%
    dplyr::mutate(codes_names = names(codes), codes = unlist(codes)) %>%
    tidyr::pivot_wider(names_from = codes_names, names_glue = "{codes_names}_code", values_from = codes)
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
    dplyr::mutate(codes_names = names(codes), codes = unlist(codes)) %>%
    tidyr::pivot_wider(names_from = codes_names, names_glue = "{codes_names}_code", values_from = codes)
}
