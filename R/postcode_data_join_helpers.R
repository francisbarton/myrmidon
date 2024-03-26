req_base <- function(x = "") {
  ua <- "github.com/francisbarton/myrmidon // httr2"

  paste0("https://api.postcodes.io/", x, "postcodes/") |>
    httr2::request() |>
    httr2::req_user_agent(ua)
}



pluck_result <- function(req) {
  req |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("result")
}



validate_code <- function(x) {
  req_base() |>
    httr2::req_url_path_append(URLencode(x)) |>
    httr2::req_url_path_append("/validate") |>
    pluck_result()
}



check_terminated <- function(x) {
  req_base("terminated_") |>
    httr2::req_url_path_append(URLencode(x)) |>
    pluck_result()
}
check_term_possibly <- purrr::possibly(check_terminated, otherwise = NULL)




bulk_reverse_geocode <- function(.data, prev_data = NULL, curr_radius = 250L) {

  if (curr_radius > 4000L) {
    usethis::ui_info("Geocoding searches have not found some replacement postcodes despite searching up to 4km in radius from the original postcode. Stopping search now.")
    prev_data # return early
  }

  # data_with_output_areas <- .data |>
  #   dplyr::select(all_of(c(lon = "longitude", lat = "latitude"))) |>
  #   dplyr::mutate(oa21cd = purrr::pmap_chr(lonlatapi::find_oa))

  data_list <- .data |>
    dplyr::select(all_of(c("longitude", "latitude"))) |>
    dplyr::mutate(limit = 1L) |>
    dplyr::mutate(radius = curr_radius) |>
    dplyr::mutate(batch = ceiling(dplyr::row_number() / 100L)) |>
    # batch into groups of max 100 rows
    dplyr::nest_by("batch") |>
    dplyr::pull("data")

  geodata_return <- data_list |>
    purrr::map(get_geodata_return) |>
    purrr::list_flatten() # re-combine batches into a single list

  geodata_queries <- geodata_return |>
    purrr::map("query")

  geodata_query_results <- geodata_return |>
    purrr::map("result") |>
    purrr::list_flatten()

  nifty_bind_cols <- function(x, y) {
    x2 <- x |>
      tibble::as_tibble_row() |>
      dplyr::select(all_of(c("longitude", "latitude"))) |>
      dplyr::rename_with(\(x) paste0("orig_", x))
    if (!is.null(y)) {
      y2 <- y |>
        purrr::compact() |>
        tibble::as_tibble() |>
        unnest_codes()
    } else {
      y2 <- NULL
    }
    dplyr::bind_cols(x2, y2)
  }

  data_out <- geodata_queries |>
    purrr::map2(geodata_query_results, nifty_bind_cols) |>
    purrr::list_rbind()

  if (!"postcode" %in% names(data_out)) {
    data_out <- data_out |>
      dplyr::mutate(postcode = NA_character_)
  }

  dat_done <- data_out |>
    dplyr::filter(!if_any("postcode", is.na)) |>
    dplyr::bind_rows(prev_data)

  dat_missing <- data_out |>
    dplyr::filter(if_any("postcode", is.na)) |>
    dplyr::select(all_of(c(
      longitude = "orig_longitude",
      latitude = "orig_latitude"
    )))

  if (nrow(dat_missing) > 0L) {
    bulk_reverse_geocode(dat_missing, dat_done, curr_radius = curr_radius * 2L)
  } else {
    dat_done |>
      # https://stackoverflow.com/a/76044270#76044270
      dplyr::rename_with(\(x) paste0("new_", x, recycle0 = TRUE), .cols = any_of(c("longitude", "latitude")))
  }
}



unnest_codes <- function(.data) {
  if ("codes" %in% names(.data)) {
    .data |>
      dplyr::mutate(codes_names = names(.data[["codes"]])) |>
      dplyr::mutate(across("codes", unlist)) |>
      dplyr::rename(code = "codes") |>
      tidyr::pivot_wider(
        names_from = "codes_names",
        names_glue = "{codes_names}_{.value}",
        values_from = "code"
      )
  } else {
    .data
  }
}




autocomplete <- function(x) {
  # Create incomplete postcode: If x ends in two letters, keep only the first
  # one. If x ends in a single letter (i.e. already incomplete), still keep it.
  x <- stringr::str_replace(x, "([:alpha:]?)([:alpha:]?)$", "\\1") |>
    URLencode()
  req_base() |>
    httr2::req_url_path_append(x) |>
    httr2::req_url_path_append("/autocomplete") |>
    httr2::req_url_query(limit = 5L) |>
    pluck_result() |>
    unlist() |>
    sample(1L)
}
autocomplete_possibly <- purrr::possibly(autocomplete, otherwise = NA)




bulk_lookup <- function(x) {
  req_base() |>
    httr2::req_body_json(list(postcodes = x), auto_unbox = FALSE) |>
    pluck_result() |>
    purrr::map_df("result")
}


get_geodata_return <- function(.data) {
  req_base() |>
    httr2::req_body_json(list(geolocations = .data)) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("result")
}
