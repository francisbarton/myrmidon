#' Use postcodes.io to get postcode data
#'
#' @param df A data frame with a column of postcodes.
#' @param var The name of the variable in the data frame that contains
#'   postcodes (only!). Should be acceptable as a symbol (variable name)
#'   or as a standard character string.
#' @param fix_invalid Whether to try to fix any postcodes that are not
#'   found (potentially because they are terminated codes, or typos).
#' @param narrow Whether to narrow the returned data frame by removing
#'   a bunch of potentially unnecessary fields from the data before
#'   returning the final product to the user. Boolean, default TRUE.
#'   Set to FALSE to get all data variables available from the API,
#'   plus a data quality indicator and a record of which codes were
#'   invalid and subsequently fixed, if any.
#'
#' @importFrom assertthat assert_that
#' @export
postcode_data_join <- function(df, var = postcode, fix_invalid = TRUE, narrow = TRUE) {

  valid_results <- NULL
  terminated_results <- NULL
  autocompleted_results <- NULL
  replaced <- c()


  # check that var is a column of df
  var <- rlang::as_string(rlang::enexpr(var))
  assertthat::assert_that(
    var %in% names(df),
    msg = "That variable doesn't seem to exist in this data frame."
  ) %>%
    invisible()

  # select and rename a single column df with unique postcodes to validate
  codes <- df %>%
    dplyr::select(query_code = .data[[var]]) %>%
    dplyr::distinct()

  # result is 2-col df with query_code and results (see tests)
  validated <- codes %>%
    dplyr::mutate(result = purrr::map_lgl(.data[["query_code"]], validate))

  # split into invalid and valid codes
  invalid <- validated %>%
    dplyr::filter(!.data[["result"]]) %>%
    dplyr::select(!result)

  valid <- validated %>%
    dplyr::filter(.data[["result"]]) %>%
    dplyr::select(!result)


  ##### if we have some invalid codes then try to fix them
  # by, firstly, seeing if they are terminated codes, finding the lon/lat and
  # then reverse geocoding the lon/lat to get the current code;
  # then if that fails, returning a nearby code using the autocomplete feature
  # (assuming that codes with only single final character different are
  # geographically near each other - I think this is sound).

  if (nrow(invalid) > 0) {
    if (fix_invalid) {

      # filter out any invalid codes that match a terminated code...
      terminated <- invalid %>%
        dplyr::mutate(response = purrr::map(.data[["query_code"]], check_term_possibly)) %>%
        dplyr::filter(!purrr::map_lgl(.data[["response"]], is.null))

      # ...and find the current code for the same lon/lat
      # (only proceed with trying to expand the API response if there are
      # matched codes!)
      if (nrow(terminated) > 0) {
        lonlats <- terminated %>%
          extract_lonlat()
        # keep original codes as 'postcode' column
        terminated_codes <- terminated %>%
          dplyr::select(postcode = .data[["query_code"]])

        terminated_results <- bulk_reverse_geocode(lonlats) %>%
          dplyr::rename(query_code = .data[["postcode"]]) %>%
          dplyr::bind_cols(terminated_codes, .) %>%
          dplyr::select(!.data[["distance"]])

        replaced <- terminated_codes$postcode
      }

      usethis::ui_info(stringr::str_glue(
        "The following postcodes are terminated:
          {terminated_results$postcode}
          and have been replaced with the following current postcodes:
          {terminated_results$query_code}"
      ))

      # if there is a remainder of invalid codes that don't match terminated
      # codes, try the autocomplete method.
      # NB this will return codes that may be geographically inaccurate at small
      # scales compared to the original postcode query.
      if (nrow(terminated) < nrow(invalid)) {
        autocompleted <- invalid %>%
          dplyr::anti_join(terminated, by = "query_code") %>%
          # will be used to left_join to original df
          dplyr::rename(postcode = .data[["query_code"]]) %>%
          dplyr::mutate(query_code = purrr::map_chr(.data[["postcode"]], autocomplete_possibly)) %>%
          dplyr::filter(!purrr::map_lgl(.data[["query_code"]], is.null)) %>%
          # keep both original postcode and new postcode to query
          dplyr::select(postcode, query_code)

        if (nrow(autocompleted) > 0) {
          autocompleted_results <- autocompleted[["query_code"]] %>%
            batch_it(100) %>%
            purrr::map_df(bulk_lookup) %>%
            dplyr::left_join(autocompleted, ., by = c(query_code = "postcode"))
        }
        replaced <- c(replaced, autocompleted$postcode)
      }
      remainder <- setdiff(invalid$query_code, replaced)

      if (length(replaced) > 0) {
        usethis::ui_info(
          stringr::str_wrap(
            stringr::str_glue(
              "The following codes were found to be invalid, but were
            successfully replaced with nearby valid codes: {replaced}"
            ), 72))
      }

      ##### end of trying to match/replace invalid codes
    } else {
      remainder <- invalid$query_code
    }
    if (length(remainder) > 0) {
      usethis::ui_info(
        stringr::str_wrap(
          stringr::str_glue(
            "The following codes were unsuccessful: {remainder}"
          ), 72))
    }
  }

  if (nrow(valid) > 0) {
    valid_results <- valid$query_code %>%
      batch_it(100) %>%
      purrr::map_df(bulk_lookup) %>%
      dplyr::mutate(query_code = postcode)
  }

  postcode_data <- dplyr::bind_rows(
    purrr::compact(
      list(
        valid = valid_results,
        terminated = terminated_results,
        autocompleted = autocompleted_results
      )
    ),
    .id = "result_type"
  ) %>%
    dplyr::relocate(query_code, .after = .data[["result_type"]])

  if (narrow) postcode_data <- narrow(postcode_data)

  df %>%
    dplyr::left_join(postcode_data, by = vctrs::vec_c({{var}} := "postcode"))
}
