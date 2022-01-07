#' Use postcodes.io to get postcode data
#'
#' @param df a data frame with a column of postcodes
#' @param var the name of the variable in the data frame that contains postcodes (only!)
#' @param fix_invalid whether to try to fix any postcodes that are not found (potentially because they are terminated codes)
#'
#' @importFrom assertthat assert_that
#' @export
postcode_data_join <- function(df, var = postcode, fix_invalid = TRUE) {

  # check that var is a column of df
  df %>%
    in_names_test("{{var}}")

  # select and rename a single column df with unique postcodes to validate
  codes <- df %>%
    dplyr::select("query_code" = .data[["{{var}}"]]) %>%
    dplyr::distinct()

  # result is 2-col df with query_code and results (see tests)
  validated <- codes %>%
    dplyr::mutate("result" = purrr::map_lgl(.data[["query_code"]], validate))

  # split into invalid and valid codes
  invalid <- validated %>%
    dplyr::filter(!.data[["result"]]) %>%
    dplyr::select(!"result")

  valid <- validated %>%
    dplyr::filter(.data[["result"]]) %>%
    dplyr::select("result")

  ##### if we have some invalid codes then try to fix them
  # by, firstly, seeing if they are terminated codes, finding the lon/lat and
  # then reverse geocoding the lon/lat to get the current code;
  # then if that fails, returning a nearby code using the autocomplete feature
  # (assuming that codes with only single final character different are
  # geographically near each other - I think this is sound).
  if (fix_invalid & nrow(invalid) > 0) {

    # filter out any invalid codes that match a terminated code...
    terminated <- invalid %>%
      dplyr::mutate("response" = purrr::map(.data[["query_code"]], check_term_possibly)) %>%
      dplyr::filter(!purrr::map_lgl(.data[["response"]], is.null))

    # ...and find the current code for the same lon/lat
    # (only proceed with trying to expand the API response if there are
    # matched codes!)
    if (nrow(terminated) > 0) {
      lonlats <- terminated %>%
        extract_lonlat()
      # keep original codes as 'postcode' column
      terminated_codes <- terminated %>%
        dplyr::select("postcode" = .data[["query_code"]])

      # find replacement (current) codes using reverse geocode
      # the output here should be very similar in structure to `valid` above
      terminated_results <- bulk_reverse_geocode(lonlats) %>%
        dplyr::rename("query_code" = .data[["postcode"]]) %>%
        dplyr::bind_cols(terminated_codes, .) %>%
        dplyr::select(!.data[["distance"]])

      usethis::ui_info(stringr::str_glue(
        "The following postcodes are terminated:
        {terminated_results$postcode}
        and have been replaced with the following current postcodes:
        {terminated_results$query_code}"
      ))
    } else {
      terminated_results <- NULL
    }

    # if there is a remainder of invalid codes that don't match terminated
    # codes, try the autocomplete method.
    # NB this will return codes that may be geographically inaccurate at small
    # scales compared to the original postcode query
    if (nrow(terminated) < nrow(invalid)) {
      autocompleted <- invalid %>%
        dplyr::anti_join(terminated) %>%
        # will be used to left_join to original df
        dplyr::rename("postcode" = .data[["query_code"]]) %>%
        dplyr::mutate("query_code" = purrr::map_chr(.data[["postcode"]], autocomplete_possibly)) %>%
        dplyr::filter(!purrr::map_lgl(.data[["query_code"]], is.null)) %>%
        # keep both original postcode and new postcode to query
        dplyr::select(c("postcode", "query_code"))
    } else autocompleted <- NULL

    # filter out those that have been replaced as terminated
    # or via autocomplete (this bit needs rethinking)
    # remainder <- invalid %>%
    #   dplyr::anti_join(terminated) %>%
    #   dplyr::anti_join(autocompleted, by = c(query_code = "postcode"))
    #
    # if (nrow(remainder) > 0) {
    #   usethis::ui_info(stringr::str_glue(
    #     "The following codes were unsuccessful: {remainder$query_code}"
    #   ))
    # }
  }
  ##### end of trying to match/replace invalid codes


  # not me just assuming there was at least one valid code!
  if (nrow(valid) > 0) {
    valid_results <- valid$query_code %>%
      batch_it(100) %>%
      purrr::map_df(bulk_lookup)
  } else valid_results <- NULL

  if (nrow(autocompleted) > 0) {
    autocompleted_results <- autocompleted[["query_code"]] %>%
      batch_it(100) %>%
      purrr::map_df(bulk_lookup) %>%
      dplyr::left_join(autocompleted, ., by = c("query_code" := .data[["postcode"]]))
  } else autocompleted_results <- NULL

  postcode_data <- dplyr::bind_rows(
    purrr::compact(
      list(
        valid = valid_results,
        terminated = terminated_results,
        autocomplete = autocompleted_results
        )
      ),
    .id = "result_type"
  ) %>%
    dplyr::relocate("query_code", .after = .data[["result_type"]])

  df %>%
    dplyr::left_join(postcode_data, by = vctrs::vec_c("{{var}}" := .data[["postcode"]]))
}
