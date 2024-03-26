#' Use postcodes.io to get postcode data
#'
#' @param .data A data frame with a column of postcodes, or a vector
#'  of postcodes.
#' @param var Character. The name of the variable in the data frame that
#'  holds the postcodes to be submitted. Ignored if .data is a vector.
#'
#' @examples
#' postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")
#' test_df1 <- dplyr::tibble(
#'   place = paste0("place_", 1:3),
#'   postcode = postcodes)
#' postcode_data_join(test_df1)
#' @export
postcode_data_join <- function(.data, var = "postcode") {

  if (is.data.frame(.data)) {
    assert_that(
      var %in% names(.data),
      msg = "That column doesn't exist in this data frame."
    )
    codes <- unique(.data[[var]]) 
  } else {
    assert_that(rlang::is_character(.data))
    codes <- unique(.data)
  }

  assert_that(length(codes) > 0L, msg = "No postcodes have been found.")

  codes <- toupper(codes)

  valid_codes <- codes |>
    purrr::keep(validate_code)
  invalid_codes <- setdiff(codes, valid_codes)

  if (length(invalid_codes) > 0L) {
    c(
      "{.fn postcode_data_join} found {length(invalid_codes)} ",
      "invalid postcodes. Examples: ",
      "{.val {head(invalid_codes, 5L)}}. ",
      "These will not be used for the data join. ",
      "Use {.fn suggest_postcode_fixes} to find valid replacement postcodes."
    ) |>
      cli::cli_alert_warning(wrap = TRUE)
  }


  if (length(valid_codes) > 0L) {
    valid_results <- valid_codes |>
      batch_it(100L) |>
      purrr::map(bulk_lookup) |>
      purrr::list_rbind() |>
      unnest_codes()
  } else {
    valid_results <- NULL
  }

  if (is.data.frame(.data) & !is.null(valid_results)) {
    .data |>
      dplyr::left_join(valid_results, by = vctrs::vec_c({{ var }} := "postcode"))
  } else {
    valid_results
  }
}


#' Return data for suggested replacements for invalid postcodes
#' 
#' Data for terminated invalid codes will be returned according to the nearest
#' current valid code for the old code's longitude/latitude.
#' Data for codes that are invalid due to an incorrect final letter will be
#' returned according to an autocompletion process.
#' Missing data will be returned for any other invalid postcodes that could not
#' be replaced using the two methods above.
#' Any valid codes supplied will be ignored, and excluded from the returned data
#' 
#' @param codes character vector. Postcodes that may be invalid.
#' @returns A data frame with a row for each invalid postcode supplied.
#' @examples suggest_postcode_fixes(c("hd1 2ut", "hd1 2uu", "hd1 2uv"))
#' @export
suggest_postcode_fixes <- function(codes) {

  codes <- unique(toupper(codes))
  assert_that(length(codes) > 0L, msg = "No postcodes were supplied.")
  invalid_codes <- codes |>
    purrr::discard(validate_code)

  if (length(invalid_codes) == 0L) {
    "{.fn postcode_data_join} found no postcodes that need fixing." |>
      cli::cli_alert_success()
    invisible(NULL) # return
  } else {
    terminated_codes_data <- invalid_codes |>
      purrr::map(check_terminated_possibly) |>
      purrr::compact() |>
      purrr::map(tibble::as_tibble_row) |>
      purrr::list_rbind()
    if (nrow(terminated_codes_data) > 0L) {
      fixed_terminated_data <- fix_terminated(terminated_codes_data)
    }
    if (!is.null(fixed_terminated_data)) {
      unfixed_codes <- invalid_codes |>
        setdiff(fixed_terminated_data[["postcode"]])
    } else {
      unfixed_codes <- invalid_codes
    }
    if (length(unfixed_codes) > 0L) {
      fixed_autocomplete_data <- fix_by_autocomplete(unfixed_codes)
    } else {
      fixed_autocomplete_data <- NULL
    }

    fixed_results <- list(
      original_terminated = fixed_terminated_data,
      original_invalid = fixed_autocomplete_data
    ) |>
      purrr::list_rbind(names_to = "reason_for_fixing")

    # return:
    tibble::tibble(postcode = invalid_codes) |>
      dplyr::left_join(fixed_results, "postcode") |>
      dplyr::mutate(
        across("reason_for_fixing", \(x) if_else(is.na(x), "unfixed", x))
      )
  }
}


fix_terminated <- function(.data) {
  geocoded_data <- .data |>
    bulk_reverse_geocode()
  if (nrow(geocoded_data) > 0L) {
    fixed_terminated_codes <- geocoded_data |>
      unnest_codes() |>
      dplyr::select(!"distance") |>
      dplyr::rename(new_postcode = "postcode")
    .data |>
      dplyr::rename_with(\(x) paste0("orig_", x), .cols = ends_with("tude")) |>
      dplyr::inner_join(
        fixed_terminated_codes, c("orig_longitude", "orig_latitude")
      ) |>
      dplyr::select(!all_of(c("orig_longitude", "orig_latitude"))) |>
      dplyr::rename(
        longitude = "new_longitude",
        latitude = "new_latitude"
      )
  } else {
    NULL
  }
}



fix_by_autocomplete <- function(codes) {
  autocomplete_results <- codes |>
    purrr::map_chr(autocomplete_possibly)

  ac_data <- tibble::tibble(
    postcode = codes,
    new_postcode = autocomplete_results
  )
  ac_codes <- autocomplete_results |>
    purrr::discard(is.na)

  if (length(ac_codes) > 0L) {
    fixed_ac_data <- ac_codes |>
      batch_it(100L) |>
      purrr::map(bulk_lookup) |>
      purrr::map(unnest_codes) |>
      purrr::list_rbind() |>
      dplyr::rename(new_postcode = "postcode")
    ac_data |>
      dplyr::left_join(fixed_ac_data, "new_postcode")
  } else {
    ac_data
  }
}
