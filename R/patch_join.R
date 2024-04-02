#' A "patch join" or "coalesce join"
#'
#' This uses a column in one data frame (`dtf_y`) to patch `NA` values in
#'  another data frame (`dtf_x`).
#'
#' Currently the function is deliberately kept limited and simple. It could of
#'  course be extended with further functionality (multiple patch columns,
#'  patching from a column with a different name, replacing values more
#'  generally not just `NA`s, specifying custom join_by (key) columns and so on).
#'
#' But for now I just want to make a solid function that works and does this
#'  one job well, without lots of risks or potential scope for data loss
#'  disasters.
#'
#' A further (deliberate) limitation of this function is that it expects a
#'  one-to-one relationship between rows in `dtf_x` and `dtf_y`. This means that
#'
#'  -  Each row in x matches at most 1 row in y
#'  -  Each row in y matches at most 1 row in x
#'
#' (from the `dplyr::left_join()` help page).
#'
#' This means that it will throw an error if `dtf_y` would provide multiple
#'  matching rows for a row in `dtf_x`, creating additional rows in the output
#'  tibble. I want to avoid that - I want the patching to be precise: each `NA`
#'  can only be replaced by at most one patch value.
#'
#' @param dtf_x A data frame. This contains a column with `NA` values that you
#' intend to replace with actual values where possible.
#' @param dtf_y A data frame. This contains a column with values that you intend
#' to use to replace the `NA`s in `dtf_x`
#' @param patch_col A string. This provides the name of the column which is to
#' be patched. Currently this needs to be present in `dtf_x` and `dtf_y` and
#' refer to the column to be patched and the column with the replacement values.
#' The column should be a vector column in both cases, and of the same `type`
#' otherwise coercion to the lower level type may occur.
#'
#' @returns A tibble with the same column names and number of rows as `dtf_x`
#'
#' @export
#'


patch_join <- function(dtf_x, dtf_y, patch_col) {
  assert_that(
    patch_col %in% names(dtf_x) & patch_col %in% names(dtf_y),
    msg = "patch_join: The column to patch is not found in both data frames."
  )

  # Automatically identify the common columns that will be used to join by.
  # All columns in common apart from `patch_col`
  by_cols <- setdiff(
    intersect(names(dtf_x), names(dtf_y)),
    patch_col
  )

  assert_that(
    is.vector(dtf_x[[patch_col]]),
    msg = "patch_join: The column to be patched in dtf_x is not a vector col."
  )
  assert_that(
    is.vector(dtf_y[[patch_col]]),
    msg = "patch_join: The column to patch from in dtf_y is not a vector col."
  )

  assert_that(
    typeof(dtf_x[[patch_col]]) == typeof(dtf_y[[patch_col]])
  )

  dtf_y <- dtf_y |>
    dplyr::select(all_of(c(by_cols, patch_col))) |>
    dplyr::rename(src_col = {{ patch_col }})

  out <- dtf_x |>
    dplyr::left_join(dtf_y, by = {{ by_cols }}, relationship = "one-to-one") |>
    dplyr::mutate(across({{ patch_col }}, \(x) dplyr::coalesce(x, .data[["src_col"]]))) |>
    dplyr::select(!"src_col") |>
    dplyr::select(all_of(names(dtf_x)))

  if (rlang::is_interactive()) {
    usethis::ui_info(
      "There were {sum(is.na(dtf_x[[patch_col]]))} NAs in the original data frame column, out of {nrow(dtf_x)} rows. {sum(is.na(dtf_x[[patch_col]])) - sum(is.na(out[[patch_col]]))} of these have now been patched."
    )}

  assert_that(nrow(out) == nrow(dtf_x))
  assert_that(identical(names(out), names(dtf_x)))

  out
}
