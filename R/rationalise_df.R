#' Shuffle the columns of a data frame in decreasing order of the number of unique values in each column
#' 
#' This makes sense when you have "nested" values such as sub-categories of
#' categories and you want the top-level categories at the right-hand end
#' of your data frame and the more fine-grained or unique values at the
#' left-hand end. If you have been doing left joins you might end up with an
#' "irrationally" sorted table; this function will help you rationalise it.
#' This won't make sense for all datasets, naturally.
#' 
#' @param .data A data frame
#' @export
rationalise_df <- function(.data) {
  assert_that(is.data.frame(.data))
  l <- .data |>
    dplyr::summarise(across(everything(), \(x) length(unique(x)))) |>
    unlist() |>
    sort(decreasing = TRUE)
  
  dplyr::select(.data, all_of(names(l)))
}
