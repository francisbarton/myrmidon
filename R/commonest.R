#' Pull out the commonest (mode) item from a vector
#'
#' @param vec A vector
#' @param first If there is a tie, this decides whether to prioritise
#'  the element that appears first in `vec`. If `FALSE`, the function will just
#'  return the first item in the result produced by the default mode of
#'  `vec_count` (`"count"`, which does not prioritise results by location)
#'
#' @returns A single element of the same type as `vec`
#'
#' @examples
#' set <- withr::with_seed(4444, sample(letters, 40, replace = TRUE))
#' commonest(set)
#' commonest(set, first = FALSE)
#' test_df <- dplyr::tibble(letters = set)
#' dplyr::summarise(test_df, letter = commonest(letters))
#' @export
commonest <- function(vec, first = TRUE) {
  if (first) {
    out <- vctrs::vec_count(vec, sort = "location") |>
      dplyr::arrange(desc(pick("count")))
  } else {
    # sort = "count" is the default, but making this explicit here
    out <- vctrs::vec_count(vec, sort = "count")
  }
  first(out[["key"]])
}
