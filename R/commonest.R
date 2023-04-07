#' Pull out the commonest (mode) item from a vector
#'
#' @param vec A vector.
#' @param first If there is a tie, this parameter decides whether to prioritise
#'  the element that appears first in `vec`. If `FALSE`, the function will just
#'  return the first item in the result produced by the default mode of
#'  `vec_count` (ie `"count"`, which does not return results prioritised by
#'  location -- see test-commonest file).
#'
#' @returns A single element of the same type as `vec`.
#' @export
#'
#' @examples
#' set <- sample(letters, 40, replace = TRUE)
#' commonest(set)
#' commonest(set, first = FALSE)
#'
#' test_df <- dplyr::tibble(letters = set)
#' dplyr::summarise(test_df, letter = commonest(letters))
commonest <- function(vec, first = TRUE) {

  if (first) {
    out <- vctrs::vec_count(vec, sort = "location") |>
      dplyr::arrange(desc(.data$count))
  } else {
    # sort = "count" is the default, but making this explicit here
    out <- vctrs::vec_count(vec, sort = "count")
  }

  # return
  out$key[1]
}
