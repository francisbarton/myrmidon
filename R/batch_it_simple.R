#' Convert a list or vector to a batched list of its elements
#' @inheritParams batch_it
#' @examples
#' batch_it_simple(letters, 6)
#' batch_it_simple(letters, 0.45)
#' @export
batch_it_simple <- function(x, batch_size) {
  if (!rlang::is_interactive()) {
    options(usethis.quiet = TRUE)
  }

  # ensure x is a reasonable vector
  if (is.list(x)) {
    ui_info("Converting list to single vector")
    x <- purrr::list_c(x)
  }

  if (!is.vector(x)) {
    ui_stop("This function only works with lists or vectors")
  }

  if (length(x) > 10e6) {
    ui_nope("Easy, tiger! That vector has more than a million items.
            Are you sure you want to continue?")
  }

  # ensure batch_size is an appropriate single positive number
  if (length(batch_size) != 1 | batch_size <= 0) {
    ui_stop("The batch_size parameter must be a single positive value")
  }

  # if batch_size is supplied as a decimal between 0 and 1, interpret this as
  # a proportion of the length of `x`, and convert to an integer
  if (batch_size < 1) {
    batch_size <- ceiling(length(x) * batch_size)
  }

  if (batch_size > length(x)) {
    ui_oops("Batch size provided was greater than the length of the vector.")
    batch_size <- length(x)
  }

  batch_size <- round(batch_size)
  assertthat::assert_that(batch_size > 0)

  # do the batching by creating a vector of factors of length(x)
  # then use this as the factor argument to split(x)
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) |>
    utils::head(length(x))
  unname(split(x, f))
}
