#' Turn A Long Vector Into A Batched List
#'
#' Go \code{batch_it()} crazy!
#'
#' Batch up a long vector or list of vectors for passing to
#' services with limited APIs such as postcodes.io
#'
#' @importFrom usethis ui_info ui_stop ui_oops ui_nope
#' @importFrom assertthat assert_that
#' @importFrom rlang is_interactive
#' @importFrom lubridate as_date days_in_month ymd
#' @importFrom purrr map_dbl
#' @importFrom utils head
#'
#' @param x a vector
#' @param batches size of batches to create. Can be a single value or
#'   multiple values (see examples). Should be a whole, positive
#'   number.
#' @param proportion proportional sizes of batches to be created, for
#'   example c(4, 6) will create two batches of approximately 40% and
#'   60% of the length of the target vector. When multiple proportions
#'   are provided, these are not repeated. A single proportion less
#'   than 1 is repeated as many times as possible to get near to the
#'   length of the target vector. For example, a proportion of 0.1
#'   will be treated as a tenth, and batch sizes will be rounded to an
#'   integer size nearest to a tenth of the vector length.
#' @param maximise FALSE by default. When TRUE, a vector of batch
#'   sizes will be partially repeated to fit maximally to the length
#'   of the target vector. See examples below.
#'
#' @export
#'
#' @examples
#' batch_it(seq(2, 60, 2), 6)
#' batch_it(seq(2, 60, 2), proportion = 0.2)
#' batch_it(1:100, batches = c(20, 30, 50))
#' batch_it(letters, batches = c(4, 6))
#' batch_it(letters, batches = c(4, 6), maximise = TRUE)
#' batch_it(letters, proportion = c(4, 6))
#'
#' as_year <- function(x) {
#'   lubridate::as_date(
#'     lubridate::ymd(paste0(x, "-01-01")):
#'     lubridate::ymd(paste0(x, "-12-31"))
#'   )
#' }
#' month_lengths <- function(year) {
#'   lubridate::as_date(paste0(year, "-", 1:12, "-01")) %>%
#'     lubridate::days_in_month() %>%
#'     split(factor(names(.), levels = month.abb)) %>%
#'     purrr::map_dbl(1)
#' }
#' batch_it(x = as_year(2019), batches = month_lengths(2019))
batch_it <- function(x, batches = NULL, proportion = NULL, maximise = FALSE) {
  if (!rlang::is_interactive()) options(usethis.quiet = TRUE)

  # if no arguments supplied, set batches to a default value
  if (is.null(batches) && is.null(proportion)) batches <- length(x) / 10

  # prefer batches if both are supplied
  if (!is.null(batches) && !is.null(proportion)) {
    proportion <- NULL
    ui_info("batches and proportion cannot both be supplied.
            batches only is being kept.")
  }

  # sub-routine to handle proportion parameter
  if (!is.null(proportion)) {
    batches <- convert_proportion_to_batches(x, proportion) # helper
  }

  # just checking
  batches <- as.numeric(batches)

  assertthat::assert_that(is.numeric(batches),
    msg = ui_oops("Batch sizes provided are not numeric")
  )

  assertthat::assert_that(all(batches >= 0),
    msg = ui_oops("Batch sizes must not be negative numbers")
  )

  # ensure x is a reasonable vector
  if (is.list(x)) {
    ui_info("Converting list to single vector")
    x <- do.call("c", x)
  }

  assertthat::assert_that(is.atomic(x),
    msg = ui_stop("This function only works with lists or vectors")
  )


  if (length(x) > 10e6) {
    ui_nope("Easy, tiger! That vector has more than a million items.
              Are you sure you want to continue?")
  }

  batches <- round(batches)
  batches <- batches[which(!batches == 0)]
  batches <- maximise_batches(x, batches, maximise) # helper


  # this shouldn't be able to happen...
  if (sum(batches) > length(x)) {
    ui_stop("Batch sizes ended up longer than the length of the vector")
  }

  if (!length(x) - sum(batches) == 0) {
    ui_oops("The length of the target vector is not an exact multiple of the
    total of the batches. The remainder will be added as a final batch.")

    batches <- c(batches, length(x) - sum(batches))
  }

  # add initial zero to support algorithm below
  batches <- c(0, batches)

  # should be able to do all this with purrr??

  # construct batched list to return
  out <- list()

  # for (i in (1:(length(batches) - 1))) {
  for (i in head(seq_along(batches), -1)) {
    out[[i]] <- x[sum(batches[1:i], 1):sum(batches[1:(i + 1)])]
  }

  out

  # end of main function
}


### helper functions (internal)
convert_proportion_to_batches <- function(x, proportion) {
  proportion <- as.numeric(proportion)

  if (!all(proportion > 0)) {
    ui_stop("Proportions must be positive numbers")
  }

  if (length(proportion) == 1 && proportion < 1) {
    proportion <- rep(proportion, times = floor(1 / proportion))
    proportion <- c(proportion, 1 - sum(proportion))
  }

  `/`(proportion, sum(proportion)) * length(x)
}

maximise_batches <- function(x, batches, maximise) {
  if (!maximise) {
    batches <- rep(batches, times = floor(length(x) / sum(batches)))
  } else {
    batches <- rep(batches, times = ceiling(length(x) / sum(batches)))

    while (sum(batches) > length(x)) {
      batches <- head(batches, -1)
    }
    # test this feature with e.g.:
    # batch_it(letters, batches = c(4, 6), maximise = TRUE)
  }

  batches
}
