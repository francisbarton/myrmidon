# set <- sample(letters, 20, replace = TRUE)
set <- c("k", "g", "e", "z", "l", "n", "e", "e", "e", "y", "b", "h", "h", "s", "h", "u", "y", "y", "y", "a")

set.seed(1234)

"test_head" %>%
  testthat::test_that({
    out <- vctrs::vec_count(set)
    testthat::expect_length(out, 2)
    testthat::expect_equal(nrow(out), length(unique(set)))
    testthat::expect_identical(out$key[1], "y")
  })

"test_overall" %>%
  testthat::test_that({
    testthat::expect_equal(commonest(set), "e")
    testthat::expect_equal(commonest(set, first = FALSE), "y")
  })

"test_summarise" %>%
  testthat::test_that({
    test_df <- dplyr::tibble(
      letters = set
    )
    testthat::expect_equal(
      test_df %>%
        dplyr::summarise(
          letter = commonest(letters)
        ),
      structure(list(letter = "e"), class = c("tbl_df", "tbl", "data.frame"
      ), row.names = c(NA, -1L))
    )
    testthat::expect_equal(
      test_df %>%
        dplyr::summarise(
          letter = commonest(letters, first = FALSE)
        ),
      structure(list(letter = "y"), class = c("tbl_df", "tbl", "data.frame"
      ), row.names = c(NA, -1L))
    )
  })
