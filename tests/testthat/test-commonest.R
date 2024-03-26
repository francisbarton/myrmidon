# set <- sample(letters, 20, replace = TRUE)
set <- c("k", "g", "y", "z", "l", "n", "e", "e", "e", "y", "b", "h", "h", "s", "h", "u", "y", "y", "e", "a")

set.seed(1234)

"test_head" |>
  test_that({
    out <- vctrs::vec_count(set)
    expect_length(out, 2)
    expect_equal(nrow(out), length(unique(set)))
    expect_identical(out$key[1], "y")
  })

"test_overall_y" |>
  test_that({
    expect_equal(commonest(set, first = TRUE), "y")
  })
"test_overall_e" |>
  test_that({
    expect_equal(commonest(set, first = FALSE), "y")
  })

"test_summarise" |>
  test_that({
    test_df <- dplyr::tibble(
      letters = set
    )
    expect_equal(
      test_df |>
        dplyr::summarise(
          letter = commonest(letters)
        ),
      structure(list(letter = "y"), class = c("tbl_df", "tbl", "data.frame"
      ), row.names = c(NA, -1L))
    )
    expect_equal(
      test_df |>
        dplyr::summarise(
          letter = commonest(letters, first = FALSE)
        ),
      structure(list(letter = "y"), class = c("tbl_df", "tbl", "data.frame"
      ), row.names = c(NA, -1L))
    )
  })
