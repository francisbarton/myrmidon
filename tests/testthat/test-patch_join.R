"test 1" |>
  test_that({
    dtf_x <- tibble::tibble(
      key = letters,
      code = paste0(LETTERS, sample(seq(1000, 9999), 26)),
      value = sample(c(seq(1.0, 6.0, 0.2), rep(NA_real_, 13)), 26)
    )

    dtf_y <- tibble::tibble(
      key = sample(letters, 20),
      colour = sample(sub("\\d*$", "", colours()), 20),
      value = sample(seq(1.0, 6.0, 0.1), 20)
    )

    patch_col <- "value"

    expect_true(patch_col %in% names(dtf_x) & patch_col %in% names(dtf_y))

    expect_true(typeof(dtf_x[[patch_col]]) == typeof(dtf_y[[patch_col]]))

    by_cols <- setdiff(
      intersect(names(dtf_x), names(dtf_y)),
      patch_col
    )

    expect_length(by_cols, 1)

    dtf_y <- dtf_y |>
      dplyr::select(all_of(c(by_cols, src_col = patch_col)))

    expect_equal(ncol(dtf_y), 2)


    out <- dtf_x |>
      dplyr::left_join(dtf_y, by = {{ by_cols }}, relationship = "one-to-one") |>
      dplyr::mutate(across({{ patch_col }}, \(x) dplyr::coalesce(x, src_col))) |>
      dplyr::select(!"src_col") |>
      dplyr::select(all_of(names(dtf_x)))

    expect_equal(nrow(out), nrow(dtf_x))
    expect_equal(names(out), names(dtf_x))
  })

