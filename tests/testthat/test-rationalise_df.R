"column unique lengths" |>
  test_that({
    l <- dplyr::starwars |>
      dplyr::summarise(across(everything(), \(x) length(unique(x)))) |>
      unlist() |>
      sort(TRUE)
    
    expect_length(l, ncol(dplyr::starwars))
    expect_true(all(names(l) %in% colnames(dplyr::starwars)))

    df_out <- dplyr::starwars |>
      dplyr::select(all_of(names(l)))

    expect_identical(dplyr::starwars[["name"]], df_out[["name"]])
  })


"test overall function" |>
  test_that({
    # randomly resort the columns
    df1 <- dplyr::starwars |>
      dplyr::select(sample(seq(ncol(dplyr::starwars))))

    df2 <- rationalise_df(df1)
    df3 <- rationalise_df(dplyr::starwars)

    expect_identical(colnames(df2), colnames(df3))
  })
