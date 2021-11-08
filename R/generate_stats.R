function(area, measure, source_pcts, invert = TRUE, age_bands = "lloyds_all", summary = TRUE) {

  slice_pops <- function(area, low, high) {

    ons_age_profiles %>%
      dplyr::filter(name == area) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        total = sum(c_across(num_range("age_", low:high)))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(total)

  }

  wa <- ""

  if (invert) {
    source_pcts <- `-`(100, source_pcts)
    w <- "no"
  }

  if (age_bands == "lloyds_all") {
    assert_that(length(source_pcts) == 7)
    lows <- 1:7 %>%
      `*`(10) %>%
      `+`(5)
    highs <- rep(9, 6) %>%
      c(., 15) %>%
      purrr::map2_dbl(lows, `+`)
  } else if (age_bands == "lloyds_wa") {
    assert_that(length(source_pcts) == 5)
    lows <- 1:5 %>%
      `*`(10) %>%
      `+`(5)
    highs <- rep(9, 5) %>%
      purrr::map2_dbl(lows, `+`)
    wa <- "Working Age "
  } else if (age_bands == "lloyds_65") {
    assert_that(length(source_pcts) == 6)
    lows <- 1:6 %>%
      `*`(10) %>%
      `+`(5)
    highs <- rep(9, 5) %>%
      c(., 25) %>%
      purrr::map2_dbl(lows, `+`)
  } else if (age_bands == "ons") {
    assert_that(length(source_pcts) == 7)
    lows <- 2:7 %>%
      `*`(10) %>%
      `+`(5) %>%
      c(16, .)
    highs <- rep(9, 5) %>%
      c(8, ., 15) %>%
      purrr::map2_dbl(lows, `+`)
  } else if (age_bands == "ofcom") {
    assert_that(length(source_pcts) == 7)
    lows <- 2:7 %>%
      `*`(10) %>%
      `+`(5) %>%
      c(16, .)
    highs <- rep(9, 5) %>%
      c(8, ., 15) %>%
      purrr::map2_dbl(lows, `+`)
  }
  assert_that(length(lows) == length(highs))
  assert_that(length(lows) == length(source_pcts))




  pops_out <- purrr::map2_dbl(lows, highs, ~ slice_pops(area, .x, .y))
  assert_that(length(pops_out) == length(source_pcts))

  measure_pct <- paste0("pct_", w, "_", snakecase::to_snake_case(measure))
  measure_pop <- paste0("pop_", w, "_", snakecase::to_snake_case(measure))

  df <- purrr::map2_chr(lows, highs, ~ paste0(.x, "-", .y)) %>%
    dplyr::tibble(area = area, age_band = .) %>%
    dplyr::bind_cols({{ measure_pct }} := source_pcts) %>%
    dplyr::bind_cols(age_band_popn = pops_out) %>%
    dplyr::rowwise() %>%
    dplyr::mutate({{ measure_pop }} := round(
      prod(
        across({{measure_pct}}), age_band_popn, 0.01
      )
    )) %>%
    dplyr::ungroup()

  # return
  if (summary) {
    df %>%
      dplyr::group_by(area) %>%
      dplyr::summarise(
        "Total {wa}Population" := sum(age_band_popn),
        "{wa}Population {w} {measure}" := sum(across({{ measure_pop }})),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        "% {wa}Population {w} {measure}" := round(sum(across(3))*100/sum(across(2)), 1)
      )
  } else df
}
