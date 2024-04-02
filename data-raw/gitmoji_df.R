gitmoji_df <- paste0(
  "https://github.com/carloscuesta/gitmoji/",
  "raw/master/packages/gitmojis/src/gitmojis.json"
 ) |>
  jsonlite::fromJSON() |>
  purrr::pluck("gitmojis") |>
  tibble::as_tibble()

source(here::here("data-raw/project_mascots.R"))

usethis::use_data(
  gitmoji_df,
  project_mascots,
  mascot_names,
  star_names,
  overwrite = TRUE,
  internal = TRUE
)
