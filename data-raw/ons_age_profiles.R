# code to prepare `ons_age_profiles` dataset goes here
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland

ons_age_profiles <- readr::read_csv(here::here("data-raw", "ONS2020_PopnByAgeYearEstimate.csv"), lazy = FALSE, ) %>%
  dplyr::rename_with(.cols = 5:last_col(), .fn = ~ paste0("age_", .)) %>%
  janitor::clean_names() %>%
  dplyr::mutate(across(c(name, geography),
                       ~ dplyr::case_when(
                         stringr::str_detect(., "[a-z]", negate = TRUE) ~ stringr::str_to_title(.),
                         TRUE ~ .)
  ))

usethis::use_data(ons_age_profiles, overwrite = TRUE)
