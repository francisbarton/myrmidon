
unicode_url <- "https://unicode-table.com/en/emoji/animals-and-nature/"

url_data <- unicode_url |>
  xml2::read_html()

unicode_data  <- url_data |>
  rvest::html_elements("main .page-content .character-list ul.character-list__list") |>
  purrr::map(\(x) rvest::html_elements(x, "li.character-list__item a"))

emoji <- unicode_data |>
  purrr::map(\(x) rvest::html_attr(x, "data-symbol")) |>
  purrr::list_c()

emoji_names <- unicode_data |>
  purrr::map(\(x) rvest::html_attr(x, "title")) |>
  purrr::list_c()

project_mascots <- tibble::tibble(
  mascot_name = emoji_names,
  mascot_emoji = emoji
) |>
  dplyr::filter(
    if_any("mascot_name", \(x) stringr::str_detect(x,
    paste(
      "Bactrian",
      "Horse",
      "Dog",
      "Cat Face",
      "Poodle",
      "Monkey Face",
      "Pig Face",
      "Guide Dog",
      "Pig Nose",
      "Paw Prints",
      "Hatching Chick",
      "Baby Chick",
      "Feather",
      "Dragon Face",
      "Spiral Shell",
      "Spouting Whale",
      "^Fish",
      "Spider Web",
      "White Flower",
      "Rosette",
      "Blossom",
      "Bouquet",
      "Wilted Flower",
      "Ear of Rice",
      "Four Leaf Clover",
      "Fallen Leaf",
      "Leaf Fluttering In Wind",
      "Herb",
      "Shamrock",
      "Potted Plant",
    sep = "|"), negate = TRUE))) |>
    dplyr::mutate(across("mascot_name", \(x) dplyr::case_when(
      x == "Dromedary Camel" ~ "Camel",
      x == "Lady Beetle" ~ "Ladybird",
      x == "Honeybee" ~ "Honey Bee",
      x == "Dove of Peace" ~ "Dove",
      x == "Tropical Fish" ~ "Fish",
      x == "Deciduous Tree" ~ "Oak",
      stringr::str_detect(x, "Tree$") ~ stringr::str_remove(x, " Tree$"),
      stringr::str_detect(x, "Face$") ~ stringr::str_remove(x, " Face$"),
      TRUE ~ x
    )))
