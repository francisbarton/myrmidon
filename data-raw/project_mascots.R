
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



mascot_names1 <- "1fc6thrwFTPKwP0PlBTvzKjfiGlnM37JXLPaR8_In3fw" |>
  googlesheets4::read_sheet(2, "C:C") |>
  dplyr::pull(First) |>
  unique() |>
  stringr::str_to_sentence() |>
  purrr::discard(
    \(x) x %in% c(
      "Alfre", "America", "Bigfreedia", "Blind", "Contrapoints", "Dj", "Dr", "Drake", "E", "Eazye", "Ego", "Ice", "J", "Kj", "Jesus", "Kasturba",
      "Littlemix", "Ma", "Mc", "Mf", "Missmajor", "Mj", "M", "Ma", "Mf",
      "Nate diana", "Nghi", "Nk", "P", "Pc", "Prince", "Pseudonym", "Rj",
      "Scholastique", "Tanehisi", "Tnia", "Vv", "Yaa", "Yoshitsune", "Zapmama"
    ))


star_names <- "https://en.wikipedia.org/wiki/List_of_proper_names_of_stars" |>
  rvest::read_html() |>
  rvest::html_elements("table") |>
  dplyr::nth(1) |>
  rvest::html_table() |>
  dplyr::pull(3) |>
  stringr::str_extract("^[:alpha:]+") |>
  unique() |>
  stringr::str_to_sentence() |>
  purrr::discard(
    \(x) x %in% c(
      "Aldebaran", "Absolutno", "Betelgeuse", "Felixvarela", "Intercrus", "La", "Lionrock", "Phact", "Pollux"
      )) |>
  purrr::discard(
    \(x) nchar(x) > 9
  )

