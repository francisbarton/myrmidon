
unicode_url <- "https://symbl.cc/en/emoji/animals-and-nature/"

url_data <- unicode_url |>
  xml2::read_html()

unicode_data  <- url_data |>
  rvest::html_elements("main .blocks-item .character-list ul.character-list__list") |>
  purrr::map(\(x) rvest::html_elements(x, "li.character-list__item a")) |>
  purrr::map(\(x) tail(x, -1))

emoji <- unicode_data |>
  purrr::map(\(x) rvest::html_attr(x, "data-symbol")) |>
  purrr::list_c()

emoji_labels <- unicode_data |>
  purrr::map(\(x) rvest::html_attr(x, "data-label")) |>
  purrr::list_c()

# Not included in the animals and nature list but still cute
extra_mascots <- tibble::tribble(
  ~ mascot_label, ~ mascot_emoji,
  "Crab", "\U1f980",
  "Shrimp", "\U1f990",
  "Squid", "\U1f991",
  "Lobster", "\U1f99e",
  "Ghost", "\U1f47b",
  "Alien", "\U1f47d",
  "Monster", "\U1f47e",
  "Robot", "\U1f916"
)

project_mascots <- tibble::tibble(
  mascot_label = emoji_labels,
  mascot_emoji = emoji
  ) |>
  dplyr::filter(
    if_any("mascot_label", \(x) stringr::str_detect(x,
    paste(
      "Mouse",
      "Cow",
      "Tiger",
      "Rabbit",
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
      "Spider Web",
      "Cockroach",
      "White Flower",
      "Blossom",
      "Rosette",
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
  dplyr::mutate(across("mascot_label", \(x) dplyr::case_match(x,
      "Bird" ~ "Nightjar", # We needed an N!
      "Dromedary Camel" ~ "Camel",
      "Rooster" ~ "Cockerel",
      "Lady Beetle" ~ "Ladybird", # This is a British function!
      "Honeybee" ~ "Honey Bee",
      "Dove of Peace" ~ "Dove",
      "Deciduous Tree" ~ "Oak",
      "Evergreen Tree" ~ "Evergreen",
      "Palm Tree" ~ "Palm",
      .default = x))) |>
  dplyr::mutate(across("mascot_label", \(x) stringr::str_remove(x, " Face$"))) |>
  dplyr::bind_rows(extra_mascots) |>
  dplyr::arrange(dplyr::across("mascot_label"))



mascot_names <- "1fc6thrwFTPKwP0PlBTvzKjfiGlnM37JXLPaR8_In3fw" |>
  googlesheets4::read_sheet(2, "C:C") |>
  dplyr::pull("First") |>
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
  rvest::html_element("#mw-content-text table") |>
  rvest::html_table() |>
  dplyr::pull("Modern proper name") |>
  stringr::str_extract("^[:alpha:]+") |>
  unique() |>
  stringr::str_to_sentence() |>
  purrr::discard(
    \(x) x %in% c(
      "Aldebaran", "Absolutno", "Betelgeuse", "Felixvarela", "Intercrus", "La", "Lionrock", "Phact", "Pollux"
      )) |>
  purrr::discard(\(x) nchar(x) > 9)

