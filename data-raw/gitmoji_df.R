gitmoji_df <- jsonlite::fromJSON("https://github.com/carloscuesta/gitmoji/raw/master/packages/gitmojis/src/gitmojis.json")[[1]]

source(here::here("data-raw/project_mascots.R"))

usethis::use_data(gitmoji_df, project_mascots, mascot_names, star_names, overwrite = TRUE, internal = TRUE)
