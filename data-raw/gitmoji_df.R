## code to prepare `gitmoji` dataset goes here

gitmoji_df <- jsonlite::fromJSON("https://github.com/carloscuesta/gitmoji/raw/master/src/data/gitmojis.json")[[1]]

usethis::use_data(gitmoji_df, overwrite = TRUE, internal = TRUE)
