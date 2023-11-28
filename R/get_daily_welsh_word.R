#' Gets the latest Welsh word of the day from the GPC (Geiriadur Prifysgol Cymru) Mastodon RSS feed
#' @export
get_daily_welsh_word <- function() {

  feed_items <- read_rss("https://toot.wales/@geiriadurGPC.rss") |>
    purrr::pluck("items")

  item_text <- feed_items |>
    dplyr::pull("description") |>
    purrr::map(rvest::read_html) |>
    purrr::map(\(x) rvest::html_elements(x, "p")) |>
    purrr::map(2) |>
    purrr::compact() |>
    purrr::map(rvest::html_text) |>
    purrr::keep(\(x) stringr::str_starts(x, "Word of the Day")) |>
    dplyr::first() |>
    stringr::str_replace("^Word of the Day", "Gair y Dydd")

  item_def <- stringr::str_extract(item_text, "^.*(?=:\\s+https)")
  item_url <- stringr::str_extract(item_text, "https[:graph:]+(?=\\.)")
  item_des <- stringr::str_remove(item_text, "^.*(?=:\\s+https)") |>
    stringr::str_remove("^:\\s+") |>
    stringr::str_remove("https[:graph:]*") |>
    stringr::str_squish() |>
    stringr::str_remove("^[:punct:]+") |>
    stringr::str_squish()

  list(item_def = item_def, item_url = item_url, item_des = item_des)
}
