#' Gets the latest Welsh word of the day from the GPC (Geiriadur Prifysgol
#'  Cymru) Mastodon RSS feed
#' @export
get_daily_welsh_word <- function() {

  feed_items <- read_rss("https://toot.wales/@geiriadurGPC.rss") |>
    purrr::pluck("items")

  item_text <- feed_items |>
    dplyr::pull("description") |>
    purrr::map(rvest::read_html) |>
    purrr::map(\(x) rvest::html_elements(x, "p")) |>
    # purrr::map(1) |>
    purrr::map(\(x) purrr::map(x, rvest::html_text)) |>
    purrr::flatten() |>
    purrr::keep(\(x) stringr::str_starts(x, "Word of the Day")) |>
    dplyr::first() |>
    stringr::str_remove("^Word of the Day: ")

  item_def <- stringr::str_extract(item_text, "^.+(?=\\s+https)")
  item_url <- stringr::str_extract(item_text, "https[:graph:]*(?=\\s|\\.)")
  item_des <- stringr::str_remove(item_text, "^[^(https)]+") |>
    stringr::str_remove("https[[:graph:]]*\\s?") |>
    stringr::str_extract("^[^\\.]+")

  list(item_def = item_def, item_url = item_url, item_des = item_des)
}
