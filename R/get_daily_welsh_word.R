#' Gets the latest Welsh word of the day from the GPC (Geiriadur Prifysgol Cymru) Mastodon RSS feed
#' @export
get_daily_welsh_word <- function() {

  feed_items <- myrmidon::read_rss("https://toot.wales/@geiriadurGPC.rss") |>
    purrr::pluck("items")

  feed_items |>
    dplyr::slice(1) |>
    dplyr::pull(description) |>
    rvest::read_html() |>
    rvest::html_elements("p") |>
    purrr::pluck(2) |>
    rvest::html_text() |>
    stringr::str_squish() |>
    stringr::str_replace("^Word of the Day", "Gair y Dydd") |>
    stringr::str_extract(".+(?=: https://geiriadur)")
}
