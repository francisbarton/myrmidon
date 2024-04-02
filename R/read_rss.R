#' Read an RSS feed and return items data and feed metadata
#'
#' @param url URL of the RSS feed
#' @export
read_rss <- function(url) {
  feed <- xml2::read_xml(url)
  feed_attrs <- xml2::xml_attrs(feed)
  feed_content <- xml2::xml_child(feed, 1)

  # https://www.rssboard.org/rss-specification
  required_elements <- c(
    "title",
    "link",
    "description"
  )

  pull_elements <- function(element, feed) {
    elem <- feed |>
      xml2::xml_find_first(element)

    if (!is.na(elem)) xml2::xml_text(elem) else NA_character_
  }


  build_feed_metadata <- function(feed) {

    # https://www.rssboard.org/rss-specification
    optional_chr_elements <- c(
      "language",
      "copyright",
      "managingEditor",
      "webMaster",
      "category",
      "generator",
      "docs",
      "ttl",
      "rating"
    )

    optional_date_elements <- c(
      "pubDate",
      "lastBuildDate"
    )

    optional_chr <- optional_chr_elements |>
      purrr::map_chr(pull_elements, feed_content) |>
      purrr::set_names(optional_chr_elements) |>
      purrr::discard(is.na)

    optional_date <- optional_date_elements |>
      purrr::map_chr(pull_elements, feed_content) |>
      purrr::set_names(optional_date_elements) |>
      purrr::discard(is.na) |>
      purrr::map_dbl(lubridate::dmy_hms) |>
      lubridate::as_datetime()

    image_url <- xml2::xml_find_first(feed_content, "image") |>
      xml2::xml_child("url") |>
      xml2::xml_text() |>
      purrr::set_names("image_url") |>
      purrr::discard(is.na)

    reqd <- required_elements |>
      purrr::map_chr(pull_elements, feed_content) |>
      purrr::set_names(required_elements)

    out <- list(reqd[1], reqd[2], reqd[3], image_url, optional_chr, optional_date)
    out_names <- purrr::map_chr(out, names)
    out |>
      purrr::set_names(out_names)
  }

  build_item_data <- function(item) {
    item_required_elements <- required_elements |>
      purrr::map_chr(pull_elements, item) |>
      purrr::set_names(required_elements) |>
      tibble::as_tibble_row()

    item_optional_chr_elements <- c(
      "author",
      "category",
      "comments",
      "enclosure",
      "guid",
      "source"
    )

    item_optional_chr <- item_optional_chr_elements |>
      purrr::map_chr(pull_elements, item) |>
      purrr::set_names(item_optional_chr_elements) |>
      purrr::discard(is.na)

    item_optional_date <- "pubDate" |>
      purrr::map_chr(pull_elements, item) |>
      purrr::set_names("pubDate") |>
      purrr::discard(is.na) |>
      purrr::map_dbl(lubridate::dmy_hms) |>
      lubridate::as_datetime()

    item_required_elements |>
      dplyr::bind_cols(as.list(item_optional_chr)) |>
      dplyr::bind_cols(pub_date = item_optional_date)
  }

  feed_metadata <- build_feed_metadata(feed_content)
  items <- feed_content |>
    xml2::xml_find_all("item") |>
    purrr::map_df(build_item_data)

  list(items = items, feed_metadata = feed_metadata)
}
