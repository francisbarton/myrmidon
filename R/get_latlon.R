#' Use doogal.co.uk API To Get Postcode Data
#'
#' Deprecated - use \code{get_postcode_data()} instead
#' Doesn't accept a vector of codes all at once, so use with purrr::map_dfr()
#' @param postcode a UK postcode
get_latlon <- function(postcode) {

  data_names <- c(
    "postcode",
    "lat",
    "lon",
    "quality",
    "constituency",
    "district",
    "ward",
    "lsoa11nm",
    "county",
    "region",
    "country",
    "national_park",
    "altitude",
    "rural_urban"
    )

  endpoint <- "https://www.doogal.co.uk/GetPostcode.ashx?postcode="

  paste0(endpoint, postcode) %>%
    utils::URLencode() %>%
    rvest::read_html() %>%
    rvest::html_node("p") %>%
    rvest::html_text() %>%
    readr::read_delim("\t", col_names = data_names)
}
