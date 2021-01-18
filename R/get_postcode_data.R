#' Use postcodes.io To Get Postcode Metadata
#'
#' @param postcodes one or more valid GB postcodes
#' @param return_style minimal just returns LSOA data and UK Grid eastings & northings. moderate returns a fuller selection of columns, arranged and renamed. full returns all columns.
#' @return Tibble of postcode data
#'
#' @export
#' @examples
#' get_postcode_data(c("SW1A 0AA", "E15 1TT"))
#'
get_postcode_data <- function(postcodes, return_style = c("moderate", "minimal", "full")) {

  # calls to postcodes.io API and returns rows of data
  retrieve_postcode_metadata <- function(postcodes) {
    endpoint <- "https://api.postcodes.io/postcodes"

    # session <- polite::bow(endpoint, force = TRUE)

    # replace NULL values with NA = thanks to
    # https://stackoverflow.com/a/49539022/5168907
    replace_x <- function(x, replacement = NA_character_) {
      if (length(x) == 0 || length(x[[1]]) == 0) {
        replacement
      } else {
        x
      }
    }

    out <- httr::POST(
      url = endpoint
      , body = list(postcodes = unique(unlist(postcodes)))
      , encode = "json"
      # , config = httr::add_headers()
      , httr::user_agent("Fran Barton github.com/francisbarton/myrmidon")
      , httr::content_type_json()
      )
    httr::warn_for_status(out)

    result <- httr::content(out) %>%
      purrr::pluck("result") %>%
      purrr::map("result") %>%
      purrr::modify_depth(2, replace_x)

    info <- result %>%
      purrr::map_dfr(`[`, 1:23)
    codes <- result %>%
      purrr::map_dfr("codes") %>%
      dplyr::rename_with(~ paste0(., "_code"))

    dplyr::bind_cols(info, codes)
  }

  return_style <- match.arg(return_style, several.ok = FALSE)

  # tidy response and return
  if (return_style == "minimal") {
    cols_to_keep <- rlang::expr(dplyr::all_of(
      c(
        1
        , lsoa11cd = 33
        , lsoa11nm = 12
        , 3
        , 4
        )
      ))
  } else if (return_style == "moderate") {
    cols_to_keep <- rlang::expr(dplyr::all_of(
      c(
        1
        , lsoa11cd = 33
        , lsoa11nm = 12
        , msoa11cd = 34
        , msoa11nm = 13
        , wd20cd = 26
        , wd20nm = 20
        , lad20cd = 24
        , lad20nm = 17
        , cty20cd = 25
        , cty20nm = 19
        , rgn20nm = 11
        , 3
        , 4
        , 7
        , 8
        )
      ))
  } else if (return_style == "full") {
    cols_to_keep <- rlang::expr(dplyr::everything())
  }

  postcodes %>%
    myrmidon::batch_it(batches = 100, quiet = TRUE) %>%
    purrr::map_df(retrieve_postcode_metadata) %>%
    # purrr::reduce(dplyr::bind_rows) %>%
    dplyr::select(!!cols_to_keep)
}
