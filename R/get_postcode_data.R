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
  retrieve_postcode_metadata <- function(codes) {


    # replace NULL values with NA = thanks to
    # https://stackoverflow.com/a/49539022/5168907
    replace_x <- function(x, replacement = NA_character_) {
      if (length(x) == 0 || length(x[[1]]) == 0 || is.null(x)) {
        replacement
      } else {
        x
      }
    }



    make_numeric <- function(lst) {
      lst %>%
        purrr::modify_at("eastings", ~ as.numeric(.)) %>%
        purrr::modify_at("northings", ~ as.numeric(.)) %>%
        purrr::modify_at("longitude", ~ as.numeric(.)) %>%
        purrr::modify_at("latitude", ~ as.numeric(.))
    }



    endpoint <- "https://api.postcodes.io/postcodes"


    out <- list(postcodes = codes) %>%
      httr::POST(
        url = endpoint
      , body = .
      , encode = "json"
      # , config = httr::add_headers()
      # , httr::user_agent("Fran Barton github.com/francisbarton/myrmidon")
      # , httr::content_type_json()
      )

    httr::warn_for_status(out)



    results <- httr::content(out) %>%
      purrr::pluck("result") %>%
      purrr::modify_depth(3, replace_x) %>%
      purrr::map("result") %>%
      purrr::map(make_numeric)


    info <- results %>%
      purrr::map_dfr(`[`, 1:23)
    codes <- results %>%
      purrr::map_dfr("codes") %>%
      dplyr::rename_with(~ paste0(., "_code"))


    dplyr::bind_cols(info, codes)
  }







  return_style <- match.arg(return_style, several.ok = FALSE)

  if (is.null(return_style)) return_style <- "moderate"


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





  postcodes <- postcodes[!is.na(postcodes)] %>%
    unique()

  fixed_postcodes <- postcodes %>%
    validate_postcode_list()

  if (!setequal(fixed_postcodes, postcodes)) {

    usethis::ui_info(
      paste0(
        "Certain postcodes were found to be invalid ",
        "and have been replaced with their nearest available ",
        "valid postcode.\n",
        "original codes: ",
        stringr::str_c(
          setdiff(fixed_postcodes, postcodes)
          , collapse = ", "
          ),
        "\n",
        "replacement codes: ",
        stringr::str_c(
          setdiff(postcodes, fixed_postcodes)
          , collapse = ", "
          )
      )
    )
  }




  fixed_postcodes %>%
    myrmidon::batch_it(batches = 50, quiet = TRUE) %>%
    purrr::map_df(retrieve_postcode_metadata) %>%
    dplyr::select(!!cols_to_keep)


}
