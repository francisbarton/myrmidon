#' Use postcodes.io To Get Postcode Metadata
#'
#' @param postcodes one or more valid GB postcodes
#'
#' @return Tibble of postcode data
#'
#' @importFrom httr POST warn_for_status content
#' @importFrom purrr pluck map map_dfr
#' @importFrom dplyr rename_with bind_cols
#' @importFrom magrittr extract
#'
#' @export
#'
#' @examples
#' get_postcode_data("W1A 1AA")
#'
get_postcode_data <- function(postcodes) {
  endpoint <- "https://api.postcodes.io/postcodes"
  body <- list(postcodes = postcodes)

  out <- httr::POST(url = endpoint, body = body, encode = "json")
  httr::warn_for_status(out)

  results <- httr::content(out) %>%
    purrr::pluck("result") %>%
    purrr::map("result")

  codes <- results %>%
    purrr::map_dfr("codes") %>%
    dplyr::rename_with(~ paste0(., "_code"))

  info <- results %>%
    purrr::map_dfr(magrittr::extract, 1:23)

  dplyr::bind_cols(info, codes)
}
