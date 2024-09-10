#' Get a cute emoji mascot for your latest project
#' 
#' ... or even just for your current R session
#' 
#' @param alliterative Boolean. `TRUE` will try to return a name that starts
#'  with the same letter as the (randomly) chosen emoji. `FALSE` by default.
#' @examples 
#' project_mascot() |>
#'   glue::glue_data("{mascot_name} the {mascot_label} {mascot_emoji}")
#' 
#' @export
project_mascot <- function(alliterative = FALSE) {

  # Emoji currently missing on Windows
  missing_emojis <- c(
    "Beaver", "Beetle", "Bison", "Dodo", "Fly", "Mammoth", "Seal", "Worm"
  )

  mascot_row <- project_mascots |>
    dplyr::filter(!if_any("mascot_label", \(x) vec_in(x, missing_emojis))) |>
    dplyr::slice_sample(n = 1L)
  mascot_names <- c(mascot_names, star_names)

  if (alliterative) {
    mascot_names <- mascot_names |>
      stringr::str_subset(paste0("^", chars(mascot_row[["mascot_label"]])[[1]]))
  }

  list(
    mascot_name = sample(mascot_names, 1L),
    mascot_label = mascot_row[["mascot_label"]],
    mascot_emoji = mascot_row[["mascot_emoji"]]
  )
}

#' Alias for stringr::str_split_1(..., pattern = "")
#' Split a string into its characters
#' @param x A single character string
# '@export
chars <- function(x) {
  assert_that(length(x) == 1, is.character(x))
  stringr::str_split_1(x, "")
}