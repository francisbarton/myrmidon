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
  missing_emojis <- c("Beaver", "Beetle", "Bison", "Dodo",
                      "Fly", "Mammoth", "Seal", "Worm")

  mascot_row <- project_mascots |>
    dplyr::filter(!if_any("mascot_label", \(x) vec_in(x, missing_emojis))) |>
    dplyr::slice_sample(n = 1L)
  mascot_names <- c(mascot_names, star_names)

  if (alliterative) {
    mascot_names <- mascot_names |>
      grep(pattern = substr(mascot_row[["mascot_label"]], 1L, 1L), value = TRUE)
  }

  # glue::glue("{mascot_name} the {mascot_label} {mascot_emoji}")
  list(
    mascot_name = sample(mascot_names, 1L),
    mascot_label = mascot_row[["mascot_label"]],
    mascot_emoji = mascot_row[["mascot_emoji"]]
  )
}
