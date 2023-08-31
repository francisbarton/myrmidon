#' @export
project_mascot <- function(alliterative = FALSE) {

  # Emoji currently missing on Windows
  missing_emojis <- c("Beaver", "Beetle", "Bison", "Dodo",
                      "Fly", "Mammoth", "Seal", "Worm")

  mascot_row <- project_mascots |>
    dplyr::filter(if_any("mascot_label", \(x) !x %in% missing_emojis)) |>
    dplyr::slice_sample(n = 1)
  mascot_names <- c(mascot_names, star_names)

  if (alliterative) {
    mascot_names <- mascot_names |>
      grep(pattern = substr(mascot_row[["mascot_label"]], 1, 1), value = TRUE)
  }

  # glue::glue("{mascot_name} the {mascot_label} {mascot_emoji}")
  list(
    mascot_name = sample(mascot_names, 1),
    mascot_label = mascot_row[["mascot_label"]],
    mascot_emoji = mascot_row[["mascot_emoji"]]
  )
}
