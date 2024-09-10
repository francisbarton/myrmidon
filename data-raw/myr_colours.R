#' @export
myr_colours <- c(
  "slateblue4",
  "dodgerblue",
  "deepskyblue",
  "lightskyblue",
  "cadetblue",
  "darkcyan",
  "seagreen4",
  "palegreen3",
  "yellowgreen",
  "thistle",
  "bisque",
  "seashell",
  "linen",
  "coral",
  "tomato1",
  "violetred1",
  "hotpink1",
  "maroon3",
  "darkorchid",
  "plum1",
  "slategrey"
) |>
  rlang::set_names() |>
  col2hex()

usethis::use_data(myr_colours)
