myr_colours <- list(
  "cadetblue",
  "deepskyblue",
  "dodgerblue",
  "lightskyblue",
  "slateblue4",
  "hotpink1",
  "maroon3",
  "plum1",
  "tomato1",
  "violetred1",
  "darkcyan",
  "palegreen3",
  "seagreen4",
  "yellowgreen",
  "bisque",
  "coral",
  "darkorchid",
  "seashell",
  "slategrey",
  "thistle"
) |>
  rlang::set_names()

usethis::use_data(myr_colours, overwrite = TRUE)
