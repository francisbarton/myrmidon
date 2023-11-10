myr_colours <- list(
  blues = list(
    "cadetblue",
    "cyan4",
    "darkcyan",
    "deepskyblue",
    "dodgerblue",
    "lightskyblue",
    "slateblue"
  ),
  reds = list(
    "firebrick",
    "deeppink",
    "hotpink",
    "maroon2",
    "plum2",
    "violetred"
  ),
  greens = list(
    "seagreen",
    "springgreen3",
    "yellowgreen"
  ),
  misc = list(
    "antiquewhite",
    "bisque",
    "coral",
    "lavenderblush",
    "lightcoral",
    "papayawhip",
    "slategrey",
    "thistle"
  )
) |>
  purrr::map(rlang::set_names)
