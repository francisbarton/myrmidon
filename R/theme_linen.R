#' My preferred ggplot2 theme, based on a "linen" coloured background
#' 
#' @param font_family Specify a font family to use. "Fira Sans" by default.
#' @export
theme_linen <- function(font_family = "Fira Sans") {
  ggplot2::theme(
    text = element_text(family = font_family, size = 14, colour = "grey24"),
    title = element_text(hjust = 0.05, face = "bold"),
    plot.title = element_text(
      size = 18, margin = margin(8, 8, 12, 18)
      # hjust = 0.05, margin = margin(8, 8, 8, 24)
    ),
    plot.subtitle = element_text(face = "plain", margin = margin(2, 0, 2, 0)),
    plot.caption = element_text(
      hjust = 0.96, face = "plain", size = 12, margin = margin(12, 8, 6, 0)
    ),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "linen", colour = NA),
    plot.margin = margin(12), # margin(8, 12, 8, 12),
    panel.background = element_rect(fill = "linen"),
    panel.spacing = grid::unit(48, "pt"),
    legend.title = element_text(hjust = 0.05),
    legend.background = element_rect(fill = "linen"),
    legend.key = element_rect(fill = "linen", colour = NA),
    legend.key = element_blank(),
    # legend.box.background = element_rect(fill = "linen"),
    panel.grid.major = element_line(
      colour = "#bfbfbf7f", # 50% opacity
      linewidth = 0.8,
      lineend = "square"
    ),
    panel.grid.minor = element_line(
      colour = "thistle",
      linewidth = 0.3,
      lineend = "butt"
    ),
    axis.title.x = element_text(margin = margin(t = 8, l = 0), hjust = 0), 
    axis.title.y = element_text(margin = margin(r = 4, l = 12)),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_line(
      colour = "#bfbfbf7f",
      linewidth = 0.8,
      lineend = c("round", "square")
    ),
    axis.ticks.x.bottom = element_line(lineend = c("round", "butt")),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y.left = element_line(lineend = c("round", "butt")),
    axis.ticks.y.right = element_blank(),
    strip.background = element_rect(fill = "slateblue4", colour = NA),
    strip.text = element_text(colour = "grey95", size = 14),
    validate = TRUE
  )
}


#' A ggplot2 theme specifically for maps, with a dark ("ebony clay") background
#' 
#' Starts with theme_void() and removes all gridlines and axis markings and
#'  labels. Legend moves inside the panel area and is transparent.
#' @inheritParams theme_linen
#' @export
theme_clay <- function(font_family = "Fira Sans") {
  ggplot2::theme_void() +
  ggplot2::theme(
    text = element_text(family = font_family, size = 14, colour = "#bdc9ce"),
    title = element_text(hjust = 0.05, face = "bold"),
    plot.title = element_text(size = 18, margin = margin(8, 8, 12, 18)),
    plot.subtitle = element_text(face = "plain", margin = margin(2, 0, 2, 0)),
    plot.caption = element_text(
      hjust = 0.96, face = "plain", size = 12, margin = margin(12, 8, 6, 0)
    ),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#26283b", colour = NA),
    plot.margin = margin(12),
    panel.spacing = grid::unit(48, "pt"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(hjust = 0.9),
    legend.justification = c(0, 1),
    legend.title.position = "left",
    legend.position.inside = TRUE,
    legend.position = c(0.72, 0.2),
    legend.key = element_blank(),
    legend.background = element_blank(),
    validate = TRUE
  )
}