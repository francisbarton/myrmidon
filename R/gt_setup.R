gt_setup <- function(data) {
  data %>%
    gt::gt() %>%
    gt::tab_options(
      table.width = pct(80),
      table.font.names = "Source Sans Pro",
      table.font.size = "10px",
      heading.align = "left",
      heading.background.color = "#164194",
      heading.title.font.weight = "bold",
      heading.title.font.size = "14px",
      heading.subtitle.font.size = "12px",
      column_labels.font.size = "11px",
      source_notes.font.size = "10px",
      row.striping.background_color = "#faf0fa"
    ) %>%
    gt::opt_row_striping() %>%
    gt::tab_style(
      style = list(
        cell_fill(color = "#164194"),
        cell_text(weight = "bold", color = "white", align = "left"),
        cell_borders(sides = "all", color = "#164194")
      ),
      locations = cells_title()
    ) %>%
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    gt::tab_style(
      style = cell_text(align = "right", v_align = "middle"),
      locations = list(
        cells_column_labels(),
        cells_body()
      )) %>%
    gt::tab_style(
      style = cell_text(weight = "bold", align = "left"),
      locations = list(
        cells_column_labels(columns = 1),
        cells_body(columns = 1)
      )) %>%
    gt::tab_style(
      style = cell_text(align = "right"),
      locations = cells_source_notes()
    )
}
